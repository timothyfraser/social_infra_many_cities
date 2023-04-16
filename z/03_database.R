#' @name generate.R
#'
#' @description A script to generate supplemental or formatted information
#' based on our previous API calls.
#'


# 3. Overall


# 2. Get Bounds  ################################################

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)
library(sf)

library(RSQLite)
library(DBI)

data("meta") # get city name metadata
data("wgs") # Get EPSG:4326 (WGS 84) projection (https://spatialreference.org/ref/epsg/wgs-84/)
data("aea") # Get equal area conic projection


# Let's initialize the geospatial database, using a blank table called 'starter'
tibble(geometry = "POINT(42.4534 -76.4735)") %>%
  st_as_sf(wkt = "geometry", crs = 4326) %>%
  st_write(obj = ., dsn = Sys.getenv("DATABASE"), layer = "starter", delete_dsn = TRUE, delete_layer = TRUE)



# Write a combine function
combine = function(name, type, ext = ".geojson", area = FALSE){
  print(name);
  # Get the shapes
  shapes = paste0("search/", name, "/", type, ext) %>%
    read_sf() %>% mutate(name = name)
  # IF area == TRUE,
  if(area == TRUE){
    data("aea")
    # Calculate the area, in square kilometers
    shapes = shapes %>%
      st_transform(crs = aea) %>%
      mutate(area = as.numeric(st_area(geometry)) / 1000000) %>%
      st_transform(crs = 4326)
  }
  return(shapes)
}


# Now connect to it using SQLite
# Now, let's add a bunch of data
data("connect") # Get connect() function
data("wkb_as_sf") # Get function to convert from wkb to sf format
# Establish connection
geo = connect()

# Get all bounds and save to table (overwrite the table)
meta$cities %>%
  map(~combine(name = ., type = "bounds", ext = ".geojson", area = TRUE)) %>%
  dplyr::bind_rows() %>%
  st_write(obj = ., dsn = geo, layer = "bounds", delete_layer = TRUE, format = "WKB")

# Test view it
# test = geo %>% tbl("bounds") %>% head(1) %>% collect()
# wkb_as_sf(test)$geometry %>% plot()
# remove(test)

# Get all block ground boundaries and save to file  (overwrite table)
meta$cities %>%
  map(~combine(name = ., type = "bg", ext = ".geojson", area = TRUE)) %>%
  dplyr::bind_rows() %>%
  st_write(obj = ., dsn = geo, layer = "bg", delete_layer = TRUE, format = "WKB")

# Get all grids and save to file (overwrite table)
meta$cities %>%
  # Skip area calculation - they're all 1 square kilometer!
  map(~combine(name = ., type = "grid1km", ext = ".geojson", area = FALSE)) %>%
  bind_rows() %>%
  st_write(obj = ., dsn = geo, layer = "grid1km", delete_layer = TRUE, format = "WKB")

# Get all tracts and save to file (overwrite table)
meta$cities %>%
  map(~combine(name = ., type = "tracts", ext = ".geojson", area = TRUE)) %>%
  bind_rows() %>%
  st_write(obj = ., dsn = geo, layer = "tracts", delete_layer = TRUE, format = "WKB")

# Write a function to import a specific city's data and format it
filter_sites = function(name){
    # Get shapes for that city
    shapes = paste0("search/", i, "/bounds.geojson") %>% read_sf() %>% st_transform(crs = aea) %>%
      mutate(area = as.numeric(st_area(geometry)) / 1000000)

    # Get points for that city
    points = paste0("search/", i, "/results.csv") %>% read_csv() %>% select(place_id:term) %>%
      # Get distinct searches, by grabbing just the first result of any multiples that appear.
      group_by(place_id) %>%
      summarize(across(.cols = everything(), .fns = ~.x[1])) %>%
      ungroup() %>%
      # Make into an sf object
      st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
      st_transform(crs = aea)
    # Overwrite, by narrowing into just points in our cities
    points = points %>%
      # Filter to just sites within city
      st_join(y = shapes %>% select(geometry), left = FALSE) %>%
      # Turn back to simple formatting
      st_transform(crs = 4326)
    return(points)
  }

meta$cities %>%
  map(~filter_sites(name = .)) %>%
  bind_rows() %>%
  st_write(obj = ., dsn = geo, layer = "sites", delete_layer = TRUE, format = "WKB")



geo = connect()

geo %>% dbListTables()


dbDisconnect(geo)



```{r, echo = FALSE}
censuskey <- "5e33463051bc6a5e05db5a76784747d680a65df4"
```

```{r, eval = FALSE}
censuskey <- "YOUR_CENSUS_API_KEY_HERE"
# Register for one here! It's free and takes less than 1 minute.
# https://api.census.gov/data /key_signup.html
```

### Get List of Relevant Tracts

Let's get a list of all census tracts of interest. This means, all census tracts located within or overlapping with our city boundaries.


library(tidyverse)
library(sf)

dir.create("census2020")

# Get EPSG:4326 (WGS 84) projection
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#
# Simpler version
read_sf("all/tracts.geojson") %>%
  as_tibble() %>%
  select(geoid, name) %>%
  write_csv("census2020/sample_tracts.csv")


### Get Census Block Data

```{r}
library(censusapi)

myvars <- list("GEO_ID" = "geoid",
               "P1_001N" = "pop",
               "P1_003N" = "white",
               "P1_004N" = "black",
               "P1_005N" = "natam",
               "P1_006N" = "asian",
               "P2_002N" = "hisplat",
               "H1_001N" = "units",
               "H1_002N" = "units_occupied") %>%
  as_tibble() %>%
  pivot_longer(cols = -c(), names_to = "id", values_to = "variable")


library(future)
library(furrr)

# Set up parallel processing

# Check current worksers
nbrOfWorkers()
# check available workers
numworkers <- availableWorkers() %>% length()
# Initiate parallel processing
# Always use at least 1 fewer than the total available workers
plan(multisession, workers = numworkers - 1)

myunits <- read_csv("census2020/sample_tracts.csv") %>%
  mutate(state = str_sub(geoid, 1,2),
         county = str_sub(geoid, 3,5),
         tract = str_sub(geoid, 6,-1)) %>%
  mutate(geoid = str_sub(geoid, 1,5)) %>%
  select(geoid, state, county) %>%
  distinct()

# For reference!
# https://api.census.gov/data/2020/dec/pl/examples.html
# https://api.census.gov/data/2020/dec/pl/variables.html

myunits %>%
  split(.$geoid) %>%
  future_map_dfr(
    ~censusapi::getCensus(
      key = censuskey,
      vintage = "2020",
      name = "dec/pl",
      region = "block:*",
      regionin = paste(
        "state:", .$state, # Choose just blocks in this state
        "&", "county:", .$county,  # in this county
#        "&", "tract:", .$tract, # in this tract
        sep = ""),
      # Grab these variables
      vars = myvars$id),
    .progress = TRUE) %>%
  # Keep just the rows with our specified names
    select(myvars$id) %>%
  # Rename them to simpler, more easily understandable names
  magrittr::set_colnames(value = c(myvars$variable)) %>%
  # better format the geoid
  mutate(geoid = str_remove(geoid, "1000000US"),
         tract = str_sub(geoid, 1, 11)) %>%
  # Filter to just blocks in our tracts of interest
  filter(tract %in% read_csv("census2020/sample_tracts.csv")$geoid) %>%
  select(-tract) %>%
  # Convert subcategories into percentages
  mutate(white = white / pop,
         black = black / pop,
         asian = asian / pop,
         natam = asian / pop,
         hisplat = hisplat / pop,
         units_occupied = units_occupied / units) %>%
  # Reset any nans to NA
  mutate_at(vars(white:units_occupied),
            list(~if_else(is.nan(.) | is.infinite(.), NA_real_, as.numeric(.)))) %>%
# Save to file
  saveRDS("census2020/block_data.rds")

plan(sequential)
```


### Get Census Block Group Data

```{r}
library(censusapi)

myvars <- list(
    # 2020 ACS5 variables
    "GEO_ID" = "geoid",
    "B01003_001E" = "pop", # Total Population
    # Race
    "B02001_001E" = "pop_race",
    "B02001_002E" = "white",
    "B02001_003E" = "black",
    "B02001_004E" = "natam",
    "B02001_005E" = "asian",
    "B02001_006E" = "pacific",
    # Ethnicity
    "B03003_001E" = "pop_ethnicity",
    "B03003_003E" = "hispanic",
    # Age
    "B01001_020E" = "pop_age_65_66_male",
    "B01001_021E" = "pop_age_67_69_male",
    "B01001_022E" = "pop_age_70_74_male",
    "B01001_023E" = "pop_age_75_79_male",
    "B01001_024E" = "pop_age_80_84_male",
    "B01001_025E" = "pop_age_85_over_male",

    "B01001_044E" = "pop_age_65_66_female",
    "B01001_045E" = "pop_age_67_69_female",
    "B01001_046E" = "pop_age_70_74_female",
    "B01001_047E" = "pop_age_75_79_female",
    "B01001_048E" = "pop_age_80_84_female",
    "B01001_049E" = "pop_age_85_over_female",

    "B01001_026E" = "pop_women", # Gender

    #"EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER"
    "B15003_001E" = "pop_education",
    "B15003_019E" = "edu_some_college_under_1",
    "B15003_020E" = "edu_some_college_over_1",
    "B15003_021E" = "edu_associates",
    "B15003_022E" = "edu_bachelors",
    "B15003_023E" = "edu_masters",
    "B15003_024E" = "edu_professional",
    "B15003_025E" = "edu_doctoral",
    # Median household income in the past 12 months (in 2020 inflation-adjusted dollars)
    "B19013_001E" = "median_household_income",
    # FAMILY INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS)
    "B19101_001E" = "pop_income",	#Estimate!!Total:
    "B19101_002E" = "pop_under_10000",	#Estimate!!Total:!!Less than $10,000
    "B19101_003E" = "pop_10000_14999",	#Estimate!!Total:!!$10,000 to $14,999
    "B19101_004E" = "pop_15000_19999",	#Estimate!!Total:!!$15,000 to $19,999
    "B19101_005E" = "pop_20000_24999",	#Estimate!!Total:!!$20,000 to $24,999
    "B19101_006E" = "pop_25000_29999",	#Estimate!!Total:!!$25,000 to $29,999
    "B19101_007E" = "pop_30000_34999",	#Estimate!!Total:!!$30,000 to $34,999
    "B19101_008E" = "pop_35000_39999",	#Estimate!!Total:!!$35,000 to $39,999
    "B19101_009E" = "pop_40000_44999",	#Estimate!!Total:!!$40,000 to $44,999
    "B19101_010E" = "pop_45000_49999",	#Estimate!!Total:!!$45,000 to $49,999
    "B19101_011E" = "pop_50000_59999",	#Estimate!!Total:!!$50,000 to $59,999
    "B19101_012E" = "pop_60000_74999",	#Estimate!!Total:!!$60,000 to $74,999
    "B19101_013E" = "pop_75000_99999",	#Estimate!!Total:!!$75,000 to $99,999
    "B19101_014E" = "pop_100000_124999",	#Estimate!!Total:!!$100,000 to $124,999
    "B19101_015E" = "pop_125000_149999",	#Estimate!!Total:!!$125,000 to $149,999
    "B19101_016E" = "pop_150000_199999",	#Estimate!!Total:!!$150,000 to $199,999
    "B19101_017E" = "pop_200000_plus",	#Estimate!!Total:!!$200,000 or more
    # Gini Index of Income Inequality
    "B19083_001E" = "gini", # GINI INDEX OF INCOME INEQUALITY
    # Housing Costs
    "B25105_001E" = "median_monthly_housing_costs", # Median Monthly Housing Costs in USD
    # Employment
    "B23025_003E" = "pop_labor_force", # Estimate!!Total!!In labor force!!Civilian labor force
    "B23025_004E" = "pop_employed", #Estimate!!Total!!In labor f orce!!Civilian labor force!!Employed
    "B23025_005E" = "pop_unemployed", # Estimate!!Total:!!In labor force:!!Civilian labor force:!!Unemployed
    "B29001_001E" = "pop_voting_age" # Citizen, Voting Age Population
    ) %>%
  as_tibble() %>%
  pivot_longer(cols = -c(), names_to = "id", values_to = "variable")


library(future)
library(furrr)

# Set up parallel processing

# Check current worksers
nbrOfWorkers()
# check available workers
numworkers <- availableWorkers() %>% length()
# Initiate parallel processing
# Always use at least 1 fewer than the total available workers
plan(multisession, workers = numworkers - 1)

myunits <- read_csv("census2020/sample_tracts.csv") %>%
  mutate(state = str_sub(geoid, 1,2),
         county = str_sub(geoid, 3,5),
         tract = str_sub(geoid, 6,-1)) %>%
  mutate(geoid = str_sub(geoid, 1,5)) %>%
  select(geoid, state, county) %>%
  distinct()

myunits %>%
  split(.$geoid) %>%
  future_map_dfr(
    ~censusapi::getCensus(
      name = "acs/acs5",
      vintage = 2020,
      key = censuskey,
      # Grab these variables
      vars = myvars$id,
      region = "block group:*",
      regionin = paste(
        "state:", .$state, # Choose just blocks in this state
        "&", "county:", .$county,  # in this county
        #        "&", "tract:", .$tract, # in this tract
        sep = "")),
      .progress = TRUE) %>%
      # Keep just the rows with our specified names
      select(myvars$id) %>%
      # Rename them to simpler, more easily understandable names
      magrittr::set_colnames(value = c(myvars$variable)) %>%
      # better format the geoid
      mutate(geoid = str_remove(geoid, "1500000US"),
             tract = str_sub(geoid, 1, 11)) %>%
      # Filter to just block groups in our tracts of interest
      filter(tract %in% read_csv("census2020/sample_tracts.csv")$geoid) %>%
      select(-tract) %>%
     # Save to file
    saveRDS("census2020/bg_data.rds")

plan(sequential)


# Adjust data
read_rds("census2020/bg_data.rds") %>%
  # Convert subcategories into percentages
  mutate(
    women = pop_women / pop,
    white = white / pop_race,
    black = black / pop_race,
    asian = asian / pop_race,
    natam = natam / pop_race,
    pacific = pacific / pop_race,
    hisplat = hispanic / pop_ethnicity,
    unemployment = pop_unemployed / pop_labor_force * 1000,
    some_college = (edu_some_college_under_1 + edu_some_college_over_1 + edu_associates +
                      edu_bachelors + edu_masters + edu_professional + edu_doctoral) / pop_education, # % of those over 25 with ANY college education
    over_65 = (pop_age_65_66_male + pop_age_67_69_male +
                 pop_age_70_74_male + pop_age_75_79_male +
                 pop_age_80_84_male + pop_age_85_over_male +
                 pop_age_65_66_female + pop_age_67_69_female +
                 pop_age_70_74_female + pop_age_75_79_female +
                 pop_age_80_84_female + pop_age_85_over_female) / pop,
    income_0_60K = (pop_under_10000 + pop_10000_14999 +
                      pop_15000_19999 + pop_20000_24999 +
                      pop_25000_29999 + pop_30000_34999 +
                      pop_35000_39999 + pop_40000_44999 +
                      pop_45000_49999 + pop_50000_59999) / pop_income) %>%
  # Reset any nans to NA
  mutate_at(vars(women:income_0_60K),
            list(~if_else(is.nan(.) | is.infinite(.), NA_real_, as.numeric(.)))) %>%
  select(geoid, pop, women, over_65, hisplat, white, black, asian, natam, pacific,
         some_college, income_0_60K, median_household_income,
         median_monthly_housing_costs, gini, unemployment) %>%
  write_rds("census2020/bg_data.rds")


```


### Get Census Tract Data

```{r}
library(censusapi)

myvars <- list("GEO_ID" = "geoid",
     "P1_001N" = "pop",
     "P1_003N" = "white",
     "P1_004N" = "black",
     "P1_005N" = "natam",
     "P1_006N" = "asian",
     "P2_002N" = "hisplat",
     "H1_001N" = "units",
     "H1_002N" = "units_occupied") %>%
  as_tibble() %>%
  pivot_longer(cols = -c(), names_to = "id", values_to = "variable")


library(future)
library(furrr)

# Set up parallel processing

# Check current worksers
nbrOfWorkers()
# check available workers
numworkers <- availableWorkers() %>% length()
# Initiate parallel processing
# Always use at least 1 fewer than the total available workers
plan(multisession, workers = numworkers - 1)

myunits <- read_csv("census2020/sample_tracts.csv") %>%
  mutate(state = str_sub(geoid, 1,2),
         county = str_sub(geoid, 3,5),
         tract = str_sub(geoid, 6,-1)) %>%
  mutate(geoid = str_sub(geoid, 1,5)) %>%
  select(geoid, state, county) %>%
  distinct()

# For reference!
# https://api.census.gov/data/2020/dec/pl/examples.html
# https://api.census.gov/data/2020/dec/pl/variables.html

myunits %>%
  split(.$geoid) %>%
  future_map_dfr(
    ~censusapi::getCensus(
      key = censuskey,
      vintage = "2020",
      name = "dec/pl",
      region = "tract:*",
      regionin = paste(
        "state:", .$state, # Choose just blocks in this state
        "&", "county:", .$county,  # in this county
        sep = ""),
      # Grab these variables
      vars = myvars$id),
    .progress = TRUE) %>%
  # Keep just the rows with our specified names
    select(myvars$id) %>%
  # Rename them to simpler, more easily understandable names
  magrittr::set_colnames(value = c(myvars$variable)) %>%
  # Convert subcategories into percentages
  mutate(white = white / pop,
         black = black / pop,
         asian = asian / pop,
         natam = natam / pop,
         hisplat = hisplat / pop,
         units_occupied = units_occupied / units) %>%
  # Reset any nans to NA
  mutate_at(vars(white:units_occupied),
            list(~if_else(is.nan(.) | is.infinite(.), NA_real_, as.numeric(.)))) %>%
  # better format the geoid
  mutate(geoid = str_remove(geoid, "1400000US")) %>%
  # Filter to just blocks in our tracts of interest
  filter(geoid %in% read_csv("census2020/sample_tracts.csv")$geoid) %>%
  # Save to file
  saveRDS("census2020/tract_data.rds")

plan(sequential)




## Get Tally

```{r, eval = FALSE}
library(tidyverse)
library(sf)
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Get equal area conic projection
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Get bounds
bounds <- read_sf("all/bounds.geojson") %>%
  st_transform(crs = aea) %>%
  mutate(area = as.numeric(st_area(geometry)) / 1000000)

# Get Sites
sites <- read_sf("all/sites.geojson") %>%
  st_transform(crs = aea)


# First, let's calculate population density for each census block.
blocks <- read_sf("all/blocks.geojson") %>%
  filter(area_land > 0) %>%
  # Join in block-level census data
  left_join(
    by = c("geoid"),
    y = read_rds("census2020/block_data.rds")) %>%
  # Calculate population density
  mutate(pop_density = pop / (area_land / (1000^2) )  ) %>%
  st_transform(crs = aea)

# How many points are in the grid?
# What's the approximate population density in the grid?

# Get tallies by grid cell
block_traits <- read_sf("all/grid1km.geojson") %>%
  st_transform(crs = aea) %>%
  # Join in block traits
  st_join(blocks %>% select(pop_density, white:hisplat, units_occupied),
          left = TRUE) %>%
  as_tibble() %>%
  group_by(name, cell) %>%
  summarize_at(vars(pop_density, white:hisplat, units_occupied),
               list(~median(., na.rm = TRUE))) %>%
  ungroup()

site_traits <- read_sf("all/grid1km.geojson") %>%
  st_transform(crs = aea) %>%
  # Join in block traits
  st_join(sites %>% select(type), left = TRUE) %>%
  as_tibble() %>%
  group_by(name, cell) %>%
  summarize(community_space = sum(type == "Community Space", na.rm = TRUE),
            place_of_worship = sum(type == "Place of Worship", na.rm = TRUE),
            social_business = sum(type == "Social Business", na.rm = TRUE),
            park = sum(type == "Park", na.rm = TRUE))

read_sf("all/grid1km.geojson") %>%
  left_join(by = c("name", "cell"),
            y = site_traits)  %>%
  left_join(by = c("name", "cell"),
            y = block_traits)  %>%
  mutate_at(vars(community_space:park),
            list(~./pop_density * 1000)) %>%
  st_write("all/tally1km.geojson", delete_dsn = TRUE)


# Get tallies by block
site_traits_by_block <- blocks %>%
  st_transform(crs = aea) %>%
  # Join in block traits
  st_join(sites %>% select(type), left = TRUE) %>%
  as_tibble() %>%
  group_by(name, geoid) %>%
  summarize(community_space = sum(type == "Community Space", na.rm = TRUE),
            place_of_worship = sum(type == "Place of Worship", na.rm = TRUE),
            social_business = sum(type == "Social Business", na.rm = TRUE),
            park = sum(type == "Park", na.rm = TRUE))

blocks %>%
  left_join(by = c("name", "geoid"), y = site_traits_by_block) %>%
  mutate_at(vars(community_space:park),
            list(~./pop_density * 1000)) %>%
  st_write("all/tallyblock.geojson", delete_dsn = TRUE)

remove(block_traits, site_traits, blocks, site_traits_by_block, sites, bounds)

# Then, get the block group traits
library(tidyverse)
library(sf)
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Get equal area conic projection
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Import tallies
tally <- read_sf("all/tally1km.geojson") %>%
  st_transform(crs = aea)


# Import block groups with data
bg <- read_sf("all/bg.geojson") %>%
  filter(area_land > 0) %>%
  # Join in block-level census data
  left_join(
    by = c("geoid"),
    y = read_rds("census2020/bg_data.rds")) %>%
  st_transform(crs = aea) %>%
  # These variables aren't available for block group
  select(-gini, -median_monthly_housing_costs)

# get block group traits
tally %>%
  select(cell, name) %>%
  st_join(bg %>% select(women:unemployment)) %>%
  as_tibble() %>%
  group_by(cell, name) %>%
  summarize_at(vars(women:unemployment), list(~median(., na.rm = TRUE))) %>%
  ungroup() %>%
  # Now join these results back into the tally spatial data.frame
  right_join(tally %>%
               # renaming demographic variables  to avoid duplicates
               rename(white_block = white, black_block = black,
                      asian_block = asian, natam_block = natam,
                      hisplat_block = hisplat, units_occupied_block = units_occupied),
             by = c("cell", "name")) %>%
  st_write("all/tally1kmbg.geojson", delete_dsn = TRUE)

#bg_traits %>%
#  as_tibble() %>%
#  group_by(name) %>%
#  summarize(sum = sum(is.na(black)) / n() )
```
### Get Smooth

(Doesn't work well right now)

```{r, message=FALSE, warning = FALSE, eval = FALSE}
rm(list = ls())
sf::sf_use_s2(FALSE)

#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Get equal area conic projection
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

tally <- read_sf("all/tally1kmbg.geojson")%>%
  st_set_crs(value = aea) %>%
  st_transform(crs = wgs)

tally %>% st_bbox()

library(future)
library(furrr)


tally %>%
  st_intersects(tally, sparse = TRUE)  %>%
  write_rds("all/tallyintersect.rds")


adj <- read_rds("all/tallyintersect.rds") %>%
  map_dfr(~data.frame(original = .[1],
           values = .))


out <- tally %>%
  as_tibble() %>%
  select(-geometry) %>%
  mutate(original = 1:n()) %>%
  left_join(by = "original", y = adj) %>%
  group_by(name, cell) %>%
  summarize_at(vars(women:units_occupied_block,
                    -park,-social_business,
                    -community_space,-place_of_worship),
               list(~median(., na.rm = TRUE))) %>%
  ungroup()

```

## Get Social Capital

First, let's download the most up to date versions of our social capital indices.

 ```{r}
 library(dataverse)

 dataverse::get_dataframe_by_name(
   filename = "index_tracts_V3_04_10_2022.tab",
   dataset = "doi:10.7910/DVN/OSVCRC",
   server = "dataverse.harvard.edu") %>%
   # Download values needed
   filter(geoid %in% unique(read_csv("census2020/sample_tracts.csv")$geoid)) %>%
   # Save
   saveRDS("census2020/sci.rds")
 ```
