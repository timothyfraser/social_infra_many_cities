#' @name generate.R
#'
#' @description A script to generate supplemental or formatted information
#' based on our previous API calls.
#'


# 3. Overall

# 1. Get Cities #######################################

# Create some metadata
meta <- list(
  top = c("nyc", "la", "chicago", "houston", "phoenix",
             "philadelphia", "san_antonio", "san_diego", "dallas", "san_jose",
             "austin", "jacksonville", "fort_worth", "columbus", "indianapolis",
             "charlotte", "san_francisco", "seattle", "denver", "dc",
             "nashville", "oklahoma", "el_paso", "boston", "portland"),
             # Extras
  others = c("worcester","ithaca",
             "atlanta", "berkeley", "boulder", "daytona_beach","honolulu",
             "louisville", "miami", "minneapolis_st_paul", "new_orleans",
             "norfolk", "oakland", "pittsburgh","st_louis")
  )
meta$cities = c(meta$top, meta$others) %>% sort()

# Save to fil
save(meta, file = "meta.rdata")


# 2. Get Bounds  ################################################
library(tidyverse)
library(sf)
# Get EPSG:4326 (WGS 84) projection
#https://spatialreference.org/ref/epsg/wgs-84/
data("wgs")

# Get equal area conic projection
data("aea")

combine = function(x){
  print(x)
  paste(x, "/bounds.geojson", sep = "") %>%
    read_sf() %>%
    mutate(name = x) %>%
    return()
}

load("meta.rdata")
meta$cities %>%
  map(~combine(.)) %>%
  dplyr::bind_rows() %>%
  st_write("all/bounds.geojson", delete_dsn = TRUE)


## Get Block Groups   ################################################

library(tidyverse)
library(sf)
# Get EPSG:4326 (WGS 84) projection
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Get equal area conic projection
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

combine = function(x){

  print(x)

  paste(x, "/bg.geojson", sep = "") %>%
    read_sf() %>%
    mutate(name = x) %>%
    return()
}
load("meta.rdata")

meta$cities %>%
  map(~combine(.)) %>%
  dplyr::bind_rows() %>%
  st_write("all/bg.geojson", delete_dsn = TRUE)


## Get Tracts   ################################################

# Finally, for validation purposes, let's also gather the census tracts for each locale.

library(tidyverse)
library(tigris)
library(sf)

get_tracts = function(name, mystate, mycounty){

  require(tigris)
  require(tidyverse)
  require(sf)

  # Get EPSG:4326 (WGS 84) projection
  #https://spatialreference.org/ref/epsg/wgs-84/
  wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  tigris::tracts(
    state = mystate,
    county = mycounty,
    year = 2020) %>%
    st_as_sf()  %>%
    st_transform(crs = wgs) %>%
    st_join(read_sf(paste(name, "/bounds.geojson", sep = "")) %>%
              select(geometry), left = FALSE) %>%
    mutate(name = name) %>%
    select(geoid = GEOID, area_land = ALAND) %>%
    st_write(paste(name, "/tracts.geojson", sep = ""), delete_dsn = TRUE)

}


# Wave 1
get_tracts(name = "nyc", mystate = "NY",
           mycounty = c("New York", "Queens", "Kings",
                        "Bronx", "Richmond"))
get_tracts(name = "chicago", mystate = "IL",
           mycounty = c("Cook"))
get_tracts(name = "la", mystate = "CA",
           mycounty = c("Los Angeles"))
get_tracts(name = "houston", mystate = "TX",
           mycounty = c("Harris County"))
get_tracts(name = "phoenix", mystate = "AZ",
           mycounty = c("Maricopa County"))

# Wave 2
get_tracts(name = "philadelphia", mystate = "PA",
           mycounty = c("Philadelphia County"))
get_tracts(name = "san_antonio", mystate = "TX",
           mycounty = c("Bexar County"))
get_tracts(name = "san_diego", mystate = "CA",
           mycounty = c("San Diego County"))
get_tracts(name = "dallas", mystate = "TX",
           mycounty = c("Dallas County", "Collin County", "Denton County",
                        "Rockwall County", "Kaufman County"))
get_tracts(name = "san_jose", mystate = "CA",
           mycounty = c("Santa Clara County"))

# Wave 3
get_tracts(name = "austin", mystate = "TX",
           mycounty = c("Travis County", "Hays County", "Williamson County"))
get_tracts(name = "jacksonville", mystate = "FL",
           mycounty = c("Duval County"))
get_tracts(name = "fort_worth", mystate = "TX",
           mycounty = c("Tarrant County", "Denton County", "Parker County",
                        "Wise County", "Johnson County"))
get_tracts(name = "columbus", mystate = "OH",
           mycounty = c("Franklin County", "Delaware County", "Fairfield County"))
get_tracts(name = "indianapolis", mystate = "IN",
           mycounty = c("Marion County"))


# Wave 4
get_tracts(name = "charlotte", mystate = "NC",
           mycounty = c("Mecklenburg County"))
get_tracts(name = "san_francisco", mystate = "CA",
           mycounty = c("San Francisco County"))
get_tracts(name = "seattle", mystate = "WA",
           mycounty = c("King County"))
get_tracts(name = "denver", mystate = "CO",
           mycounty = c("Denver County"))
get_tracts(name = "dc", mystate = "DC",
           mycounty = c("District of Columbia"))

# Wave 5
get_tracts(name = "nashville", mystate = "TN",
           mycounty = c("Davidson County"))
get_tracts(name = "oklahoma", mystate = "OK",
           mycounty = c("Oklahoma County", "Canadian County", "Cleveland County",
                        "Pottawatomie County"))
get_tracts(name = "el_paso", mystate = "TX",
           mycounty = c("El Paso County"))
get_tracts(name = "boston", mystate = "MA",
           mycounty = c("Suffolk County"))
get_tracts(name = "portland", mystate = "OR",
           mycounty = c("Multnomah County", "Washington County", "Clackamas County"))

# Special
get_tracts(name = "worcester", mystate = "MA", mycounty = c("Worcester"))

get_tracts(name = "ithaca", mystate = "NY", mycounty = c("Tompkins"))

# Wave 6

get_tracts(name = "new_orleans", mystate = "LA", mycounty = "Orleans Parish")
get_tracts(name = "boulder", mystate = "CO", mycounty = "Boulder County")
get_tracts(name = "daytona_beach", mystate = "FL", mycounty = "Volusia County")
get_tracts(name = "miami", mystate = "FL", mycounty = "Miami-Dade County")
get_tracts(name = "atlanta", mystate = "FL", mycounty = c("Fulton County", "DeKalb County"))
get_tracts(name = "pittsburgh", mystate = "PA", mycounty = c("Allegheny County"))
get_tracts(name = "minneapolis_st_paul", mystate = "MN", mycounty = c("Hennepin County", "Ramsey County"))
get_tracts(name = "st_louis", mystate = "MO", mycounty = "510")
get_tracts(name = "honolulu", mystate = "HI", mycounty = "Honolulu County")
get_tracts(name = "louisville", mystate = "KY", mycounty = "Jefferson County")
get_tracts(name = "berkeley", mystate = "CA", mycounty = c("Alameda County"))
get_tracts(name = "norfolk", mystate = "VA", mycounty = "710")
get_tracts(name = "oakland", mystate = "CA", mycounty = c("Alameda County"))
#get_tracts(name = "tulsa", mystate = "OK", mycounty = c("Osage County", "Rogers County", "Tulsa County", "Wagoner County"))


combine = function(x){
  print(x)
  paste(x, "/tracts.geojson", sep = "") %>%
    read_sf() %>%
    mutate(name = x) %>%
    return()
}


load("meta.rdata")
meta$cities %>%
  map(~combine(.)) %>%
  bind_rows() %>%
  st_write("all/tracts.geojson", delete_dsn = TRUE)


## Get Blocks   ################################################

library(tidyverse)
library(tigris)
library(sf)
# Get EPSG:4326 (WGS 84) projection
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

get_blocks = function(name, mystate, mycounty){

  require(tigris)
  require(tidyverse)
  require(sf)

  # Get EPSG:4326 (WGS 84) projection
  #https://spatialreference.org/ref/epsg/wgs-84/
  wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  tigris::blocks(
    state = mystate,
    county = mycounty,
    year = 2020) %>%
    st_as_sf()  %>%
    st_transform(crs = wgs) %>%
    st_join(read_sf(paste(name, "/bounds.geojson", sep = "")) %>%
              select(geometry), left = FALSE) %>%
    mutate(name = name) %>%
    select(geoid = GEOID20, area_land = ALAND20, name) %>%
    st_write(paste(name, "/blocks.geojson", sep = ""), delete_dsn = TRUE)

}


# Wave 1
get_blocks(name = "nyc", mystate = "NY",
           mycounty = c("New York", "Queens", "Kings",
                        "Bronx", "Richmond"))
get_blocks(name = "chicago", mystate = "IL",
           mycounty = c("Cook"))
get_blocks(name = "la", mystate = "CA",
           mycounty = c("Los Angeles"))
get_blocks(name = "houston", mystate = "TX",
           mycounty = c("Harris County"))
get_blocks(name = "phoenix", mystate = "AZ",
           mycounty = c("Maricopa County"))

# Wave 2
get_blocks(name = "philadelphia", mystate = "PA",
           mycounty = c("Philadelphia County"))
get_blocks(name = "san_antonio", mystate = "TX",
           mycounty = c("Bexar County"))
get_blocks(name = "san_diego", mystate = "CA",
           mycounty = c("San Diego County"))
get_blocks(name = "dallas", mystate = "TX",
           mycounty = c("Dallas County", "Collin County", "Denton County",
                        "Rockwall County", "Kaufman County"))
get_blocks(name = "san_jose", mystate = "CA",
           mycounty = c("Santa Clara County"))

# Wave 3
get_blocks(name = "austin", mystate = "TX",
           mycounty = c("Travis County", "Hays County", "Williamson County"))
get_blocks(name = "jacksonville", mystate = "FL",
           mycounty = c("Duval County"))
get_blocks(name = "fort_worth", mystate = "TX",
           mycounty = c("Tarrant County", "Denton County", "Parker County",
                        "Wise County", "Johnson County"))
get_blocks(name = "columbus", mystate = "OH",
           mycounty = c("Franklin County", "Delaware County", "Fairfield County"))
get_blocks(name = "indianapolis", mystate = "IN",
           mycounty = c("Marion County"))


# Wave 4
get_blocks(name = "charlotte", mystate = "NC",
           mycounty = c("Mecklenburg County"))
get_blocks(name = "san_francisco", mystate = "CA",
           mycounty = c("San Francisco County"))
get_blocks(name = "seattle", mystate = "WA",
           mycounty = c("King County"))
get_blocks(name = "denver", mystate = "CO",
           mycounty = c("Denver County"))
get_blocks(name = "dc", mystate = "DC",
           mycounty = c("District of Columbia"))

# Wave 5
get_blocks(name = "nashville", mystate = "TN",
           mycounty = c("Davidson County"))
get_blocks(name = "oklahoma", mystate = "OK",
           mycounty = c("Oklahoma County", "Canadian County", "Cleveland County",
                        "Pottawatomie County"))
get_blocks(name = "el_paso", mystate = "TX",
           mycounty = c("El Paso County"))
get_blocks(name = "boston", mystate = "MA",
           mycounty = c("Suffolk County"))
get_blocks(name = "portland", mystate = "OR",
           mycounty = c("Multnomah County", "Washington County", "Clackamas County"))

# Special
get_blocks(name = "worcester", mystate = "MA", mycounty = c("Worcester"))

get_blocks(name = "ithaca", mystate = "NY", mycounty = c("Tompkins"))

# Wave 6

# NOT YET DONE
get_blocks(name = "new_orleans", mystate = "LA", mycounty = "Orleans Parish")
get_blocks(name = "boulder", mystate = "CO", mycounty = "Boulder County")
get_blocks(name = "daytona_beach", mystate = "FL", mycounty = "Volusia County")
get_blocks(name = "miami", mystate = "FL", mycounty = "Miami-Dade County")
get_blocks(name = "atlanta", mystate = "FL", mycounty = c("Fulton County", "DeKalb County"))
get_blocks(name = "pittsburgh", mystate = "PA", mycounty = c("Allegheny County"))
get_blocks(name = "minneapolis_st_paul", mystate = "MN", mycounty = c("Hennepin County", "Ramsey County"))
get_blocks(name = "st_louis", mystate = "MO", mycounty = "510")
get_blocks(name = "honolulu", mystate = "HI", mycounty = "Honolulu County")
get_blocks(name = "louisville", mystate = "KY", mycounty = "Jefferson County")
get_blocks(name = "berkeley", mystate = "CA", mycounty = c("Alameda County"))
get_blocks(name = "norfolk", mystate = "VA", mycounty = "710")
get_blocks(name = "oakland", mystate = "CA", mycounty = c("Alameda County"))
get_blocks(name = "tulsa", mystate = "OK", mycounty = c("Osage County", "Rogers County", "Tulsa County", "Wagoner County"))

load("meta.rdata")

meta$cities %>%
  split(.$name) %>%
  map(~read_sf(paste(.$name, "/blocks.geojson", sep = "")), .id = "name") %>%
  bind_rows(.id = "name") %>%
  st_write("all/blocks.geojson", delete_dsn = TRUE)




## Get Sites   ################################################

# Get EPSG:4326 (WGS 84) projection
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


# Get equal area conic projection
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


# Get bounds
bounds <- read_sf("all/bounds.geojson") %>%
  st_transform(crs = aea) %>%
  mutate(area = as.numeric(st_area(geometry)) / 1000000)

load("meta.rdata")

data.frame(city = meta$cities) %>%
  split(.$city) %>%
  map_dfr(~read_csv(paste(.$city, "/results.csv", sep = "")), .id = "city") %>%
  select(place_id:term) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = wgs) %>%
  # Now add in some bounds
  st_transform(crs = aea) %>%
  # Filter to just sites within city
  st_join(bounds %>% select(geometry), left = FALSE) %>%
  # Turn back to simple formatting
  st_transform(crs = wgs) %>%

  st_write("all/sites.geojson", delete_dsn = TRUE)


## Get Grids   ################################################

library(tidyverse)
library(sf)
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Get equal area conic projection
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

combine = function(name){
  read_sf( paste(name, "/grid1km.geojson", sep = "") ) %>%
    st_as_sf() %>%
    mutate(name = name) %>%
    return()
}


load("meta.rdata")
meta$cities %>%
  map(~combine(.)) %>%
  bind_rows() %>%
  st_write("all/grid1km.geojson", delete_dsn = TRUE)


# 4. Census

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

```{r}
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
```

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


```



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
