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

# Get all blocks and save to file (overwrite table)
data("meta")
# For each city...
for(i in meta$cities){
  combine(name = i, type = "blocks", ext = ".geojson", area = FALSE) %>%
    st_write(obj = ., dsn = geo, layer = "blocks", delete_layer = FALSE, format = "WKB", append = TRUE)
  gc()
}

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

# Get all sites, filted to within the bounds!
meta$cities %>%
  map(~filter_sites(name = .)) %>%
  bind_rows() %>%
  st_write(obj = ., dsn = geo, layer = "sites", delete_layer = TRUE, format = "WKB")



geo = connect()

geo %>% dbListTables()


dbDisconnect(geo)





### Get Census Tract Data
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
