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


# combine = function(x){
#   print(x)
#   paste(x, "/tracts.geojson", sep = "") %>%
#     read_sf() %>%
#     mutate(name = x) %>%
#     return()
# }


# load("meta.rdata")
# meta$cities %>%
#   map(~combine(.)) %>%
#   bind_rows() %>%
#   st_write("all/tracts.geojson", delete_dsn = TRUE)


## Get Blocks   ################################################

library(tidyverse)
library(tigris)
library(sf)
# Get EPSG:4326 (WGS 84) projection
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

get_blocks = function(name, mystate, mycounty){

  require(tigris)
  require(dplyr)
  require(readr)
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
    st_join(read_sf(paste("search/", name, "/bounds.geojson", sep = "")) %>%
              select(geometry), left = FALSE) %>%
    mutate(name = name) %>%
    select(geoid = GEOID20, area_land = ALAND20, name) %>%
    st_write(paste("search/", name, "/blocks.geojson", sep = ""), delete_dsn = TRUE)
  gc()
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

get_blocks(name = "new_orleans", mystate = "LA", mycounty = "Orleans Parish")
get_blocks(name = "boulder", mystate = "CO", mycounty = "Boulder County")
get_blocks(name = "daytona_beach", mystate = "FL", mycounty = "Volusia County")
get_blocks(name = "miami", mystate = "FL", mycounty = "Miami-Dade County")
get_blocks(name = "atlanta", mystate = "GA", mycounty = c("Fulton County", "DeKalb County"))
get_blocks(name = "pittsburgh", mystate = "PA", mycounty = c("Allegheny County"))
get_blocks(name = "minneapolis_st_paul", mystate = "MN", mycounty = c("Hennepin County", "Ramsey County"))
get_blocks(name = "st_louis", mystate = "MO", mycounty = "510")
get_blocks(name = "honolulu", mystate = "HI", mycounty = "Honolulu County")
get_blocks(name = "louisville", mystate = "KY", mycounty = "Jefferson County")
get_blocks(name = "berkeley", mystate = "CA", mycounty = c("Alameda County"))
get_blocks(name = "oakland", mystate = "CA", mycounty = c("Alameda County"))
get_blocks(name = "norfolk", mystate = "VA", mycounty = "710")
get_blocks(name = "tulsa", mystate = "OK", mycounty = c("Osage County", "Rogers County", "Tulsa County", "Wagoner County"))
#  DONE




#load("meta.rdata")

# meta$cities %>%
#   split(.$name) %>%
#   map(~read_sf(paste(.$name, "/blocks.geojson", sep = "")), .id = "name") %>%
#   bind_rows(.id = "name") %>%
#   st_write("all/blocks.geojson", delete_dsn = TRUE)


