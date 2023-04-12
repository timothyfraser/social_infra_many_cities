#' @name `get_data()`
#'
#' @description First, let's make a function to gather the necessary county subdivision and block group data given a series of names.
#' Get the county subdivision polygons for a series of county subdivisions located within the specified vector of `county` ids in the specified `state`
#' @param name ...
#' @param state ...
#' @param county ...
#' @param csub ...


get_data = function(name, state, county, csub){
  require(tigris)
  require(dplyr)
  require(purrr)
  require(readr)
  require(sf)

  # Make a directory to hold results
  dir.create(name)

  # Get EPSG:4326 (WGS 84) projection
  #https://spatialreference.org/ref/epsg/wgs-84/
  data("wgs")

  tigris::county_subdivisions(
    state = state,
    county = county,
    cb = TRUE, year = 2019) %>%
    sf::st_as_sf(crs = 4326) %>%
    dplyr::select(geoid = GEOID, name = NAME, area_land = ALAND, geometry) %>%
    # Filter to just county subdivisions (cities) of interest
    dplyr::filter(name %in% csub) %>%
    sf::st_write(paste(name, "/csub.geojson", sep = ""), delete_dsn = TRUE)

  # Load get bg function
  source("R/get_bg.R")

  # Get all block groups within these counties
  data.frame(county = county) %>%
    dplyr::split(.$county) %>%
    purrr::map(~get_bg(mycounty = .$county, mystate = state)) %>%
    dplyr::bind_rows() %>%
    sf::st_write(paste(name, "/bg.geojson",sep = ""), delete_dsn = TRUE)

  # Read in county subdivisions
  mycsub <- paste(name, "/csub.geojson", sep = "") %>% read_sf()

  # Read in Block Groups
  paste(name, "/bg.geojson", sep = "") %>%
    sf::read_sf() %>%
    # Keep just the blocks within our county subdivisions of interest
    sf::st_join(mycsub %>% select(name), left = FALSE) %>%
    # Now filter to just blocks on land
    dplyr::filter(area_land > 0) %>%
    # Save just the block groups within our county subdivisions, on land
    sf::st_write(paste(name, "/bg_land.geojson", sep = ""), delete_dsn = TRUE)

}
