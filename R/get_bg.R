#' @name `get_bg()`
#'
#' @description Write a function for gathering block groups
#' @param mycount ...
#' @param mystate ...
#' @importFrom tigris block_groups
#' @importFrom sf st_as_sf
#' @importFrom dplyr select `%>%`

get_bg = function(mycounty, mystate){
  require(tigris)
  require(sf)
  require(dplyr)

  # Extract blocks for these 5 counties making up New York (returns 5 boros)
  tigris::block_groups(
    state = mystate,
    county = mycounty,
    cb = TRUE,
    year = 2019) %>%
    st_as_sf(crs = 4326) %>%
    dplyr::select(geoid = GEOID, area_land = ALAND, geometry) %>%
    return()
}
