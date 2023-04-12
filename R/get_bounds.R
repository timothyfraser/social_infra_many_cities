#' @name get_bounds()
#'
#' @description let's write a function to get us nice unified boundary polygons for making the grid.
#' @param name (string) name of directory in which data is stored; should match city name. Eg. altanta.

get_bounds = function(name){
  require(tigris)
  require(dplyr)
  require(sf)

  # Get EPSG:4326 (WGS 84) projection
  #https://spatialreference.org/ref/epsg/wgs-84/
  data("wgs")

  paste(name, "/bg_land.geojson", sep = "") %>%
    sf::read_sf() %>%
    # Transform data to WGS projection
    sf::st_transform(crs = wgs) %>%
    # Consolidate into a single polygon
    # Unioning 1000s of block groups, even the low resolution ones, takes a while
    dplyr::summarize(geometry = st_union(geometry)) %>%
    # Save bounds to file
    sf::st_write(paste(name, "/bounds.geojson", sep = ""), delete_dsn = TRUE)
}
