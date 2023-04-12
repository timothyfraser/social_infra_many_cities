#' @name get_map()
#'
#' @description A quick visualization function to see what we made.
#' @param name name of city
#' @param test use test data or real data? TRUE/FALSE

get_map = function(name, test = FALSE){
  require(dplyr)
  require(readr)
  require(sf)
  require(ggplot)

  # Get EPSG:4326 (WGS 84) projection
  #https://spatialreference.org/ref/epsg/wgs-84/
  data("wgs")

  # Get input file path
  condition = if(test == TRUE){ "/results.csv" }else{ "/test_results.csv"}
  input = paste0(name, condition)

  # Read in sites in sf format
  sites <- input %>%
    read_csv() %>%
    st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
    st_transform(crs = wgs)

  # Get bounds
  bounds <- paste(name, "/bounds.geojson", sep = "") %>% read_sf()
  # Get Grid
  grid <- paste(name, "/grid.geojson", sep = "") %>% read_sf()

  # Get colors
  data("colors")

  # Get subtitle
  subtitle = paste0( name, ": Social Infrastructure\n",  nrow(sites), " sites")

  ggplot() +
    geom_sf(data = bounds, color = "black", fill = "grey") +
    geom_sf(data = grid, color = "white", fill = NA) +
    geom_sf_text(data = grid,
                 mapping = aes(label = cell), color = "black", size = 1) +
    geom_sf(data = sites, mapping = aes(fill = type),
            shape = 21, color = "white", size = 1) +
    scale_fill_manual(
      breaks = names(colors),
      values = colors) +
    theme_void(base_size = 14) +
    labs(x = NULL, y = NULL, fill = "Type",
         subtitle = subtitle) %>%
    return()

}
