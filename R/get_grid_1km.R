#' @name get_grid_1km()
#' @title Get Grid (1km)
#'
#' @description Give me a set of grids formatted for 1-square kilometer
#' @param name name of city

get_grid_1km = function(name){

  require(tigris)
  require(dplyr)
  require(sf)
  require(ggplot2)


  # Make a folder to contain it
  dir.create(name)

  # Get North American Albers Equal Area Conic Projection
  # https://spatialreference.org/ref/esri/north-america-albers-equal-area-conic/
  data("aea")

  # Get EPSG:4326 (WGS 84) projection
  #https://spatialreference.org/ref/epsg/wgs-84/
  data("wgs")

  bounds = paste(name, "/bounds.geojson", sep = "")
  dest = paste(name, "/grid1km.geojson", sep = "")

  # Get grid for boundaries
  bounds %>%
    read_sf() %>%
    # Set to equal albers conic projection
    st_transform(crs = aea) %>%
    # Make grid of 1000 square kilometer cells
    st_make_grid(cellsize = c(1000, 1000), crs = aea, what = "polygons", square = TRUE) %>%
    # Convert to sf
    st_as_sf(crs = aea) %>%
    # Join in bounds; keep only cells within the bounds
    st_join(
      y = bounds %>%
        read_sf() %>%
        st_transform(crs = aea),
      left = FALSE) %>%
    # Give each an ID to each remaining cell
    mutate(cell = 1:n()) %>%
    # Give again the simplest WGS projection
    st_transform(crs = wgs) %>%
    # Save to file
    st_write(dest, delete_dsn = TRUE)

  results = paste(name, "/results.csv", sep = "")

  sites <- results %>%
    read_csv() %>%
    st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
    st_transform(crs = wgs) %>%
    # Keep just sites within the bounds
    st_join(
      y = bounds %>%
        read_sf() %>%
        st_transform(crs = wgs),
      left = FALSE)

  ggplot() +
    # Plot background
    geom_sf(data = read_sf(paste(name, "/bounds.geojson", sep = "")),
            color = "black", size = 3, fill = "grey") +
    # Overlay grid
    geom_sf(data = read_sf(paste(name, "/grid1km.geojson", sep = "")),
            color = "white", size = 1, fill = "white", alpha = 0.25) +
    # Overlay grid
    geom_sf(data = sites,
            shape = 21, color = "white", size = 2, fill = "steelblue", alpha = 0.85) +
    theme_void(base_size = 14) +
    labs(subtitle = paste(
      "Gridded Polygons (1 km for analysis)")) %>%
    return()

}
