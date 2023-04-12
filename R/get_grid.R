#' @name get_grid()
#'
#' @title Get Grid (2 km searches)
#' @description Let's write a function to get us a grid of 2 square km cells based on the boundaries of our land-based block grounds within the county subdivisions of interest.
#' @param name Name of city (will become directory in which data is stored)

get_grid = function(name){

  # Give me a set of county subdivisions formatted as follows

  require(tigris)
  require(dplyr)
  require(sf)


  # Make a folder to contain it
  dir.create(name)

  # Get North American Albers Equal Area Conic Projection
  data("aea")
  # Get EPSG:4326 (WGS 84) projection
  data("wgs")

   # Get grid for boundaries
  paste(name, "/bounds.geojson", sep = "") %>%
    read_sf() %>%
    # Set to equal albers conic projection
    st_transform(crs = aea) %>%
    # Make grid of 2000 square kilometer cells
    st_make_grid(cellsize = c(2000, 2000), crs = aea, what = "polygons", square = TRUE) %>%
    # Convert to sf
    st_as_sf(crs = aea) %>%
    # Join in bounds; keep only cells within the bounds
    st_join(
      y = read_sf(paste(name, "/bounds.geojson", sep = "")) %>%
        st_transform(crs = aea),
      left = FALSE) %>%
    # Give each an ID to each remaining cell
    mutate(cell = 1:n()) %>%
    # Give again the simplest WGS projection
    st_transform(crs = wgs) %>%
    # Save to file
    st_write(paste(name, "/grid.geojson", sep = ""), delete_dsn = TRUE)


  # Get centroids of cells
  paste(name, "/grid.geojson", sep = "") %>%
    read_sf() %>%
    # Set to equal albers conic projection
    st_transform(crs = aea) %>%
    # Get centroid
    mutate(geometry = st_centroid(geometry)) %>%
    # Give again the simplest WGS projection
    st_transform(crs = wgs) %>%
    # Save to file
    st_write(paste(name, "/grid_points.geojson", sep = ""), delete_dsn = TRUE)

  paste(name, "/grid_points.geojson", sep = "") %>%
    read_sf() %>%
    # Extract coordinates
    mutate(x = st_coordinates(geometry)[,1], # longitude
           y = st_coordinates(geometry)[,2] # latitude
    ) %>%
    # Format as tibble
    as_tibble() %>%
    select(cell, x, y) %>%
    # Now repeat that data.frame for every search term (10)
    expand_grid(
      term = c(
        # Community Spaces
        "library",
        "community center",
        "city hall",
        # Place of Worship
        "place of worship",
        # Parks
        "park",
        "fountain",
        "square",
        "garden",
        # Social Businesses
        "bookstore",
        "cafe")) %>%
    # Get an ID for every row
    mutate(id = 1:n()) %>%
    # Save to file
    write_csv(paste(name, "/searches.csv", sep = ""))


  ggplot() +
    # Plot background
    geom_sf(data = read_sf(paste(name, "/bounds.geojson", sep = "")),
            color = "black", size = 3, fill = "grey") +
    # Overlay grid
    geom_sf(data = read_sf(paste(name, "/grid.geojson", sep = "")),
            color = "white", size = 1, fill = "white", alpha = 0.25) +
    # Overlay grid
    geom_sf(data = read_sf(paste(name, "/grid_points.geojson", sep = "")),
            shape = 21, color = "white", size = 2, fill = "steelblue", alpha = 0.85) +
    theme_void(base_size = 14) +
    labs(subtitle = paste(
      "Gridded Polygons (2 km for searches)\nExpected Cost: $",
      round(
        read_csv(
          paste(name, "/searches.csv", sep = "")) %>%
          nrow() * 0.05,
        2) ) ) %>%
    return()

}
