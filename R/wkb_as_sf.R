#' @name wkb_as_sf()
#' @title WKB to SF format
#' @description Function to convert an tibble with a WKB formatted geometry to sf format
#'

wkb_as_sf = function(data, sf_geometry_column = "geometry"){

  data[[sf_geometry_column]] <- sf::st_as_sfc(x = structure(as.list(data[[sf_geometry_column]]), class = "WKB"), EWKB=TRUE)
  return(data)

}
