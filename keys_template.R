#' `keys.R`
#'
#' File of API keys. DO NOT SHARE ON GITHUB.
#' Fill this in with your values and then change file name to keys.R

# Let's load in your API keys here
Sys.setenv(
  # Google Places API Key
  "PLACES_API_KEY" = "a_very_long_key",
  # Census API Key
  "CENSUS_API_KEY" = "another_very_long_key",
  # PATH TO GEOPACKAGE
  "DATABASE" = "geo.gpkg"
)
