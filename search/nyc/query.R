#' @name nyc/query.R
#' @title New York City API queries
#' @description A code to gather and process API data for NYC.

# Load functions and core values
data("functions")
data("aea")
data("wgs")
data("colors")

# Load keys
source("keys.R")

# Gather all necessary data
name = get_folder("nyc")
get_data(name = name, state = "NY",
         county = c("New York", "Queens", "Kings", "Bronx", "Richmond"),
         csub = c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island"))
get_bounds(name = name) # # Obtain relevant boundaries for study area
get_grid(name = name) # Obtain grid

# Test it
get_testapi(name = name, parallel = TRUE, mycell = 131) # Run 5 Test Searches
get_file(name, "test_sites") %>% read_rds() # it works!
get_results(name = name, test = TRUE) # Refine Results
get_map(name = name, test = TRUE)

# Run actual searches
get_api(name = name, parallel = TRUE) # Run Searches
get_results(name = name, test = FALSE) # Refine results
get_map(name = name, test = FALSE)
get_diagnostics(name = name)
get_grid_1km(name = name)

# Write message
print(paste0("---done: ", name))
rm(list = ls()); gc()

