#' @name get_query
#'
#' @description A function to generate a fully query template.
#' @param name name of city.
#' @param folder directory path in which city-folder will be located. Should be "search"
#' @param open Open the `query.R` file you just made? (Requires RStudio). Default is `FALSE`.
#' @param force Force overwrite the query document? TRUE/FALSE. Default is `FALSE`.

get_query = function(name, folder = "search", open = FALSE, force = FALSE){

  # Testing values
  # name = "atlanta"; folder = "search"; open = FALSE; force = FALSE
  require(dplyr)

  data("functions")

  path = paste0(get_folder(name, folder = folder), "/query.R")
  # Does it already exist? If so, don't overwrite it.
  check = file.exists(path)

  # If it exists and we DON'T want to overwrite it
  if(check == TRUE & force == FALSE){
    # Skip and don't do anything.
    print(paste0("---skipping; already exists: ", path))

    }else{
    # If it doesn't exist OR we do want to overwrite it, make a template version.
    template = paste(
      paste0("#' @name ", name, "/", "query.R"),
      paste0("#' @title API queries for: ", name),
      "#' @description A code to gather and process API data.",
      "",
      "# 0. SETUP: Load functions and core values ####################",
      "data('functions')",
      "data('aea')",
      "data('wgs')",
      "data('colors')",
      "",
      "# Load keys",
      "source('keys.R')",
      "",
      "",
      "# 1. INITIAL TESTING (Optional) ########################",
      "",
      "",
      "",
      "",
      "# 2. PARAMETERS #########################################",
      paste0("name = get_folder(", '"', name, '") # Get Folder-Augmented Name (eg. search/nyc)'),
      "",
      "",
      'state = "" ',
      'county = "" ',
      'csub = "" ',
      "",
      "",
      "# GATHER NECESSARY DATA ################################",
      "get_data(name = name, state = state, county = county, csub = csub)",
      "remove(state, county, csub)",
      "",
      "get_bounds(name = name) # Obtain relevant boudnaries for study area",
      "get_grid(name = name) # Obtain grid",
      "",
      "# RUN 5 TEST SEARCHES ###################################",
      "get_testapi(name = name, parallel = TRUE)",
      'get_file(name, "test_sites") %>% read_rds() # it works!',
      "get_results(name = name, test = TRUE)",
      "get_map(name = name, test = TRUE)",
      "",
      "# RUN ACTUAL SEARCHES ####################################",
      "get_api(name = name, parallel = TRUE)",
      "get_results(name = name, test = FALSE)",
      "get_map(name = name, test = FALSE)",
      "get_diagnostics(name = name)",
      "get_grid_1km(name = name)",
      "",
      "",
      "",
      "# END ####################################################",
      "print(paste0('---done: ', name)); rm(list = ls()); gc() # write message and clear",
      "",
      "",
      sep = "\n"
    )

    # Print it
    cat(template, file = path, sep = "\n")

    # Write a message to user
    print(paste0("---template made: ", path))

    # If open == TRUE, open the file. Requires RStudio.
    if(open == TRUE){ rstudioapi::navigateToFile(path) }

  }

}
