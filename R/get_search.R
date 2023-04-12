#' @name get_search()
#'
#' @description A search function for querying the Google Places API for the rows in `[name]/searches.csv`. Outputs `[name]/sites.rds`.
#' @param name name of city
#' @param yourkey API Key
#' @param data NULL
#' @param test TRUE

get_search = function(name, yourkey = Sys.getenv("PLACES_API_KEY"), data = NULL, test = TRUE){

  # test values:
  # name = "atlanta"; yourkey = Sys.getenv("PLACES_API_KEY"); data = NULL; test = TRUE

  require(readr)
  require(dplyr)
  require(furrr)
  require(googleway)

  # Make sure there is a directory for that name
  dir.create(name)

  # Make expected input file paths
  path = paste0(name, "/searches.csv")
  # Get output file path; if this is a test run, call it test_sites.rds
  dest = paste0(name, '/', if(test == TRUE){ "test_sites.rds" }else{ "sites.rds" })

  # If there's no input of any kind, stop.
  if(file.exists(path) == FALSE & is.null(data) ){
    stop(paste0("File does not exist: ", path))
  }

  # If there IS an input file path but no supplied data, read it in.
  if(file.exists(path) == TRUE & is.null(data)){ data = path %>% read_csv()

  # Otherwise, if there IS data supplied, just use it.
  }else if(!is.null(data)){ data = data }


  # Let's import our grid cell points
  data %>%
    # For every row
    split(.$id) %>%
    # Run this function
    furrr::future_map_dfr(
      ~tibble(
        google_places(
          location = c(.$y, # latitude
                       .$x), # longitude
          keyword = .$term,
          radius = 1125,  #circumference of a circle that extends that far
          key = yourkey)$result,
        # Add extra columns
        cell = .$cell,
        x = .$x,
        y = .$y,
        term = .$term),
      .progress = TRUE,
      .id = "id") %>%
    saveRDS(file = dest)

}
