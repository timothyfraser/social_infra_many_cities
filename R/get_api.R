#' @name get_api()
#'
#' @description Let's write a function for automatically gathering our search criteria.
#' @param name name of city
#' @param yourkey Places API key
#' @param parallel parallel processing? TRUE/FALSE

get_api = function(name, yourkey = Sys.getenv("PLACES_API_KEY"), parallel = TRUE){

  require(tidyverse)
  require(googleway)
  require(sf)

  # initial Validity Check
  if(is.null(yourkey) | yourkey == ""){
    paste("Oops! Need to insert an API Key!") %>%
      return()

  }else{

    # If valid API key,
    # Load search function
    source("R/get_search.R")

    # Given valid API key, check if parallel
    if(parallel == TRUE){
      require(future)
      require(furrr)

      # Set up parallel processing for
      nbrOfWorkers() # Check current works
      numworkers <- availableWorkers() %>% length() # check available workers
      plan(multisession, workers = numworkers - 1) # Initiate parallel processing
      # Always use at least 1 fewer than the total available workers
      # Let the read know how many workers are being used:
      paste("Parallel Processing: Using ",
            numworkers - 1, " workers", sep = "") %>%
        print()

      # Run the search
      get_search(name = name, yourkey = yourkey, data = NULL, test = FALSE)

      # Finish parallel processing
      plan(sequential)

      paste("Parallel Finished") %>%
        print()


    }else{
      paste("No Parallel Processing Used") %>%
        print()

      # Run the search
      get_search(name = name, yourkey = yourkey, data = NULL, test = FALSE)

    }

  }

}
