#' @name get_testapi()
#'
#' @description Let's write a test-searching just 3 grid cells, before we spend lots of money.
#' @param name name of city
#' @param yourkey your Places API Key
#' @param parallel Use parallel processing? TRUE/FALSE
#' @param mycell ...

get_testapi = function(name, yourkey = Sys.getenv("PLACES_API_KEY"), parallel = TRUE, mycell = NULL){

  require(readr)
  require(dplyr)
  require(googleway)
  require(sf)

  # Make sure there is a directory for that name
  dir.create(name)

  # Import search data
  # If there is no cell number, import a random sample
  if(is.null(mycell)){
    # Let's import our grid cell points
    mysearches <- paste(name, "/searches.csv", sep = "") %>%
      read_csv() %>%
      # Grab 5 random searches
      sample_n(size = 5)

  }else{
    # If there is a cell number, import that particular range of cells
    # Let's import our grid cell points
    mysearches <- paste(name, "/searches.csv", sep = "") %>%
      read_csv() %>%
      # Grab all 10 searches in a specific cell ($0.25)
      filter(cell %in% mycell)
  }


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
      get_search(name = name, yourkey = yourkey,
                 # As a test search, using the supplied dataframe 'mysearches'
                 data = mysearches, test = TRUE)

      # Finish parallel processing
      plan(sequential)

      paste("Parallel Finished") %>%
        print()


    }else{
      paste("No Parallel Processing Used") %>%
        print()

      # Run the search
      get_search(name = name, yourkey = yourkey,
                 # As a test search, using the supplied dataframe 'mysearches'
                 data = mysearches, test = TRUE)

    }

  }

}
