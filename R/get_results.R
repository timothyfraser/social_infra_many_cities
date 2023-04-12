#' @name get_results()
#'
#' @description Process your Results. Let's write a function to extract the results into a meaningful CSV.
#' @param name name of city
#' @param test analyze just test_sites.rds or real sites.rds? TRUE/FALSE

get_results = function(name, test = FALSE){

  # testing values
  # name = "atlanta"; test = TRUE

  require(dplyr)
  require(sf)
  require(readr)

  # Make sure there is a directory for that name
  dir.create(name)


  if(test == FALSE){
    input = paste(name, "/sites.rds", sep = "")
    dest = paste(name, "/results.csv", sep = "")
  }else{
    input = paste(name, "/test_sites.rds", sep = "")
    dest = paste(name, "/test_results.csv", sep = "")
  }

  # Read in results from input path
  mysites <- input %>% read_rds()

  # Extract various business traits
  mytraits <- mysites %>%
    as_tibble() %>%
    mutate(categories = types %>%
             map(~paste(., collapse = "; ")) %>%
             unlist()) %>%
    select(place_id,
           any_of(c("business_status",
                    "rating", "categories",
                    "permanently_closed", "user_ratings_total"))) %>%
    distinct()


  # Extract key traits of interest (location, etc.)
  myresults <- mysites %>%
    mutate(lat = geometry$location$lat,
           lng = geometry$location$lng) %>%
    # group by place_id
    group_by(place_id) %>%
    summarize(
      term = unique(term),
      name = unique(name),
      lat = unique(lat),
      lng = unique(lng)) %>%
    ungroup() %>%
    # Classify the type
    mutate(type = term %>% recode_factor(
      "library" = "Community Space",
      "community center" = "Community Space",
      "city hall" = "Community Space",
      # Places of Worship
      "place of worship" = "Place of Worship",
      # Parks & Green Space
      "park" = "Park",
      "fountain" = "Park",
      "square" = "Park",
      "garden" = "Park",
      # Commercial
      "bookstore" = "Social Business",
      "cafe" = "Social Business")) %>%
    # Join in extratraits
    left_join(by = "place_id",
              y = mytraits) %>%
    # Reorder columns
    select(place_id, name, lat, lng, type, term,
           any_of(c("categories", "business_status", "rating",
                    "user_ratings_total", "permanently_closed")))

  # Write results to output path
  myresults %>% write_csv(file = dest)
  # Print message
  print(paste0("Output path: ", dest))

}
