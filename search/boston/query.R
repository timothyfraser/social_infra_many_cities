#' @name boston/query.R
#' @title API queries for: boston
#' @description A code to gather and process API data.

# 0. SETUP: Load functions and core values ####################
data('functions')
data('aea')
data('wgs')
data('colors')

# Load keys
source('keys.R')


# 1. INITIAL TESTING (Optional) ########################


```{r, eval = FALSE}
# Gather all necessary data
get_data(name = "boston", state = "MA",
         county = c("Suffolk"),
         csub = c("Boston"))
get_bounds(name = "boston") # Obtain relevant boundaries for study area
get_grid(name = "boston") # Obtain grid

# Upload boston_sites.rds from our original run in May
read_rds("boston/boston_sites.rds") %>%
  mutate(term = file %>% str_remove("query/search") %>% str_remove(".rds")) %>%
  filter(term %in% c("library", "community center", "city hall", "place of worship",
                     "park", "fountain", "square", "garden", "bookstore", "cafe")) %>%
  select(term, place_id, name, geometry, business_status, types, user_ratings_total, geometry) %>%
  # Convert to same format as others
  saveRDS("boston/sites.rds")

get_results(name = "boston", test = FALSE) # Obtain grid
get_map(name = "boston")
get_grid_1km(name = "boston")
```





# 2. PARAMETERS #########################################
name = get_folder("boston") # Get Folder-Augmented Name (eg. search/nyc)


state = ""
county = ""
csub = ""


# GATHER NECESSARY DATA ################################
get_data(name = name, state = state, county = county, csub = csub)
remove(state, county, csub)

get_bounds(name = name) # Obtain relevant boudnaries for study area
get_grid(name = name) # Obtain grid

# RUN 5 TEST SEARCHES ###################################
get_testapi(name = name, parallel = TRUE)
get_file(name, "test_sites") %>% read_rds() # it works!
get_results(name = name, test = TRUE)
get_map(name = name, test = TRUE)

# RUN ACTUAL SEARCHES ####################################
get_api(name = name, parallel = TRUE)
get_results(name = name, test = FALSE)
get_map(name = name, test = FALSE)
get_diagnostics(name = name)
get_grid_1km(name = name)



# END ####################################################
print(paste0('---done: ', name)); rm(list = ls()); gc() # write message and clear


