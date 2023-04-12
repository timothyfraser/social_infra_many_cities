#' @name worcester/query.R
#' @title API queries for: worcester
#' @description A code to gather and process API data.

# 0. SETUP: Load functions and core values ####################
data('functions')
data('aea')
data('wgs')
data('colors')

# Load keys
source('keys.R')


# 1. INITIAL TESTING (Optional) ########################

Let's also gather data for Worcester. This is obviously not a big city, but it's going to be a simple comparison case for Boston. So we turned to the official city limits as specified on their [website](https://gisdata.worcesterma.gov/datasets/city-boundary/explore?location=42.275550%2C-71.807750%2C12.43).

```{r, eval = FALSE}
read_sf("worcester/bounds.geojson") %>%
  select(geometry) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  st_write("worcester/bounds.geojson", delete_dsn = TRUE)
```

Let's briefly adjust the API key so that this smaller city gets charged to me, not our grant funders.

```{r, echo = FALSE}
library(tidyverse)
library(sf)
library(googleway)
library(future)
library(furrr)

# Let's load in your API key here
mykey <- "AIzaSyBz1EyyW19aqYp7ZVpuwGsZOiC2Q0o-L5U"
set_key(key = mykey, api = "places")
```

```{r, eval = FALSE}
library(tidyverse)
library(sf)
library(googleway)
library(future)
library(furrr)

# Let's load in your API key here
mykey <- "INSERT_MY_API_KEY_HERE"
set_key(key = mykey, api = "places")

# View which API Keys we currently have registered here
google_keys()
# If you need to use it, clear keys will clear those keys too.
#clear_keys() ## clear any previously set keys
```


```{r, eval = FALSE}
# Gather all necessary data
get_data(name = "worcester", state = "MA",
         county = c("Worcester"),
         csub = c("Worcester"))
#get_bounds(name = "phoenix") # Obtain relevant boundaries for study area
# Worcester is a special case: use these bounds instead
get_grid(name = "worcester") # Obtain grid

# Run 5 Test Searches
get_testapi(name = "worcester", yourkey = mykey, parallel = TRUE)
read_rds("worcester/test_sites.rds") # It works!
get_results(name = "worcester", test = TRUE) # Refine results
get_map(name = "worcester", test = TRUE)

# Run actual searches
get_api(name = "worcester", yourkey = mykey, parallel = TRUE) # Run Searches


get_results(name = "worcester", test = FALSE) # Refine results
get_map(name = "worcester", test = FALSE)
get_diagnostics(name = "worcester")
get_grid_1km(name = "worcester")

clear_keys()
```





# 2. PARAMETERS #########################################
name = get_folder("worcester") # Get Folder-Augmented Name (eg. search/nyc)


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


