#' @name ithaca/query.R
#' @title API queries for: ithaca
#' @description A code to gather and process API data.

# 0. SETUP: Load functions and core values ####################
data('functions')
data('aea')
data('wgs')
data('colors')

# Load keys
source('keys.R')


# 1. INITIAL TESTING (Optional) ########################


First, let's confirm WHERE we are going to be gathering data from.

```{r, eval = FALSE}
# Houston is almost entirely located within Harris County; with tiny bits in others
test <- tigris::county_subdivisions(
  state = "NY",
  county = c("Tompkins County"),
  cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geometry)

myarea <- test %>%
  filter(name == "Ithaca")

# Save these bounds for the right area to file
dir.create("ithaca")
# Get polygon for greater ithaca area
myarea %>%
  select(geometry) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_transform(crs = wgs) %>%
  st_write("ithaca/bounds.geojson", delete_dsn = TRUE)

mybox <- myarea %>% st_bbox() %>% c() %>% sort()

#test$area %>% sum() * 0.25

ggplot() +
  geom_sf(data = test, color = "grey") +
  geom_sf(data = myarea, fill = "red") +
  geom_sf_text(data = test, mapping = aes(label = name)) +
  theme_void() +
  coord_sf(xlim = c(mybox[c("xmin", "xmax")] ),
           ylim = c(mybox[c("ymin", "ymax")] ))

remove(test, mybox, myarea)
```

```{r, eval = FALSE}
# Gather all necessary data
get_data(name = "ithaca", state = "NY",
         county = c("Tompkins County"),
         csub = c("Ithaca"))
#get_bounds(name = "ithaca") # Obtain relevant boundaries for study area
get_grid(name = "ithaca") # Obtain grid

# Run 5 Test Searches
get_testapi(name = "ithaca", yourkey = mykey, parallel = TRUE)
read_rds("ithaca/test_sites.rds") # It works!
get_results(name = "ithaca", test = TRUE) # Refine results
get_map(name = "ithaca", test = TRUE)

# Run actual searches
get_api(name = "ithaca", yourkey = mykey, parallel = TRUE) # Run Searches

#read_sf("ithaca/bounds.geojson") %>% st_area() / 1e6

get_results(name = "ithaca", test = FALSE) # Refine results
get_map(name = "ithaca", test = FALSE)
get_diagnostics(name = "ithaca")
get_grid_1km(name = "ithaca")

clear_keys()
```






# 2. PARAMETERS #########################################
name = get_folder("ithaca") # Get Folder-Augmented Name (eg. search/nyc)


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


