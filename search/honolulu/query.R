#' @name honolulu/query.R
#' @title API queries for: honolulu
#' @description A code to gather and process API data.

# 0. SETUP: Load functions and core values ####################
data('functions')
data('aea')
data('wgs')
data('colors')

# Load keys
source('keys.R')


# 1. INITIAL TESTING (Optional) ########################

Let's try to use OSMData to quickly access city limit boundaries.

```{r}
dir.create("honolulu")

library(osmdata)



test <- tigris::county_subdivisions(
  state = "HI",
#  county = c("St. Louis"),
  cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geoid = GEOID, geometry) %>%
  st_transform(crs = wgs) %>%
  filter(name == "Honolulu")

test %>%
  st_transform(crs = wgs) %>%
  st_make_valid() %>%
  summarize(geometry = st_union(geometry)) %>%
  st_make_valid() %>%
  sfheaders::sf_remove_holes() %>%
  st_write("honolulu/bounds.geojson", delete_dsn = TRUE)

# Import city bounds
mybounds <- read_sf("honolulu/bounds.geojson")

mybox <- test %>% st_bbox() %>% c() %>% sort()


ggplot() +
  geom_sf(data = test, color = "grey") +
#  geom_sf(data = myarea, fill = "red") +
  geom_sf_text(data = test, mapping = aes(label = name)) +
  geom_sf(data = test %>%
            st_join(mybounds, left = FALSE),
          mapping = aes(fill = name)) +
  geom_sf(data = mybounds, size = 3, fill = NA,
          mapping = aes(color = "City Limits")) +
  scale_color_manual(values = "black") +
  theme_void() +
  coord_sf(xlim = c(mybox[c("xmin", "xmax")] ),
           ylim = c(mybox[c("ymin", "ymax")] ))

# Get names of all overlapping county subdivisions
mynames <- test %>%
  st_join(mybounds, left = FALSE) %>%
  with(name)

remove(test, mybounds, myarea, mybox)
```


Now, let's run the algorithms!

  ```{r, eval = FALSE}
load("functions.RData")


# Gather all necessary data
get_data(name = "honolulu", state = "HI",
         county = "Honolulu County",
         csub =  c(mynames))
# Use city-provided bounds from above
#get_bounds(name = "honolulu") # Obtain relevant boundaries for study area
get_grid(name = "honolulu") # Obtain grid

read_sf("honolulu/grid.geojson") %>% st_area() %>% sum() / 1e6

# Run 5 Test Searches
get_testapi(name = "honolulu", yourkey = mykey, parallel = TRUE)
read_rds("honolulu/test_sites.rds") # It works!
get_results(name = "honolulu", test = TRUE) # Refine results
get_map(name = "honolulu", test = TRUE)


# Run actual searches
get_api(name = "honolulu", yourkey = mykey, parallel = TRUE) # Run Searches


get_results(name = "honolulu", test = FALSE) # Refine results
get_map(name = "honolulu", test = FALSE)
get_diagnostics(name = "honolulu")
get_grid_1km(name = "honolulu")

read_sf("honolulu/grid.geojson") %>% st_area() %>% sum() / 1e6
```




# 2. PARAMETERS #########################################
name = get_folder("honolulu") # Get Folder-Augmented Name (eg. search/nyc)


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


