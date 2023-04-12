#' @name san_francisco/query.R
#' @title API queries for: san_francisco
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

```{r, eval = FALSE}
dir.create("san_francisco")

library(osmdata)

mybox <- getbb(place_name = "San Francisco, California")

# Borrowing from my guide at the following link:
# https://rpubs.com/timothyfraser/osmdata

# Import city bounds
mybounds <- opq(mybox) %>%
  add_osm_feature(key = 'admin_level',
                  value = "6") %>%
  osmdata_sf() %>%
  with(osm_multipolygons) %>%
  filter(name == "San Francisco") %>%
  st_transform(crs = wgs)  %>%
  summarize(geometry = st_union(geometry)) %>%
  st_make_valid() %>%
  sfheaders::sf_remove_holes()


# GEt TIGRIS generated bounds instead
test <- tigris::county_subdivisions(
  state = "CA",
  county = c("San Francisco County"),
  cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geometry) %>%
  st_transform(crs = wgs)

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

remove(test, mybounds, myarea, mybox)
# Yup, we can conclude that the county boundary is better here


```


Now, let's run the algorithms!

  ```{r, eval = FALSE}
load("functions.RData")

# Gather all necessary data
get_data(name = "san_francisco", state = "CA",
         county = c("San Francisco County"),
         csub =  c("San Francisco"))

# Use city-provided bounds from above
get_bounds(name = "san_francisco") # Obtain relevant boundaries for study area
get_grid(name = "san_francisco") # Obtain grid


# Run 5 Test Searches
get_testapi(name = "san_francisco", yourkey = mykey, parallel = TRUE)
read_rds("san_francisco/test_sites.rds") # It works!
get_results(name = "san_francisco", test = TRUE) # Refine results
get_map(name = "san_francisco", test = TRUE)

# Run actual searches
get_api(name = "san_francisco", yourkey = mykey, parallel = TRUE) # Run Searches


get_results(name = "san_francisco", test = FALSE) # Refine results
get_map(name = "san_francisco", test = FALSE)
get_diagnostics(name = "san_francisco")
get_grid_1km(name = "san_francisco")

#read_sf("san_francisco/grid.geojson") %>% st_area() %>% sum() / 1e6
```



# 2. PARAMETERS #########################################
name = get_folder("san_francisco") # Get Folder-Augmented Name (eg. search/nyc)


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


