#' @name seattle/query.R
#' @title API queries for: seattle
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
dir.create("seattle")

library(osmdata)

mybox <- getbb(place_name = "Seattle, Washington")

# Borrowing from my guide at the following link:
# https://rpubs.com/timothyfraser/osmdata

mybounds <- opq(mybox) %>%
  add_osm_feature(key = 'admin_level',
                  value = "8") %>%
  osmdata_sf() %>%
  with(osm_multipolygons)

mybounds %>%
  filter(name == "Seattle") %>%
  st_transform(crs = wgs)  %>%
  summarize(geometry = st_union(geometry)) %>%
  st_make_valid() %>%
  sfheaders::sf_remove_holes() %>%
  st_write("seattle/bounds.geojson", delete_dsn = TRUE)

# Import city bounds
mybounds <- read_sf("seattle/bounds.geojson")

test <- tigris::county_subdivisions(
  state = "WA",
  county = c("King County"),
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

# Update polygon boundaries
mybounds %>%
  # By zooming into just the intersection of the
  # Seattle OSM polygon (accurate)
  # and the TIGRIS polygon (doesn't include water, so better)
st_intersection(filter(test, name == "Seattle")) %>%
  # Giving us just actual Seattle boundaries, minus water
  select(geometry) %>%
  st_transform(crs = wgs)  %>%
  summarize(geometry = st_union(geometry)) %>%
  st_make_valid() %>%
  sfheaders::sf_remove_holes() %>%
  st_write("seattle/bounds.geojson", delete_dsn = TRUE)

remove(test, mybounds, myarea, mybox)
# Yup, we can conclude that the county boundary is better here
```


Now, let's run the algorithms!

```{r, eval = FALSE}
load("functions.RData")

# Gather all necessary data
get_data(name = "seattle", state = "WA",
         county = c("King County"),
         csub =  c("Seattle"))

# Use city-provided bounds from above
#get_bounds(name = "seattle") # Obtain relevant boundaries for study area
get_grid(name = "seattle") # Obtain grid


# Run 5 Test Searches
get_testapi(name = "seattle", yourkey = mykey, parallel = TRUE)
read_rds("seattle/test_sites.rds") # It works!
get_results(name = "seattle", test = TRUE) # Refine results
get_map(name = "seattle", test = TRUE)

# Run actual searches
get_api(name = "seattle", yourkey = mykey, parallel = TRUE) # Run Searches


get_results(name = "seattle", test = FALSE) # Refine results
get_map(name = "seattle", test = FALSE)
get_diagnostics(name = "seattle")
get_grid_1km(name = "seattle")

#read_sf("seattle/grid.geojson") %>% st_area() %>% sum() / 1e6
```



# 2. PARAMETERS #########################################
name = get_folder("seattle") # Get Folder-Augmented Name (eg. search/nyc)


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


