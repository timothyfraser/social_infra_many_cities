#' @name atlanta/query.R
#' @title API queries for: atlanta
#' @description A code to gather and process API data.

# 0. SETUP: Load functions and core values ####################
data('functions')
data('aea')
data('wgs')
data('colors')

# Load keys
source('keys.R')


# 1. INITIAL TESTING (Optional) ########################
library(osmdata)
library(sfheaders)

mybox <- getbb(place_name = "Atlanta, GA")


# Borrowing from my guide at the following link:
# https://rpubs.com/timothyfraser/osmdata

mybounds <- opq(mybox) %>%
  add_osm_feature(key = 'admin_level',
                  value = "8") %>%
  osmdata_sf()

mybounds$osm_multipolygons %>%
  filter(name == "Atlanta")  %>%
  st_transform(crs = wgs) %>%
  st_make_valid() %>%
  summarize(geometry = st_union(geometry)) %>%
  st_make_valid() %>%
  sfheaders::sf_remove_holes() %>%
  st_write("atlanta/bounds.geojson", delete_dsn = TRUE)


# Import city bounds
mybounds <- read_sf("atlanta/bounds.geojson")

test <- tigris::county_subdivisions(
  state = "GA",
  county = c("Fulton County", "DeKalb County"),
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


# 2. PARAMETERS #########################################
name = get_folder("atlanta") # Get Folder-Augmented Name (eg. search/nyc)


state = "GA"
county = c("Fulton County", "DeKalb County")
csub =  c("Atlanta", "Atlanta-Decatur", "Chamblee-Doraville", "East Point")

# GATHER NECESSARY DATA ################################
get_data(name = name, state = state, county = county, csub = csub)
remove(state, county, csub)
# Use bounds from above
# get_bounds(name = name) # Obtain relevant boudnaries for study area
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


