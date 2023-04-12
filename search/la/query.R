#' @name query.R
#' @title API queries
#' @description A code to gather and process API data.

# Load functions and core values
data("functions")
data("aea")
data("wgs")
data("colors")

# Load keys
source("keys.R")


# First, let's confirm WHERE we are going to be gathering data from.

test <- tigris::county_subdivisions(state = "CA", county = "Los Angeles", cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geometry)

mybox <- test %>% filter(name == "Los Angeles") %>% st_bbox() %>% c() %>% sort()

#test$area %>% sum() * 0.25

ggplot() +
  geom_sf(data = test, color = "grey") +
  geom_sf(data = test %>% filter(name == "Los Angeles"), fill = "red") +
  geom_sf_text(data = test, mapping = aes(label = name)) +
  theme_void() +
  coord_sf(xlim = c(mybox[c("xmin", "xmax")] ),
           ylim = c(mybox[c("ymin", "ymax")] ))

remove(test, mybox)


# Gather all necessary data
name = get_folder("la")

# Gather all necessary data
get_data(name = name, state = "CA",
         county = c("Los Angeles"),
         csub = c("Los Angeles"))
get_bounds(name = name) # Obtain relevant boundaries for study area
get_grid(name = name) # Obtain grid

# Run 5 Test Searches
get_testapi(name = name, yourkey = mykey, parallel = TRUE, mycell = 274)
get_file(name, "test_sites") %>% read_rds() # it works!
get_results(name = name, test = TRUE) # Refine results
get_map(name = name, test = TRUE)


# Run actual searches
get_api(name = name, yourkey = mykey, parallel = TRUE) # Run Searches
get_results(name = name, test = FALSE) # Refine results
get_map(name = name, test = FALSE)
get_diagnostics(name = name)
get_grid_1km(name = name)
