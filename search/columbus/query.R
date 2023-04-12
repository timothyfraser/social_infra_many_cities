#' @name columbus/query.R
#' @title API queries for: columbus
#' @description A code to gather and process API data.

# 0. SETUP: Load functions and core values ####################
data('functions')
data('aea')
data('wgs')
data('colors')

# Load keys
source('keys.R')


# 1. INITIAL TESTING (Optional) ########################


```{r}
# Columbus
dir.create("columbus")
tigris::county_subdivisions(
  state = "OH",
  county = c("Franklin County", "Delaware County", "Fairfield County"),
  cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geometry) %>%
  st_transform(crs = wgs) %>%
  # Filter to the 3 divisions containing columbus
  filter(str_detect(name, "Columbus")) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_transform(crs = wgs) %>%
  st_make_valid() %>%
  sfheaders::sf_remove_holes() %>%
  st_write("columbus/bounds.geojson", delete_dsn = TRUE)
```

```{r, eval = FALSE}
# Houston is almost entirely located within Harris County; with tiny bits in others
test <- tigris::county_subdivisions(
  state = "OH",
  county = c("Franklin County", "Delaware County", "Fairfield County"),
  cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geometry) %>%
  st_transform(crs = wgs) %>%
  # Filter to the 3 divisions containing columbus
  filter(str_detect(name, "Columbus"))

mybounds <- read_sf("columbus/bounds.geojson")

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
```



Now, let's run the algorithms!

```{r, eval = FALSE}
load("functions.RData")

# Gather all necessary data
get_data(name = "columbus", state = "OH",
         county = c("Franklin County", "Delaware County", "Fairfield County"),
         csub =  c("Columbus", "Columbus City"))

# Use city-provided bounds from above
#get_bounds(name = "columbus") # Obtain relevant boundaries for study area
get_grid(name = "columbus") # Obtain grid


# Run 5 Test Searches
get_testapi(name = "columbus", yourkey = mykey, parallel = TRUE)
read_rds("columbus/test_sites.rds") # It works!
get_results(name = "columbus", test = TRUE) # Refine results
get_map(name = "columbus", test = TRUE)

# Run actual searches
get_api(name = "columbus", yourkey = mykey, parallel = TRUE) # Run Searches


get_results(name = "columbus", test = FALSE) # Refine results
get_map(name = "columbus", test = FALSE)
get_diagnostics(name = "columbus")
get_grid_1km(name = "columbus")

#read_sf("columbus/grid.geojson") %>% st_area() %>% sum() / 1e6
```



# 2. PARAMETERS #########################################
name = get_folder("columbus") # Get Folder-Augmented Name (eg. search/nyc)


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


