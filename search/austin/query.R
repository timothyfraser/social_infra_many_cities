#' @name austin/query.R
#' @title API queries for: austin
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
dir.create("austin")
# Download city limits manually from here

# Access the Texas Department of Transportation's database of city limits
# https://gis-txdot.opendata.arcgis.com/datasets/09cd5b6811c54857bd3856b5549e34f0_0/explore?location=30.273126%2C-97.659878%2C10.40
read_sf("https://opendata.arcgis.com/datasets/09cd5b6811c54857bd3856b5549e34f0_0.geojson") %>%
  filter(CITY_NM == "Austin") %>%
  select(geometry) %>%
  st_transform(crs = wgs) %>%
  select(geometry) %>%
  st_make_valid() %>%
  sfheaders::sf_remove_holes() %>%
  st_write("austin/bounds.geojson", delete_dsn = TRUE)
```

First, let's confirm WHERE we are going to be gathering data from.

```{r, eval = FALSE}
# Houston is almost entirely located within Harris County; with tiny bits in others
test <- tigris::county_subdivisions(
  state = "TX",
  county = c("Travis County", "Hays County", "Williamson County"),
  cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geometry) %>%
  st_transform(crs = wgs)

mybounds <- read_sf("austin/bounds.geojson")

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
get_data(name = "austin", state = "TX",
         county = c("Travis County", "Hays County", "Williamson County"),
         csub = c("Austin", "Cedar Park-Liberty Hill", "Kyle-Buda", "Northeast Travis",
                  "Northwest Travis", "Round Rock-Georgetown", "Southwest Travis"))

# Use city-provided bounds from above
#get_bounds(name = "austin") # Obtain relevant boundaries for study area
get_grid(name = "austin") # Obtain grid

# Run 5 Test Searches
get_testapi(name = "austin", yourkey = mykey, parallel = TRUE)
read_rds("austin/test_sites.rds") # It works!
get_results(name = "austin", test = TRUE) # Refine results
get_map(name = "austin", test = TRUE)

# Run actual searches
get_api(name = "austin", yourkey = mykey, parallel = TRUE) # Run Searches


get_results(name = "austin", test = FALSE) # Refine results
get_map(name = "austin", test = FALSE)
get_diagnostics(name = "austin")
get_grid_1km(name = "austin")
```


# 2. PARAMETERS #########################################
name = get_folder("austin") # Get Folder-Augmented Name (eg. search/nyc)


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


