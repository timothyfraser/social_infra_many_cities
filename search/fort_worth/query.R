#' @name fort_worth/query.R
#' @title API queries for: fort_worth
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
dir.create("fort_worth")
# Access the Texas Department of Transportation's database of city limits
# https://gis-txdot.opendata.arcgis.com/datasets/09cd5b6811c54857bd3856b5549e34f0_0/explore?location=30.273126%2C-97.659878%2C10.40
dat <- read_sf("https://opendata.arcgis.com/datasets/09cd5b6811c54857bd3856b5549e34f0_0.geojson") %>%
  filter(CITY_NM == "Fort Worth") %>%
  select(geometry) %>%
  st_transform(crs = wgs) %>%
  select(geometry) %>%
  st_make_valid() %>%
  sfheaders::sf_remove_holes() %>%
  st_write("fort_worth/bounds.geojson", delete_dsn = TRUE)
```


First, let's confirm WHERE we are going to be gathering data from.

```{r, eval = FALSE}
# Houston is almost entirely located within Harris County; with tiny bits in others
test <- tigris::county_subdivisions(
  state = "TX",
  county = c("Tarrant County", "Denton County", "Parker County",
             "Wise County", "Johnson County"),
  cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geometry) %>%
  st_transform(crs = wgs)

mybounds <- read_sf("fort_worth/bounds.geojson")

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
get_data(name = "fort_worth", state = "TX",
         county = c("Tarrant County", "Denton County", "Parker County",
                    "Wise County", "Johnson County"),
         csub = c("Arlington", "Boyd-Rhome", "Burleson-Joshua", "Fort Worth",
                  "Justin-Roanoke", "Northeast Tarrant", "Weatherford Southeast"))

# Use city-provided bounds from above
#get_bounds(name = "fort_worth") # Obtain relevant boundaries for study area
get_grid(name = "fort_worth") # Obtain grid

# Run 5 Test Searches
get_testapi(name = "fort_worth", yourkey = mykey, parallel = TRUE)
read_rds("fort_worth/test_sites.rds") # It works!
get_results(name = "fort_worth", test = TRUE) # Refine results
get_map(name = "fort_worth", test = TRUE)

# Run actual searches
get_api(name = "fort_worth", yourkey = mykey, parallel = TRUE) # Run Searches


get_results(name = "fort_worth", test = FALSE) # Refine results
get_map(name = "fort_worth", test = FALSE)
get_diagnostics(name = "fort_worth")
get_grid_1km(name = "fort_worth")

#read_sf("fort_worth/grid1km.geojson") %>% st_area() %>% sum() / 1e6
```



# 2. PARAMETERS #########################################
name = get_folder("fort_worth") # Get Folder-Augmented Name (eg. search/nyc)


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


