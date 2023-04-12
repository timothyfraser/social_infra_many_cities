#' @name san_jose/query.R
#' @title API queries for: san_jose
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
# Download city limits manually from here
#"https://gisdata-csj.opendata.arcgis.com/datasets/city-limits/explore"

read_sf("san_jose/City_Limits.geojson") %>%
  st_transform(crs = wgs) %>%
  select(geometry) %>%
  st_make_valid() %>%
  sfheaders::sf_remove_holes() %>%
  st_write("san_jose/bounds.geojson", delete_dsn = TRUE)
```


First, let's confirm WHERE we are going to be gathering data from.

```{r}
# Houston is almost entirely located within Harris County; with tiny bits in others
test <- tigris::county_subdivisions(
  state = "CA",
  county = c("Santa Clara County"),
  cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geometry) %>%
  st_transform(crs = wgs)

myarea <- test %>% filter(name == "San Jose")

mybox <- myarea %>% st_bbox() %>% c() %>% sort()

mybounds <- read_sf("san_jose/bounds.geojson")

ggplot() +
  geom_sf(data = test, color = "grey") +
  geom_sf(data = myarea, fill = "red") +
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

```{r, eval = FALSE}
# Gather all necessary data
get_data(name = "san_jose", state = "CA",
         county = c("Santa Clara County"),
         csub = c("Diablo Range", "South Santa Clara Valley",
                  "San Jose", "Llagas-Uvas", "Lexington Hills"))
# Use city-provided bounds from above
#get_bounds(name = "san_jose") # Obtain relevant boundaries for study area
get_grid(name = "san_jose") # Obtain grid

# Run 5 Test Searches
get_testapi(name = "san_jose", yourkey = mykey, parallel = TRUE)
read_rds("san_jose/test_sites.rds") # It works!
get_results(name = "san_jose", test = TRUE) # Refine results
get_map(name = "san_jose", test = TRUE)

# Run actual searches
get_api(name = "san_jose", yourkey = mykey, parallel = TRUE) # Run Searches


get_results(name = "san_jose", test = FALSE) # Refine results
get_map(name = "san_jose", test = FALSE)
get_diagnostics(name = "san_jose")
get_grid_1km(name = "san_jose")

```



# 2. PARAMETERS #########################################
name = get_folder("san_jose") # Get Folder-Augmented Name (eg. search/nyc)


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


