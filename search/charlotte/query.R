#' @name charlotte/query.R
#' @title API queries for: charlotte
#' @description A code to gather and process API data.

# 0. SETUP: Load functions and core values ####################
data('functions')
data('aea')
data('wgs')
data('colors')

# Load keys
source('keys.R')


# 1. INITIAL TESTING (Optional) ########################


#### 16. Charlotte


```{r, eval = FALSE}
#https://data.indy.gov/datasets/indy-neighborhoods/explore
dir.create("charlotte")

# Download zipfile from here
#"http://maps.co.mecklenburg.nc.us/opendata/Jurisdictions.zip"
#http://maps.co.mecklenburg.nc.us/openmapping/data.html?search=city

read_sf("charlotte/Jurisdictions.shp") %>%
  st_transform(crs = wgs)  %>%
  filter(name == "Charlotte") %>%
  summarize(geometry = st_union(geometry)) %>%
  st_make_valid() %>%
  sfheaders::sf_remove_holes() %>%
  st_write("charlotte/bounds.geojson", delete_dsn = TRUE)
```


```{r, eval = FALSE}
dir.create("charlotte")

test <- tigris::county_subdivisions(
  state = "NC",
  county = c("Mecklenburg County"),
  cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geometry) %>%
  st_transform(crs = wgs)

mybounds <- read_sf("charlotte/bounds.geojson")

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
get_data(name = "charlotte", state = "NC",
         county = c("Mecklenburg County"),
         csub =  c("1, Charlotte", "11, Long Creek", "12, Paw Creek",
                     "13, Morning Star", "14, Pineville", "2, Berryhill",
                     "3, Steele Creek", "5, Providence", "6, Clear Creek",
                     "7, Crab Orchard", "8, Mallard Creek"))

# Use city-provided bounds from above
#get_bounds(name = "charlotte") # Obtain relevant boundaries for study area
get_grid(name = "charlotte") # Obtain grid


# Run 5 Test Searches
get_testapi(name = "charlotte", yourkey = mykey, parallel = TRUE)
read_rds("charlotte/test_sites.rds") # It works!
get_results(name = "charlotte", test = TRUE) # Refine results
get_map(name = "charlotte", test = TRUE)

# Run actual searches
get_api(name = "charlotte", yourkey = mykey, parallel = TRUE) # Run Searches


get_results(name = "charlotte", test = FALSE) # Refine results
get_map(name = "charlotte", test = FALSE)
get_diagnostics(name = "charlotte")
get_grid_1km(name = "charlotte")

#read_sf("charlotte/grid.geojson") %>% st_area() %>% sum() / 1e6
```



# 2. PARAMETERS #########################################
name = get_folder("charlotte") # Get Folder-Augmented Name (eg. search/nyc)


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


