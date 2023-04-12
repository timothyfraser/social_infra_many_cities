#' @name indianapolis/query.R
#' @title API queries for: indianapolis
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
#https://data.indy.gov/datasets/indy-neighborhoods/explore
dir.create("indianapolis")
read_sf("https://opendata.arcgis.com/datasets/d5b4734bed3f49f9b2109ca9a0401886_10.geojson") %>%
  st_transform(crs = wgs)  %>%
  summarize(geometry = st_union(geometry)) %>%
  st_make_valid() %>%
  sfheaders::sf_remove_holes() %>%
  st_write("indianapolis/bounds.geojson", delete_dsn = TRUE)

```


```{r, eval = FALSE}
# Houston is almost entirely located within Harris County; with tiny bits in others
test <- tigris::county_subdivisions(
  state = "IN",
  county = c("Marion County"),
  cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geometry) %>%
  st_transform(crs = wgs)

mybounds <- read_sf("indianapolis/bounds.geojson")

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
get_data(name = "indianapolis", state = "IN",
         county = c("Marion County"),
         csub =  c("Warren", "Franklin", "Center", "Decatur", "Lawrence",
                   "Perry", "Pike", "Wayne", "Washington"))

# Use city-provided bounds from above
#get_bounds(name = "indianapolis") # Obtain relevant boundaries for study area
get_grid(name = "indianapolis") # Obtain grid


# Run 5 Test Searches
get_testapi(name = "indianapolis", yourkey = mykey, parallel = TRUE)
read_rds("indianapolis/test_sites.rds") # It works!
get_results(name = "indianapolis", test = TRUE) # Refine results
get_map(name = "indianapolis", test = TRUE)

# Run actual searches
get_api(name = "indianapolis", yourkey = mykey, parallel = TRUE) # Run Searches


get_results(name = "indianapolis", test = FALSE) # Refine results
get_map(name = "indianapolis", test = FALSE)
get_diagnostics(name = "indianapolis")
get_grid_1km(name = "indianapolis")

#read_sf("indianapolis/grid.geojson") %>% st_area() %>% sum() / 1e6
```



# 2. PARAMETERS #########################################
name = get_folder("indianapolis") # Get Folder-Augmented Name (eg. search/nyc)


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


