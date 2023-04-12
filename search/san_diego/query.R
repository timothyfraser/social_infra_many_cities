#' @name san_diego/query.R
#' @title API queries for: san_diego
#' @description A code to gather and process API data.

# 0. SETUP: Load functions and core values ####################
data('functions')
data('aea')
data('wgs')
data('colors')

# Load keys
source('keys.R')


# 1. INITIAL TESTING (Optional) ########################


First, let's confirm WHERE we are going to be gathering data from.

Download City Boundaries from San Diego city government website.

```{r}
dir.create("san_diego")

read_sf("https://seshat.datasd.org/sde/city_boundary/san_diego_boundary_datasd.geojson") %>%
  st_as_sf() %>%
  st_transform(crs = wgs) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_make_valid() %>%
  sfheaders::sf_remove_holes() %>%
  st_write("san_diego/bounds.geojson", delete_dsn = TRUE)

```

```{r}
# Houston is almost entirely located within Harris County; with tiny bits in others
test <- tigris::county_subdivisions(
  state = "CA",
  county = c("San Diego County"),
  cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geometry)

myarea <- test %>% filter(name == "San Diego")

mybox <- myarea %>% st_bbox() %>% c() %>% sort()

mybounds <- read_sf("san_diego/bounds.geojson")

ggplot() +
  geom_sf(data = test, color = "grey") +
  geom_sf(data = myarea, fill = "red") +
  geom_sf_text(data = test, mapping = aes(label = name)) +
  geom_sf(data = test, mapping = aes(fill = name),
          size = 1) +
  geom_sf(data = mybounds, mapping = aes(color = "City Limits"), size = 3, fill = NA) +
  scale_color_manual(values = "black") +
  theme_void() +
  coord_sf(xlim = c(mybox[c("xmin", "xmax")] ),
           ylim = c(mybox[c("ymin", "ymax")] ))

# overlaps just a bit with Oceanside-Escondido
remove(test, mybox, myarea, mybounds)
```


```{r, eval = FALSE}
# Gather all necessary data
get_data(name = "san_diego", state = "CA",
         county = c("San Diego County"),
         csub = c("San Diego", "Oceanside-Escondido"))
# We're skipping this step and using the official, custom boundaries
# since they don't easily map onto census subdivisions
#get_bounds(name = "XXXX") # Obtain relevant boundaries for study area
get_grid(name = "san_diego") # Obtain grid

# Run 5 Test Searches
get_testapi(name = "san_diego", yourkey = mykey, parallel = TRUE)
read_rds("san_diego/test_sites.rds") # It works!
get_results(name = "san_diego", test = TRUE) # Refine results
get_map(name = "san_diego", test = TRUE)

# Run actual searches
get_api(name = "san_diego", yourkey = mykey, parallel = TRUE) # Run Searches


get_results(name = "san_diego", test = FALSE) # Refine results
get_map(name = "san_diego", test = FALSE)
get_diagnostics(name = "san_diego")
get_grid_1km(name = "san_diego")

```



# 2. PARAMETERS #########################################
name = get_folder("san_diego") # Get Folder-Augmented Name (eg. search/nyc)


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


