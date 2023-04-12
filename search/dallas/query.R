#' @name dallas/query.R
#' @title API queries for: dallas
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


These are the San Antonio City Limits, downloaded from their website.

```{r, eval = FALSE, message=FALSE, warning = FALSE}
myfile <- tempfile()
myfolder <- tempfile()

dir.create("dallas")

"https://gis.dallascityhall.com/Downloads/ShpZip/Citylimit.zip" %>%
  download_html(file = myfile) %>%
  unzip(exdir = myfolder)

read_sf(paste(myfolder, "/", "CityLimit.shp", sep = "")) %>%
  st_as_sf() %>%
  st_transform(crs = wgs) %>%
  select(geometry) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_make_valid() %>%
  # Finally, city  has a few holes in it;
  # we'll fill in those holes to get social infrastructure anywhere within the limits.
sfheaders::sf_remove_holes() %>%
  st_write("dallas/bounds.geojson", delete_dsn = TRUE)

# Remove the temp files
file.remove(myfile)
unlink(myfolder, recursive = TRUE)
# Area the temp files gone?
file.exists(myfile)
file.exists(myfolder)
# Good!

remove(myfile, myfolder)
```
First, let's confirm WHERE we are going to be gathering data from.

```{r}

# Houston is almost entirely located within Harris County; with tiny bits in others
test <- tigris::county_subdivisions(
  state = "TX",
  county = c("Dallas County", "Collin County", "Denton County",
             "Rockwall County", "Kaufman County"),
  cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geometry) %>%
  st_transform(crs = wgs)

myarea <- test %>% filter(str_detect(name, "Dallas"))
mybox <- myarea %>% st_bbox() %>% c() %>% sort()

mybounds <- read_sf("dallas/bounds.geojson")

ggplot() +
  geom_sf(data = test, color = "grey") +
  geom_sf(data = myarea, fill = "red") +
  geom_sf_text(data = test, mapping = aes(label = name)) +
  geom_sf(data = test %>% st_join(mybounds, left = FALSE), size = 3,
          mapping = aes(fill = name)) +
  geom_sf(data = mybounds, size = 3, fill = NA,
          mapping = aes(color = "City Limits")) +
  scale_color_manual(values = "black") +
  theme_void() +
  coord_sf(xlim = c(mybox[c("xmin", "xmax")] ),
           ylim = c(mybox[c("ymin", "ymax")] ))


remove(test, mybox, myarea, mybounds)
```

```{r, eval = FALSE}
# Gather all necessary data
get_data(name = "dallas", state = "TX",
         county = c("Dallas County", "Collin County", "Denton County",
             "Rockwall County", "Kaufman County"),
         csub = c("Carrollton", "Forney", "Nevada", "Northeast Dallas", "Northwest Rockwall", "Plano", "South Rockwall", "Southwest Dallas"))
# Disregard - use the city-provided limits instead
#get_bounds(name = "dallas") # Obtain relevant boundaries for study area
get_grid(name = "dallas") # Obtain grid

# Run 5 Test Searches
get_testapi(name = "dallas", yourkey = mykey, parallel = TRUE)
read_rds("dallas/test_sites.rds") # It works!
get_results(name = "dallas", test = TRUE) # Refine results
get_map(name = "dallas", test = TRUE)

# Run actual searches
get_api(name = "dallas", yourkey = mykey, parallel = TRUE) # Run Searches


get_results(name = "dallas", test = FALSE) # Refine results
get_map(name = "dallas", test = FALSE)
get_diagnostics(name = "dallas")
get_grid_1km(name = "dallas")

```


# 2. PARAMETERS #########################################
name = get_folder("dallas") # Get Folder-Augmented Name (eg. search/nyc)


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


