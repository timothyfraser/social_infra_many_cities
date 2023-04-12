#' @name san_antonio/query.R
#' @title API queries for: san_antonio
#' @description A code to gather and process API data.

# 0. SETUP: Load functions and core values ####################
data('functions')
data('aea')
data('wgs')
data('colors')

# Load keys
source('keys.R')


# 1. INITIAL TESTING (Optional) ########################



These are the San Antonio City Limits, downloaded from their website.

```{r, eval = FALSE, message=FALSE, warning = FALSE}
myfile <- tempfile()
myfolder <- tempfile()

dir.create("san_antonio")

"https://gis.sanantonio.gov/Download/CoSABoundary.zip" %>%
  download_html(file = myfile) %>%
  unzip(exdir = myfolder)

read_sf(paste(myfolder, "/", "CosaBoundary/CosaBoundary.shp", sep = "")) %>%
  st_as_sf() %>%
  st_transform(crs = wgs) %>%
  select(geometry) %>%
  st_make_valid() %>%
  # Finally, San Antonio has a few holes in it;
  # we'll fill in those holes to get social infrastructure anywhere within the limits.
  sfheaders::sf_remove_holes() %>%
  st_write("san_antonio/bounds.geojson", delete_dsn = TRUE)

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
  county = c("Bexar County"),
  cb = TRUE, year = 2019) %>%
  st_as_sf() %>%
  mutate(area = st_area(geometry) %>% as.numeric(),
         area = area / 1000000) %>%
  select(name = NAME, area, geometry)
test$name
myarea <- test %>% filter(str_detect(name, "San Antonio"))
mybox <- myarea %>% st_bbox() %>% c() %>% sort()

mybounds <- read_sf("san_antonio/bounds.geojson")

ggplot() +
  geom_sf(data = test, color = "grey") +
  geom_sf(data = myarea, fill = "red") +
  geom_sf_text(data = test, mapping = aes(label = name)) +
  geom_sf(data = mybounds, size = 3, fill = NA,
          mapping = aes(color = "City Limits")) +
  scale_color_manual(values = "black") +
  theme_void() +
  coord_sf(xlim = c(mybox[c("xmin", "xmax")] ),
           ylim = c(mybox[c("ymin", "ymax")] ))


remove(test, mybox, myarea)
```

```{r, eval = FALSE}
# Gather all necessary data
get_data(name = "san_antonio", state = "TX",
         county = c("Bexar County"),
         csub = c("San Antonio East",
                  "San Antonio North",
                  "San Antonio Northwest",
                  "San Antonio Central",
                  "San Antonio South",
                  "San Antonio West",
                  "San Antonio Northeast"))
# We're skipping this step and using the official, custom boundaries
# since they don't easily map onto census subdivisions
#get_bounds(name = "XXXX") # Obtain relevant boundaries for study area
get_grid(name = "san_antonio") # Obtain grid

# Run 5 Test Searches
get_testapi(name = "san_antonio", yourkey = mykey, parallel = TRUE)
read_rds("san_antonio/test_sites.rds") # It works!
get_results(name = "san_antonio", test = TRUE) # Refine results
get_map(name = "san_antonio", test = TRUE)

# Run actual searches
get_api(name = "san_antonio", yourkey = mykey, parallel = TRUE) # Run Searches


get_results(name = "san_antonio", test = FALSE) # Refine results
get_map(name = "san_antonio", test = FALSE)
get_diagnostics(name = "san_antonio")
get_grid_1km(name = "san_antonio")

```


# 2. PARAMETERS #########################################
name = get_folder("san_antonio") # Get Folder-Augmented Name (eg. search/nyc)


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


