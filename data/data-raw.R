#' @name data-raw.R
#'
#' @description Script for producing data to be included as accessible with data()

# Get North American Albers Equal Area Conic Projection
# https://spatialreference.org/ref/esri/north-america-albers-equal-area-conic/
aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
save(aea, file = "data/aea.rda")

# Get EPSG:4326 (WGS 84) projection
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
save(wgs, file = "data/wgs.rda")

# Get Colors, with Names
colors = setNames(
  object = c("#0D0887", "#6A00A8", "#E16462" ,"#FCA636"),
  nm =  c("Community Space", "Place of Worship",
                 "Social Business", "Park"))
save(colors, file = "data/colors.rda")


# List out all main functions
f = c("get_api", "get_bounds", "get_data", "get_data", "get_diagnostics",
  "get_grid", "get_grid_1km", "get_map", "get_results", "get_testapi")
# Load all main functions
for(i in f){ source(paste0("R/",i,".R")) }
# Save all main functions, to be accessible with data("functions")
save(list = f, file = "data/functions.rda")

# Clear environment and cache
rm(list = ls()); gc()


