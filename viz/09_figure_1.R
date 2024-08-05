
# FIGURE 1 #####################################

library(dplyr)
library(readr)
library(ggplot2)
library(viridis)
library(sf)
library(ggpubr)

setwd(paste0(rstudioapi::getActiveProject()))
# read_csv("viz/table_descriptives.csv")

#cities = read_csv("viz/cities.csv")

# Get EPSG:4326 (WGS 84) projection
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"



get_viz = function(.name = "la"){
  # Get Sites
  sites <- read_csv(paste0("search/", .name, "/results.csv")) %>%
    st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
    st_transform(crs = wgs)
  # Get bounds
  bounds <- read_sf(paste0("search/", .name, "/bounds.geojson")) %>%
    mutate(area = as.numeric(st_area(.$geometry)) / 1000000,
           area = round(area, 0))

  # Get Grid
  grid <- read_sf(paste0("search/", .name, "/grid.geojson"))

  city_metadata = read_csv("viz/cities.csv") %>% filter(name == .name)

  gg <- ggplot() +
    geom_sf(data = bounds, color = "black", fill = "#373737") +
    geom_sf(data = grid, color = "grey", fill = NA) +
    #geom_sf_text(data = grid,
    #             mapping = aes(label = cell), color = "black", size = 1) +
    geom_sf(data = sites, mapping = aes(fill = type),
            shape = 21, color = "white", stroke = 0.2, size = 1.5) +
    scale_fill_manual(
      breaks = c("Community Space", "Place of Worship",
                 "Social Business", "Park"),
      values = c("#0D0887", "#6A00A8", "#E16462" ,"#FCA636"),
      guide = guide_legend(override.aes = list(size = 5))) +
    theme_void(base_size = 14) +
    theme(plot.title = element_text(hjust = 0, face = "bold", size = 16),
          plot.subtitle = element_text(hjust = 0, size = 14)) +
    labs(x = NULL, y = NULL, fill = "Type",
         title = paste0(city_metadata$name_label),
         subtitle = paste(
           city_metadata$pop_label, " residents", "\n",
           nrow(sites), " sites\n",
           bounds$area, " sq.km.",
           sep = ""))

  return(gg)
}


g1 = get_viz(.name = "nyc")
g2 = get_viz(.name = "la")
g3 = get_viz(.name = "chicago")
g4 = get_viz(.name = "houston")
g5 = get_viz(.name = "phoenix")

#install.packages("patchwork")
library(patchwork)
combo = g1 + g2 + g3 + g4 + g5 +
  plot_layout(guides = "collect") +
  patchwork::guide_area()
ggsave(combo, filename = "viz/figure_1_cities_5.png", dpi = 500, width = 8, height = 7)

browseURL("viz/figure_1_cities_5.png")




