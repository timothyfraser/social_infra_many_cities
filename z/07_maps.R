
```{r}
library(tidyverse)
library(sf)
library(ggpubr)


# Get EPSG:4326 (WGS 84) projection
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Get Sites
sites <- read_csv("nyc/results.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = wgs)
# Get bounds
bounds <- read_sf("nyc/bounds.geojson") %>%
  mutate(area = as.numeric(st_area(.$geometry)) / 1000000,
         area = round(area, 0))

# Get Grid
grid <- read_sf("nyc/grid.geojson")

g1 <- ggplot() +
  geom_sf(data = bounds, color = "black", fill = "#373737") +
  geom_sf(data = grid, color = "grey", fill = NA) +
  #geom_sf_text(data = grid,
  #             mapping = aes(label = cell), color = "black", size = 1) +
  geom_sf(data = sites, mapping = aes(fill = type),
          shape = 21, color = "white", stroke = 0.1, size = 1) +
  scale_fill_manual(
    breaks = c("Community Space", "Place of Worship",
               "Social Business", "Park"),
    values = c("#0D0887", "#6A00A8", "#E16462" ,"#FCA636"),
    guide = guide_legend(override.aes = list(size = 5))) +
  theme_void(base_size = 14) +
  labs(x = NULL, y = NULL, fill = "Type",
       subtitle = paste(
         "New York City\n",
         "8.6 milion\n",
         nrow(sites), " sites\n",
         " (", bounds$area, " sq.km.)",
         sep = ""))



# Get Sites
sites <- read_csv("la/results.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = wgs)
# Get bounds
bounds <- read_sf("la/bounds.geojson") %>%
  mutate(area = as.numeric(st_area(.$geometry)) / 1000000,
         area = round(area, 0))

# Get Grid
grid <- read_sf("la/grid.geojson")

g2 <- ggplot() +
  geom_sf(data = bounds, color = "black", fill = "#373737") +
  geom_sf(data = grid, color = "grey", fill = NA) +
  #geom_sf_text(data = grid,
  #             mapping = aes(label = cell), color = "black", size = 1) +
  geom_sf(data = sites, mapping = aes(fill = type),
          shape = 21, color = "white", stroke = 0.1, size = 1) +
  scale_fill_manual(
    breaks = c("Community Space", "Place of Worship",
               "Social Business", "Park"),
    values = c("#0D0887", "#6A00A8", "#E16462" ,"#FCA636"),
    guide = guide_legend(override.aes = list(size = 5))) +
  theme_void(base_size = 14) +
  labs(x = NULL, y = NULL, fill = "Type",
       subtitle = paste(
         "Los Angeles\n",
         "4.1 milion\n",
         nrow(sites), " sites\n",
         " (", bounds$area, " sq.km.)",
         sep = ""))



# Get Sites
sites <- read_csv("chicago/results.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = wgs)
# Get bounds
bounds <- read_sf("chicago/bounds.geojson") %>%
  mutate(area = as.numeric(st_area(.$geometry)) / 1000000,
         area = round(area, 0))

# Get Grid
grid <- read_sf("chicago/grid.geojson")

g3 <- ggplot() +
  geom_sf(data = bounds, color = "black", fill = "#373737") +
  geom_sf(data = grid, color = "grey", fill = NA) +
  #geom_sf_text(data = grid,
  #             mapping = aes(label = cell), color = "black", size = 1) +
  geom_sf(data = sites, mapping = aes(fill = type),
          shape = 21, color = "white", stroke = 0.1, size = 1) +
  scale_fill_manual(
    breaks = c("Community Space", "Place of Worship",
               "Social Business", "Park"),
    values = c("#0D0887", "#6A00A8", "#E16462" ,"#FCA636"),
    guide = guide_legend(override.aes = list(size = 5))) +
  theme_void(base_size = 14) +
  labs(x = NULL, y = NULL, fill = "Type",
       subtitle = paste(
         "Chicago\n",
         "2.7 milion\n",
         nrow(sites), " sites\n",
         " (", bounds$area, " sq.km.)",
         sep = ""))



# Get Sites
sites <- read_csv("houston/results.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = wgs)
# Get bounds
bounds <- read_sf("houston/bounds.geojson") %>%
  mutate(area = as.numeric(st_area(.$geometry)) / 1000000,
         area = round(area, 0))

# Get Grid
grid <- read_sf("houston/grid.geojson")

g4 <- ggplot() +
  geom_sf(data = bounds, color = "black", fill = "#373737") +
  geom_sf(data = grid, color = "grey", fill = NA) +
  #geom_sf_text(data = grid,
  #             mapping = aes(label = cell), color = "black", size = 1) +
  geom_sf(data = sites, mapping = aes(fill = type),
          shape = 21, color = "white", stroke = 0.1, size = 1) +
  scale_fill_manual(
    breaks = c("Community Space", "Place of Worship",
               "Social Business", "Park"),
    values = c("#0D0887", "#6A00A8", "#E16462" ,"#FCA636"),
    guide = guide_legend(override.aes = list(size = 5))) +
  theme_void(base_size = 14) +
  labs(x = NULL, y = NULL, fill = "Type",
       subtitle = paste(
         "Houston\n",
         "2.4 milion\n",
         nrow(sites), " sites\n",
         " (", bounds$area, " sq.km.)",
         sep = ""))



# Get Sites
sites <- read_csv("phoenix/results.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = wgs)
# Get bounds
bounds <- read_sf("phoenix/bounds.geojson") %>%
  mutate(area = as.numeric(st_area(.$geometry)) / 1000000,
         area = round(area, 0))

# Get Grid
grid <- read_sf("phoenix/grid.geojson")

g5 <- ggplot() +
  geom_sf(data = bounds, color = "black", fill = "#373737") +
  geom_sf(data = grid, color = "grey", fill = NA) +
  #geom_sf_text(data = grid,
  #             mapping = aes(label = cell), color = "black", size = 1) +
  geom_sf(data = sites, mapping = aes(fill = type),
          shape = 21, color = "white", stroke = 0.1, size = 1) +
  scale_fill_manual(
    breaks = c("Community Space", "Place of Worship",
               "Social Business", "Park"),
    values = c("#0D0887", "#6A00A8", "#E16462" ,"#FCA636"),
    guide = guide_legend(override.aes = list(size = 5))) +
  theme_void(base_size = 14) +
  labs(x = NULL, y = NULL, fill = "Type",
       subtitle = paste(
         "Phoenix\n",
         "1.6 milion\n",
         nrow(sites), " sites\n",
         " (", bounds$area, " sq.km.)",
         sep = ""))


dir.create("viz")

a1 <- ggpubr::ggarrange(g1,g2, ncol = 2, common.legend = TRUE, legend = "none")
a2 <- ggpubr::ggarrange(g3,g4, g5, ncol = 3, common.legend = TRUE, legend = "bottom")

combo <- ggpubr::ggarrange(a1, a2, ncol = 1)

ggsave(combo, filename = "viz/cities_5.png", dpi = 500, width = 8, height = 9)
```

