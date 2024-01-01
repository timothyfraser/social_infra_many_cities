
# FIGURE 1 #####################################

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

# FIGURE A1: MAP OF MISSING DATA #########################

#' @name `05_missing_maps.R`
#'
#' @description Script for making maps of missing data.

## Data #########################################

library(sf)
library(dplyr)
library(DBI)
library(readr)
library(purrr)
library(tidyr)
library(broom)

source("keys.R")
data("meta")
data("connect")
geo = connect()
geo %>% dbListTables()

# What cells are missing?
# original %>%
#   filter(is.na(some_college_bg)) %>%
#   group_by(name) %>%
#   count()

# original %>%
#   summary()
# group_by(name) %>%
#   summarize(missing = sum(is.na(median_household_income_bg)) / n())


original =  geo %>%
  tbl("data_grid") %>%
  filter(name %in% !!meta$top) %>%
  #filter(pop_density_block > 0) %>%
  collect() %>%
  select(name, cell, social_capital, bonding:linking,
         total, community_space, place_of_worship, social_business, park,
         pop_density_block, white_block, black_block, hisplat_block, asian_block, income_0_60K_bg, median_household_income_bg,
         some_college_bg, over_65_bg, unemployment_bg) %>%
  mutate(nonwhite_block = 1 - white_block)  %>%
  mutate(missing = case_when(
    pop_density_block <= 0 | is.na(pop_density_block) ~ "Unpopulated",
    is.na(median_household_income_bg) ~ "Missing",
    is.na(unemployment_bg) ~ "Missing",
    is.na(income_0_60K_bg) ~ "Missing",
    is.na(some_college_bg) ~ "Missing",
    is.na(over_65_bg) ~ "Missing",
    TRUE ~ "Present")) %>%
  select(name, cell, missing) %>%
  # Join in names
  left_join(by = "name", y = read_rds("viz/poptally.rds") %>% select(name, name_label))

dbDisconnect(geo); remove(geo)

# Load shapes
grid = read_sf("search/all/tally1km.geojson") %>%
  select(name, cell, geometry) %>%
  inner_join(
    by = c("name", "cell"),
    y = original)

bounds = read_sf("search/all/bounds.geojson")

# Load Packages
library(sf)
library(ggplot2)


get_viz = function(.name = "la"){

  mybounds = bounds %>% filter(name == .name)

  # Get bounds
  mygrid = grid %>% filter(name == .name)

  tab = mygrid %>%
    as_tibble() %>%
    summarize(
      name = name_label[1],
      cells = n(),
      n_present = sum(missing == "Present"),
      n_missing = sum(missing == "Missing"),
      label = paste0("n=",n_present, "/", cells))

  # Visualize the study areas and their missing data
  gg = ggplot() +
    geom_sf(data = mygrid, mapping = aes(fill = missing),
            linewidth = 0.05, color = "white") +
    geom_sf(data= mybounds, fill = NA, linewidth = 0.5, color = "black") +
    scale_fill_manual(
      breaks = c("Present", "Missing"),
      values = c("#648FFF66", "#DC267F66")) +
    theme_void(base_size = 14) +
    guides(fill = "none") +
    theme(plot.subtitle = element_text(hjust = 0, size = 10),
          plot.title = element_text(hjust = 0),
          plot.margin = margin(0,0,0,0,"cm")) +
    labs(subtitle = tab$label,
         title = tab$name)

  return(gg)
}

# Generate ggplots for every city
meta$top %>% map(~get_viz(.name = .)) %>% saveRDS("viz/gg_missing.rds")

gg = read_rds("viz/gg_missing.rds")

library(ggpubr)
ggall =  ggpubr::ggarrange(
  plotlist = gg,
  ncol = 5, nrow = 5,
  legend = "bottom",
  common.legend = TRUE)

ggsave(plot = ggall, filename = "viz/missing_map.png", dpi = 200, width = 8, height = 9)


rm(list = ls())

