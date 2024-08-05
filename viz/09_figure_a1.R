# FIGURE A1: MAP OF MISSING DATA #########################

#' @name `09_figure_a1.R`
#' @title Missing Data Maps
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


# Get rates per tract
original = geo %>%
  tbl("data_tract") %>%
  filter(name %in% !!meta$top) %>%
  # Count up total sites per census tract
  mutate(missing = case_when(
    pop_density <= 0 | is.na(pop_density) ~ "Unpopulated",
    is.na(median_household_income) ~ "Missing",
    is.na(unemployment) ~ "Missing",
    is.na(income_0_60K) ~ "Missing",
    is.na(some_college) ~ "Missing",
    is.na(over_65) ~ "Missing",
    TRUE ~ "Present")) %>%
  select(name, geoid, missing) %>%
  collect()

dbDisconnect(geo); remove(geo)

# Calculate statistics per city.
cities = original %>%
  group_by(name) %>%
  summarize(sites = sum(sites, na.rm = TRUE),
            pop = sum(pop, na.rm = TRUE),
            area = sum(area, na.rm = TRUE),
            tracts = sum(!is.na(geoid))) %>%
  mutate(label = paste0(sites, " sites in ", tracts, " tracts")) %>%
  left_join(by = "name", y= read_csv("viz/cities.csv") %>% select(name, name_label, pop_label)) %>%
  mutate(pop_label = paste0("pop. ", pop_label))

bounds = read_sf("search/all/bounds.geojson") %>%
  inner_join(by = c("name"), y = cities %>% select(name))

# Calculate missing data
mymissing = original %>%
  as_tibble() %>%
  inner_join(by = c("name"), y = read_csv("viz/cities.csv") %>% select(name, name_label)) %>%
  group_by(name, name_label) %>%
  summarize(
    cells = n(),
    n_present = sum(missing == "Present"),
    n_missing = sum(missing == "Missing"),
    label = paste0("n=",n_present, "/", cells))

dbDisconnect(geo); remove(geo)

# Load shapes
grid = read_sf("search/all/tally1km.geojson") %>%
  select(name, cell, geometry) %>%
  inner_join(
    by = c("name", "cell"),
    y = original)


bounds = read_sf("search/all/bounds.geojson")

# Fix the cropping on these shapes, to make clearer visuals.

# San Francisco Crop
cropbox = c("xmin" = -122.55, "xmax" = -122.3276, "ymin" = 37.69274, "ymax" = 37.85) %>% st_bbox()

bounds = bind_rows(
  bounds %>% filter(name != "san_francisco"),
  bounds %>% filter(name == "san_francisco") %>% st_crop(cropbox)
)

# Load shapes
grid = read_sf("search/all/tracts.geojson") %>%
  select(name, geoid, geometry) %>%
  inner_join(
    by = c("name", "geoid"),
    y = original)

grid2 = meta$top %>%
  map(~st_intersection(x = grid %>% filter(name == .x),
                       y = bounds %>% filter(name == .x))) %>%
  bind_rows()

# Load Packages
library(sf)
library(ggplot2)


get_viz = function(.name = "la"){

  mybounds = bounds %>% filter(name == .name)

  # Get bounds
  mygrid = grid2 %>% filter(name == .name)

  tab = mymissing %>% filter(name == .name)

  # Visualize the study areas and their missing data
  gg = ggplot() +
    geom_sf(data = mygrid, mapping = aes(fill = missing),
            linewidth = 0.05, color = "white") +
    geom_sf(data= mybounds, fill = NA, linewidth = 0.5, color = "black") +
    scale_fill_manual(
      breaks = c("Present", "Missing", "Unpopulated"),
      values = c("#648FFF66", "#DC267F66", "grey")) +
    theme_void(base_size = 14) +
    guides(fill = "none") +
    theme(plot.subtitle = element_text(hjust = 0, size = 10),
          plot.title = element_text(hjust = 0),
          plot.margin = margin(0,0,0,0,"cm")) +
    labs(subtitle = tab$label,
         title = tab$name_label)

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

ggsave(plot = ggall, filename = "viz/figure_a1_missing_map.png", dpi = 200, width = 8, height = 9)
browseURL("viz/figure_a1_missing_map.png")

rm(list = ls())

