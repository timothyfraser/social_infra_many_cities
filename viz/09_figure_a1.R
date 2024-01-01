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

# Fix the cropping on these shapes, to make clearer visuals.

# San Francisco Crop
cropbox = c("xmin" = -122.55, "xmax" = -122.3276, "ymin" = 37.69274, "ymax" = 37.85) %>% st_bbox()

bounds = bind_rows(
  bounds %>% filter(name != "san_francisco"),
  bounds %>% filter(name == "san_francisco") %>% st_crop(cropbox)
)

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

ggsave(plot = ggall, filename = "viz/figure_a1_missing_map.png", dpi = 200, width = 8, height = 9)


rm(list = ls())

