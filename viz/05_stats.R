#' @name 05_stats.R
#' @description
#' Revisions replication code for statistical analysis.

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
data("wkb_as_sf")

geo = connect()
geo %>% dbListTables()


meta$top

# GRID DATA ######################################

sort(meta$top)

geo %>%
  tbl("data_grid") %>%
  filter(name %in% !!meta$top) %>%
  filter(pop_density_block > 0) %>%
  collect() %>%
  select(name, cell, social_capital, bonding:linking,
         total, community_space, place_of_worship, social_business, park,
         pop_density_block, white_block, black_block, hisplat_block, asian_block, income_0_60K_bg, median_household_income_bg,
         some_college_bg, over_65_bg, unemployment_bg, geometry) %>%
  mutate(nonwhite_block = 1 - white_block) %>%
  na.omit() %>%
  # Make into spatial data
  wkb_as_sf() %>%
  st_as_sf(crs = 4326) %>%
  mutate(geometry %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>% select(x = 1, y = 1)) %>%
  as_tibble() %>%
  select(-geometry) %>%
  saveRDS("viz/dataset_grid.rds")

# TRACT DATA ######################################

dat = geo %>%
  tbl("data_tract") %>%
  # Filter to 25 biggest cities
  filter(name %in% !!meta$top) %>%
  # Filter out 152 tracts with a population density of 0
  filter(pop_density > 0) %>%
  select(name, geoid, social_capital, bonding:linking,
       total, community_space, place_of_worship, social_business, park,
       pop_density, white, black, hisplat, asian, income_0_60K, median_household_income,
       some_college, over_65, unemployment, geometry) %>%
  mutate(nonwhite = 1 - white)  %>%
  collect() %>%
  # Drop 110 tracts with missing data
  na.omit() %>%
  # Get centroids
  wkb_as_sf() %>%
  st_as_sf(crs = 4326) %>%
  mutate(geometry %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>% select(x = 1, y = 1)) %>%
  as_tibble() %>%
  select(-geometry) %>%
  # Save to file
  saveRDS("viz/dataset_tract.rds")

dbDisconnect(geo); rm(list = ls()); gc()

# DESCRIPTIVES ##################################
library(ggplot2)
dat %>%
  ggplot(mapping = aes(x = value, fill = outcome)) +
  geom_density(alpha = 0.5)

read_rds("viz/dataset_tract.rds") %>%
  nrow()

read_rds("viz/dataset_grid.rds") %>%
  nrow()


# MODELING #####################################
read_rds("viz/dataset_tract.rds") %>% nrow()

read_rds("viz/dataset_tract.rds") %>% nrow()

dat = read_rds("viz/dataset_tract.rds") %>%
  group_by(name) %>%
  mutate_at(vars(pop_density,
                 black, hisplat,asian, median_household_income,income_0_60K,
                 some_college, over_65, unemployment), list(~scale(.))) %>%
  ungroup() %>%
  na.omit() %>%
  pivot_longer(
    cols = c(social_capital, bonding, bridging, linking),
    names_to = "outcome", values_to = "value")

dat %>%
  split(.$outcome) %>%
  map(~lm(formula = value ~ log(total + 1) +
            pop_density + black + hisplat + asian +
            median_household_income + income_0_60K +
            some_college + over_65 + unemployment + name, data = .),
      .id = "outcome") %>%
  map_dfr(~broom::glance(.))

# Logit function makes virtually no difference. Skip it.
logit = function(p) { log(p / (1 - p))}
dat %>%
  split(.$outcome) %>%
  map(~lm(formula = logit(value) ~ log(total + 1) +
            pop_density + black + hisplat + asian +
            median_household_income + income_0_60K +
            some_college + over_65 + unemployment + name, data = .),
      .id = "outcome") %>%
  map_dfr(~broom::glance(.))
