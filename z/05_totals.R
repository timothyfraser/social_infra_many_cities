#' @name `05_totals.R`
#' @description A script to examine overall sample totals

library(sf)
library(dplyr)
library(DBI)
library(readr)
library(purrr)
library(stringr)

data("connect")
data("wkb_as_sf")
data("aea")
data("meta")

# Is Boston representative of other cities in our sample?
source("keys.R")
geo = connect()

geo %>% dbListTables()

# TALLY TOTALS BY CITY #################################

sites = geo %>%
  tbl("sites") %>%
  select(name, type) %>%
  group_by(name) %>%
  summarize(
    total = n(),
    park = sum(type == "Park", na.rm = TRUE),
    social_business = sum(type == "Social Business", na.rm = TRUE),
    place_of_worship = sum(type == "Place of Worship", na.rm = TRUE),
    community_space = sum(type == "Community Space", na.rm = TRUE)
  ) %>%
  collect()

grids = geo %>%
  tbl("data_grid") %>%
  filter(name %in% !!meta$top) %>%
  filter(pop_density_block > 0) %>%
  select(cell, name, total:park, pop_density_block)  %>%
  group_by(name) %>%
  summarize(
    cells = n(),
    rate_total = mean(total, na.rm = TRUE),
    rate_community_space = mean(community_space, na.rm = TRUE),
    rate_place_of_worship = mean(place_of_worship, na.rm = TRUE),
    rate_social_business = mean(social_business, na.rm = TRUE),
    rate_park = mean(park, na.rm = TRUE)
  ) %>% collect()


blocks = geo %>%
  tbl("block_data") %>%
  filter(name %in% !!meta$top) %>%
  filter(area_land > 0, pop > 0) %>%
  select(name, pop, area_land, black, white, asian, hisplat) %>%
  collect() %>%
  group_by(name) %>%
  summarize(
    across(.cols = c(white, black, asian, hisplat),
           .fns = ~sum(.x * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE)),
    pop = sum(pop, na.rm = TRUE),
    area_land = sum(area_land, na.rm = TRUE) / (1000^2),
    pop_density = (pop / area_land) ) %>%
  # Generate Labels
  mutate(name_label = name %>% str_split(pattern = "_") %>%
         map(~paste(str_sub(., 1,1) %>% toupper(),
                    str_sub(., 2,-1), sep = "")) %>%
         map(~paste(., collapse = " ")) %>% unlist(),
       name_label = if_else(name_label %in% c("Nyc", "La", "Dc"), toupper(name_label), name_label),
       name_label = if_else(name_label == "Oklahoma", "Oklahoma City", name_label)) %>%
  mutate(pop_label = round(pop / 1e6, 2))



blocks %>%
  left_join(by = "name", y= sites) %>%
  left_join(by = "name", y= grids) %>%
  saveRDS("viz/poptally.rds")


# grids %>%
#   left_join(by = c("type", "name"), y = stat) %>%
#   left_join(by = "type", y = grand) %>%
#   mutate(within = (rate - xbar)^2,
#          between = (xbar - xbbar)^2)

# ggplot() +
#   geom_crossbar(data = stat, mapping = aes(x = name, y = xbar, ymin = lower, ymax = upper)) +
#   facet_wrap(~type, ncol = 5) +
#   coord_flip()
#
# ggplot() +
#   geom_crossbar(data = viz %>% filter(type == "total"), mapping = aes(x = type, y = xbar, ymin = lower, ymax = upper)) +
#   geom_hline(data = viz %>% filter(type == "total"), mapping = aes(yintercept = xbbar)) +
#   facet_wrap(~name, scales = "free_y")

# Let's just make a table instead.

#
# viz %>%
#
#
# # For each city, find out if its rate is significantly different from
# grids %>%
#   mutate(strata = paste0(type, "-", name)) %>%
#   split(.$strata) %>%
#   map_dfr(
#     .f = ~{
#       # Get the type
#       .type = str_remove(.$strata[1], "[-].*")
#
#       # Get population mean for that type
#       mu = grids %>%
#         filter(type == .type) %>%
#         summarize(mu = mean(rate, na.rm = TRUE)) %>%
#         with(mu)
#
#       # One sample t-test
#       .x %>%
#         summarize(
#         t.test(
#           x = rate,
#           mu = mu,
#           data = .) %>%
#           broom::tidy()) } ,
#           .id = "strata") %>%
#   separate(strata, into = c("type", "name"), sep = "-", remove = TRUE) %>%
#   saveRDS("viz/popstat.rds")
#
#
# read_rds("viz/popstat.rds")
#


# TALLY FOR BAR CHART #################################
library(readr)
library(dplyr)



read_rds("viz/poptally.rds") %>%
  select(name, name_label, pop, pop_label, contains("rate")) %>%
  pivot_longer(cols = c(contains("rate")), names_to = "type", values_to = "rate") %>%
  mutate(typeid = str_remove(type, "rate[_]")) %>%
  mutate(type = typeid %>%
           recode_factor(
             "total" = "**Total**",
             "community_space" = "**Community<br>Spaces**",
             "place_of_worship" = "**Places<br>of Worship**",
             "social_business" = "**Social<br>Businesses**",
             "park" = "**Parks**")
  ) %>%
  mutate(rate = rate * 10,
         label = round(rate, 1)) %>%
  # Now make just a few adjustments...
  arrange(desc(pop), type) %>%
  group_by(type) %>%
  mutate(order = 1:n()) %>%
  group_by(type) %>%
  mutate(add = rate / 75,
         fill = if_else(str_detect(name_label, "Boston"), "Case", "Other")) %>%
  ungroup() %>%
  saveRDS("viz/tally_stat.rds")



# TALLY TOTALS OVERALL ################################

read_rds("viz/poptally.rds") %>%
  summarize(pop = sum(pop, na.rm = TRUE),
            total = sum(total, na.rm = TRUE),
            boston_cells = sum(cells[name == 'boston'], na.rm = TRUE),
            other_cells = sum(cells[name != 'boston'], na.rm = TRUE),
            cells = sum(cells, na.rm = TRUE)
)
dbDisconnect(geo)
rm(list = ls())

# TALLY DIFFERENCES OVERALL #################################

data("connect")
data("wkb_as_sf")
data("aea")
data("meta")

# Is Boston representative of other cities in our sample?
source("keys.R")
geo = connect()
library(tidyr)
library(dplyr)
grids = geo %>%
  tbl("data_grid") %>%
  filter(name %in% !!meta$top) %>%
  filter(pop_density_block > 0) %>%
  select(cell, name, total:park, pop_density_block) %>%
  collect() %>%
  tidyr::pivot_longer(cols = -c(name, cell, pop_density_block), names_to = "type", values_to = "rate")

DBI::dbDisconnect(geo)

grand = grids %>%
  group_by(type) %>%
  summarize(xbbar = mean(rate, na.rm = TRUE))

# Get within-group stats
grids %>%
  group_by(type, name) %>%
  summarize(xbar = mean(rate, na.rm = TRUE),
            s = sd(rate, na.rm = TRUE),
            n = n(),
            df = n - 1,
            se = s^2 / n,
            lower = xbar - 3*se,
            upper = xbar + 3*se) %>%
  # How much does each city differ from the grandmean?
  left_join(by = "type", y= grand) %>%
  mutate(diff = xbar - xbbar) %>%
  mutate(statistic = diff / se) %>%
  mutate(p_value = 2*(1 - pt(abs(statistic), df = df))) %>%
  left_join(by = "name", y = read_rds("viz/poptally.rds") %>% select(name, pop, name_label)) %>%
  mutate(typeid = type) %>%
  mutate(type = typeid %>% dplyr::recode_factor(
    'total' = "Total",
    "community_space" = "Community Spaces",
    "place_of_worship" = "Places of Worship",
    "social_business" = "Social Businesses",
    "park" = "Parks")) %>%
  mutate(stars = gtools::stars.pval(p_value)) %>%
  select(type, typeid, name_label, pop, xbar, lower, upper, s, n,  xbbar, diff, statistic, se, df, p_value, stars) %>%
  arrange(desc(type), desc(pop)) %>%
  saveRDS("viz/popdiff.rds")

read_rds("viz/popdiff.rds") %>%
  filter(type == "Total") %>%
  ggplot(mapping = aes(x = reorder(name_label, -statistic), y = statistic)) +
  geom_col() +
  coord_flip()


tab = read_rds("viz/popdiff.rds") %>%
  select(-typeid) %>%
  arrange(desc(type), desc(pop)) %>%
  mutate(across(.cols = any_of(c("xbar", "lower", "upper", "s", "xbbar", "diff", "statistic", "se")),
                               .fns = ~format(.x, digits = 2, scientific = FALSE))) %>%
  mutate(p_value = case_when(
    p_value < 0.001 ~ "<0.001",
    p_value >= 0.001 ~ paste0(round(p_value, 3)))) %>%
  select(-pop)

tab %>%
  write_csv("viz/table_diff_from_grandmean.csv")

