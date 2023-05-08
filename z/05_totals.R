#' @name `05_totals.R`
#' @description A script to examine overall sample totals

library(sf)
library(dplyr)
library(DBI)
library(readr)
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



# STATS FOR BAR CHART #################################
library(tidyr)
library(dplyr)
grids = geo %>%
  tbl("data_grid") %>%
  filter(name %in% !!meta$top) %>%
  filter(pop_density_block > 0) %>%
  select(cell, name, total:park, pop_density_block) %>%
  collect() %>%
  tidyr::pivot_longer(cols = -c(name, cell, pop_density_block), names_to = "type", values_to = "rate") %>%
  filter(type == "total")


mu = grids$rate %>% mean(na.rm = TRUE)


grids %>%
  group_by(name) %>%
  summarize(t.test(y = rate, mu = mu, data = .) %>% broom::tidy())

t.test()

stat = grids %>%
  group_by(name) %>%
  summarize(
    xbar_city = mean(rate, na.rm = TRUE),
    s_city = sd(rate, na.rm = TRUE),
    n_city = n()) %>%
  # Get overall stats
  mutate(
    xbar_grand = mean(xbar_city, na.rm = TRUE),
    s_grand = sd(grids$rate, na.rm = TRUE),
    n_grand = grids %>% nrow()
  ) %>%
  group_by(name) %>%
  # Calculate the difference of means and standard error, assuming group variances are unequal
  summarize(dbar = xbar_city - xbar_grand,
            se = sqrt(sum( s_city^2 / n_city  + s_grand^2 / n_grand ))) %>%
  mutate(lower = dbar - qnorm(0.975) * se,
         upper = dbar + qnorm(0.975) * se,
         statistic = dbar / se,
         p_value = 1/2*pnorm(statistic))

df = (s1^2/n1 + s2^2/n2) / (s1^4/(n1^2*(n2-1)) + s2^2/(n2^2*(n1-1)))

# Technically, you need to use a t-distribution
# df = n - 2


group_by(name) %>%
  summarize(
    cells = n(),
    rate_total = mean(total, na.rm = TRUE),
    rate_community_space = mean(community_space, na.rm = TRUE),
    rate_place_of_worship = mean(place_of_worship, na.rm = TRUE),
    rate_social_business = mean(social_business, na.rm = TRUE),
    rate_park = mean(park, na.rm = TRUE)
  ) %>% collect()

grids

# TALLY FOR BAR CHART #################################
library(readr)
library(dplyr)



read_rds("viz/poptally.rds") %>%
  select(name, name_label, pop, pop_label, contains("rate")) %>%
  pivot_longer(cols = c(contains("rate")), names_to = "type", values_to = "rate") %>%
  mutate(type = str_remove(type, "rate[_]") %>%
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
