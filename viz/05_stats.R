#' @name 05_stats.R
#' @description
#' Revisions replication code for statistical analysis.

# DATA ######################################
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
meta$top

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

## city descriptives #############################

geo = connect()

city_covariates = geo %>%
  tbl("data_tract") %>%
  filter(name %in% !!meta$top) %>%
  group_by(name) %>%
  summarize(pop = sum(pop, na.rm = TRUE),
            area = sum(area, na.rm = TRUE),
            social_capital_mean = mean(social_capital, na.rm = TRUE),
            social_capital_sd = sd(social_capital),
            bridging_mean = mean(bridging, na.rm =TRUE),
            bridging_sd = sd(bridging, na.rm = TRUE),
            nobs = sum(!is.na(geoid))) %>%
  ungroup() %>%
  collect()



tribble(
  ~name, ~name_label,
  "austin", "Austin",
  "boston", "Boston",
  "charlotte", "Charlotte",
  "chicago", "Chicago",
  "columbus", "Columbus",
  "dallas", "Dallas",
  "dc", "Washington DC",
  "denver", "Denver",
  "el_paso", "El Paso",
  "fort_worth", "Fort Worth",
  "houston", "Houston",
  "indianapolis", "Indianapolis",
  "jacksonville", "Jacksonville",
  "la", "Los Angeles",
  "nashville", "Nashville",
  "nyc", "NYC",
  "oklahoma", "Oklahoma City",
  "philadelphia", "Philadelphia",
  "phoenix", "Phoenix",
  "portland", "Portland",
  "san_antonio", "San Antonio",
  "san_diego", "San Diego",
  "san_francisco", "San Francisco",
  "san_jose", "San Jose",
  "seattle", "Seattle"
) %>%
  left_join(by = c("name"), y = city_covariates) %>%
  arrange(desc(pop)) %>%
  mutate(pop_label = scales::number(pop, accuracy = 0.1, scale_cut = scales::cut_si(unit = "")),
         area_label = scales::number(area, accuracy = 0.1, scale_cut = scales::cut_si(unit = " sq.km"))) %>%
  write_csv("viz/cities.csv")


dbDisconnect(geo)

read_csv("viz/cities.csv") %>%
  filter(bridging_mean == min(bridging_mean) |
           bridging_mean == max(bridging_mean))


## outlier detection ################################

# Connect to database
geo = connect()
geo %>% dbListTables()

# What are the census tracts with the highest rates of social infrastructure...
geo %>%
  tbl("data_tract") %>%
  # Filter to 25 biggest cities
  filter(name %in% !!meta$top) %>%
  # Filter out 152 tracts with a population density of 0
  filter(pop_density > 0) %>%
  select(name, geoid, total:park, pop, area, pop_density) %>%
  collect() %>%
  top_n(total, n = 20)

# There are a bunch of tracts that have VERY LOW POPULATION (eg. 5, 10, 20, <50),
# usually overlapping parks.


# This produces unreasonably large rates of social infrastructure.
# There are 31 tracts with population below 50
q = geo %>%
  tbl("data_tract") %>%
  # Filter to 25 biggest cities
  filter(name %in% !!meta$top) %>%
  # Filter out 152 tracts with a population density of 0
  filter(pop_density > 0) %>%
  # Social infrastructure rates are measured AS:
  # sites per 1000 residents per square kilometer
  # So lets backtransform to the the original count of total sites per tract
  mutate(sites = total * pop_density / 1000)




filter(pop <= 50) %>%
  collect()

q %>% collect() %>%
  top_n(sites, n = 20) %>%
  View()

# Certain areas of cities are split in such a way that
# there are basically commercial or park-style tracts
# For example,
# - tract '11001980000' is the national mall in DC.
# - tract '06037980019' in LA squarely overlaps Topanga State Park.
# It's not exactly right to remove these sites from consideration,
# but it's not really realistic to say that
# we should be comparing these to the rest of the city.

# There are 2 kinds of outliers that produce weirdly high rates.
# - low population tracts
# - tracts with a huge number of total sites
# We want to keep genuine, normal low-population tracts,
# but we want to exclude these weird places like the national mall,
# since that is not really describing social infrastructure in people's day-to-day life.

# How many places have both a population below 50 AND sites above 50?

# See, row2 4 (DC) here is a great example.
# Extremely low population, extremely high sites.
# But the other ones are quite fair and realistic.
q %>%
  collect() %>%
  select(name, geoid, sites, pop, total) %>%
  top_n(sites, n = 20)

# There is just 1 tract with over 50 sites and under 50 population
q  %>%
  filter( (sites >= 50 & pop <= 50)  )

# Let's adjust our thresholds.
# There are 5 sites with over 20 sites but under 50 residents
q  %>%
  filter( (sites >= 20 & pop <= 50)  ) %>%
  count()

# There are 10 tracts with over 10 sites but under 100 residents
q  %>%
  filter( (sites >= 10 & pop <= 100)  )  %>%
  select(name, geoid, sites, pop, area, pop_density, total) %>%
  collect() %>%
  write_csv("viz/outliers_removed_tract.csv")

# Let's cut these from the statistical analyses.
read_csv("viz/outliers_removed_tract.csv")$geoid


# Get all the outlier tracts polygons
outlier_tracts = geo %>%
  tbl("data_tract") %>%
  select(name, geoid, geometry) %>%
  # Get these tracts
  inner_join(by = "geoid", y = read_csv("viz/outliers_removed_tract.csv") %>% select(geoid), copy = TRUE) %>%
  collect() %>%
  wkb_as_sf() %>%
  st_as_sf(crs = 4326)

# Get all the grid cell polygons
grid = geo %>%
  tbl("data_grid") %>%
  select(name, cell, geometry) %>%
  collect() %>%
  wkb_as_sf() %>%
  st_as_sf(crs = 4326)


# Find just the grid cells that overlap within the polygons
cells_contained = outlier_tracts %>%
  st_join(y = grid %>% select(cell), join = st_overlaps, left= FALSE) %>%
  as_tibble() %>%
  select(name, cell) %>%
  inner_join(x = grid, y = ., by = c("cell", "name") ) %>%
  as_tibble() %>%
  select(name, cell) %>%
  write_csv("viz/outliers_removed_grid.csv")



# GRID DATA ######################################

sort(meta$top)


geo %>%
  tbl("data_grid") %>%
  filter(name %in% !!meta$top) %>%
  filter(pop_density_block > 0) %>%
  select(name, cell, social_capital, bonding:linking,
         total, community_space, place_of_worship, social_business, park,
         pop_density_block, white_block, black_block, hisplat_block, asian_block, income_0_60K_bg, median_household_income_bg,
         some_college_bg, over_65_bg, unemployment_bg, geometry) %>%
  mutate(nonwhite_block = 1 - white_block) %>%
  # Exclude any cells that overlap with the outlier tracts
  anti_join(by = c("name", "cell"), copy = TRUE,
            y = read_csv("viz/outliers_removed_grid.csv")) %>%
  # Collect the data
  collect() %>%
  na.omit() %>%
  # Make into spatial data
  wkb_as_sf() %>%
  st_as_sf(crs = 4326) %>%
  mutate(geometry %>% st_centroid() %>% st_coordinates() %>% as_tibble() %>% select(x = 1, y = 1)) %>%
  as_tibble() %>%
  select(-geometry) %>%
  saveRDS("viz/dataset_grid.rds")

# TRACT DATA ######################################

geo %>%
  tbl("data_tract") %>%
  # Filter to 25 biggest cities
  filter(name %in% !!meta$top) %>%
  # Filter out 152 tracts with a population density of 0
  filter(pop_density > 0) %>%
  select(name, geoid, social_capital, bonding:linking,
         total, community_space, place_of_worship, social_business, park,
         pop, area, pop_density, white, black, hisplat, asian, income_0_60K, median_household_income,
         some_college, over_65, unemployment, geometry) %>%
  mutate(nonwhite = 1 - white)  %>%
  # Exclude any cells that overlap with the outlier tracts
  anti_join(by = c("name", "geoid"), copy = TRUE,
            y = read_csv("viz/outliers_removed_tract.csv") %>%
              select(name, geoid)) %>%
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

## descriptives ################

# How many grid cells are there?
read_rds("viz/dataset_grid.rds") %>%
  nrow()

# How many tracts are there?
read_rds("viz/dataset_tract.rds") %>% nrow()

# read_rds("viz/dataset_tract.rds") %>%
#   filter(name == "boston") %>% nrow()

# What's the min-max range?
read_rds("viz/dataset_tract.rds") %>%
  group_by(name) %>%
  summarize(pop = sum(pop, na.rm = TRUE)) %>%
  ungroup() %>%
  summarize(
    total = sum(pop, na.rm = TRUE),
    min = min(pop, na.rm = TRUE),
    max = max(pop, na.rm = TRUE))

# What's the area range of these cities in km2?
read_rds("viz/dataset_tract.rds") %>%
  group_by(name) %>%
  summarize(area = sum(area, na.rm = TRUE)) %>%
  ungroup() %>%
  summarize(
    total = sum(area, na.rm = TRUE),
    min = min(area, na.rm = TRUE),
    max = max(area, na.rm = TRUE))


read_rds("viz/dataset_tract.rds") %>%
  pivot_longer(cols = c(social_capital:unemployment),
               names_to = "var", values_to = "value") %>%
  group_by(var) %>%
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    nobs = sum(!is.na(value))
  ) %>%
  mutate(across(.cols = c(mean:max), .fns = ~scales::number(.x, accuracy = 0.01)))%>%
  mutate(order = var %>% recode_factor(
    "social_capital" = "Social Capital Overall",
    "bonding" = "Bonding Social Capital Overall",
    "bridging" = "Bridging Social Capital Overall",
    'linking' = "Linking Social Capital Overall",
    "total" = "Total Social Infrastructure Rate",
    "community_space" = "Community Space Rate",
    "place_of_worship" = "Place of Worship Rate",
    "social_business" = "Social Business Rate",
    "park" = "Park Rate",

    "pop" = "Population",
    "area" = "Area (sq.km.)",
    "pop_density" = "Population Density",
    "black" = "% Black",
    "white" = "% White",
    "hisplat" = "% Hispanic/Latino",
    "asian" = "% Asian",
    "over_65" = "% Over Age 65",
    "some_college" = "% Some College",
    "income_0_60K" = "% Income Under 60K",
    "median_household_income" = "Median Household Income (USD)",
    "unemployment" = "Unemployment Rate",
  )) %>%
  arrange(order) %>%
  write_csv("viz/table_descriptives.csv")

read_csv("viz/table_descriptives.csv")
  filter(var %in% c("social_capital", "bonding", "bridging", "linking"))



## significance SC #############################

# Does social capital vary significantly between cities?

data = read_rds("viz/dataset_tract.rds") %>%
    select(name, geoid, social_capital:linking,
           total:park, pop_density, pop)

logit = function(p, adj = 0) { log( (p + adj) / (1 - (p - adj) ))}

bind_rows(
  data %>%
    reframe(aov(logit(social_capital) ~ name) %>% tidy()),
  data %>%
    reframe(aov(logit(bridging) ~ name) %>% tidy()),
  data %>%
    reframe(aov(logit(bonding) ~ name) %>% tidy()),
  data %>%
    reframe(aov(logit(linking) ~ name) %>% tidy()),
  .id = "type"
) %>%
  mutate(type = type %>% recode_factor(
    "total" = "SI Overall",
    "community_space" = "Community spaces",
    "place_of_worship" = "Places of Worship",
    "social_business" = "Social Business",
    "park" = "Parks"
  )) %>%
  filter(term == "name") %>%
  mutate(stars = gtools::stars.pval(p.value)) %>%
  mutate(label = paste0(scales::number(statistic, accuracy = 0.1), stars))

## significance SI ##########################

# Does social infrastructure vary significantly between cities?
data = read_rds("viz/dataset_tract.rds") %>%
  select(name, geoid, social_capital:linking,
         total:park, pop_density, pop)

# Let's get F-statistics for each.
bind_rows(
  data %>%
    reframe(aov(log(total + 1) ~ name) %>% tidy()),
  data %>%
    reframe(aov(log(community_space + 1) ~ name) %>% tidy()),
  data %>%
    reframe(aov(log(place_of_worship + 1) ~ name) %>% tidy()),
  data %>%
    reframe(aov(log(social_business + 1) ~ name) %>% tidy()),
  data %>%
    reframe(aov(log(park + 1) ~ name) %>% tidy()),
  .id = "type"
) %>%
  mutate(type = type %>% recode_factor(
    "1" = "SI Overall",
    "2" = "Community spaces",
    "3" = "Places of Worship",
    "4" = "Social Business",
    "5" = "Parks"
  )) %>%
  filter(term == "name") %>%
  mutate(stars = gtools::stars.pval(p.value)) %>%
  mutate(label = paste0(scales::number(statistic, accuracy = 0.1), stars))  %>%
  saveRDS("models/si_fstat.rds")



## models SI ##############################

### modeling ##############################
# Next, let's use some linear models to show how much each city's predicted level deviates.
data = read_rds("viz/dataset_tract.rds") %>%
  select(name, geoid, social_capital:linking,
         total:park, pop_density, pop,
         black, hisplat, asian, median_household_income, income_0_60K, some_college, over_65, unemployment)


logit = function(p, adj = 0) { log( (p + adj) / (1 - (p - adj) ))}

m1 = data %>%
  lm(formula = log(total + 1) ~ name +
     log(pop_density) +
       logit(black, 0.01) +
       logit(hisplat, 0.01) +
       logit(asian, 0.01) +
       log(median_household_income) +
       logit(income_0_60K, 0.01) +
       logit(some_college, 0.01) +
       logit(over_65, 0.01) +
       log(unemployment + 1)
     )

m2 = data %>%
  lm(formula = log(community_space + 1) ~ name +
       log(pop_density) +
       logit(black, 0.01) +
       logit(hisplat, 0.01) +
       logit(asian, 0.01) +
       log(median_household_income) +
       logit(income_0_60K, 0.01) +
       logit(some_college, 0.01) +
       logit(over_65, 0.01) +
       log(unemployment + 1)
  )
m3 = data %>%
  lm(formula = log(place_of_worship + 1) ~ name +
       log(pop_density) +
       logit(black, 0.01) +
       logit(hisplat, 0.01) +
       logit(asian, 0.01) +
       log(median_household_income) +
       logit(income_0_60K, 0.01) +
       logit(some_college, 0.01) +
       logit(over_65, 0.01) +
       log(unemployment + 1)
  )
m4 = data %>%
  lm(formula = log(social_business + 1) ~ name +
       log(pop_density) +
       logit(black, 0.01) +
       logit(hisplat, 0.01) +
       logit(asian, 0.01) +
       log(median_household_income) +
       logit(income_0_60K, 0.01) +
       logit(some_college, 0.01) +
       logit(over_65, 0.01) +
       log(unemployment + 1)
  )

m5 = data %>%
  lm(formula = log(park + 1) ~ name +
       log(pop_density) +
       logit(black, 0.01) +
       logit(hisplat, 0.01) +
       logit(asian, 0.01) +
       log(median_household_income) +
       logit(income_0_60K, 0.01) +
       logit(some_college, 0.01) +
       logit(over_65, 0.01) +
       log(unemployment + 1)
  )

list(m1=m1,m2=m2,m3=m3,m4=m4,m5=m5) %>%
  saveRDS("models/models_si.rds")
rm(list = ls())

### simulations #################################
data = read_rds("viz/dataset_tract.rds") %>%
  select(name, geoid, social_capital:linking,
         total:park, pop_density, pop,
         black, hisplat, asian, median_household_income,
         income_0_60K, some_college, over_65, unemployment)

logit = function(p, adj = 0) { log( (p + adj) / (1 - (p - adj) ))}

# For each model...
read_rds("models/models_si.rds") %>%
  # Get the predictions
  map_dfr(~data %>%
            mutate(yhat = predict(.x, newdata = data),
                   sigma = broom::glance(.x)$sigma), .id = "model") %>%
  # For each model and city and geoid, get 1000 simulations
  group_by(model, name, geoid) %>%
  reframe(ysim = rnorm(n = 1000, mean = yhat, sd = sigma),
          id = 1:1000) %>%
  ungroup() %>%
  # backtransform
  mutate(ysim = exp(ysim) - 1) %>%
  # Get the city average for each simulation per censsu tract
  group_by(model, name, id) %>%
  summarize(
    ysim = mean(ysim, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  saveRDS("simulations/si_value_per_city.rds")

# Calculate the MEAN overall prediction among city averages (grand mean)
overall = read_rds("simulations/si_value_per_city.rds")  %>%
  group_by(model, id) %>%
  reframe(grandmean = mean(ysim) )

read_rds("simulations/si_value_per_city.rds") %>%
  # Join in the overall grand mean
  left_join(by = c("model", "id"), y = overall) %>%
  # Calculate the difference between each city simulation
  # and the grand mean simulations
  # specifically, how much more less is the city value than the grandmean
  mutate(diff = grandmean - ysim) %>%
  # Calculate quantities of interest per city
  group_by(model, name) %>%
  summarize(
    estimate0 = median(grandmean),
    estimate1 = median(ysim),
    estimate = median(diff),
    lower = quantile(diff, probs = 0.025),
    upper =quantile(diff, probs = 0.975),
    #Get the probability of 1-tailed error
    prob1 = case_when(
      # If the estimate is positive, get share of cases LESS than 0.
      estimate > 0 ~ mean(diff < 0),
      # If the estimate is negative, get share of cases GREATER than 0.
      estimate < 0 ~ mean(diff > 0),
      # If the estimate is zero, your error is maximal
      estimate == 0 ~ 1),
    # The change could increase OR decrease the outcome,
    # meaning two tailed error, therefore, account for the possibility that
    # it could have been wrong on the other side too.
    prob2 = prob1*2) %>%
  ungroup() %>%
  saveRDS("simulations/si_qi_per_city.rds")

rm(list = ls())

### visual ############################

# read_rds("models/si_fstat.rds") %>%
#   select(type, label)

# Let's get overall estimates of the grand mean
overall = read_rds("simulations/si_value_per_city.rds")  %>%
  group_by(model, id) %>%
  reframe(grandmean = mean(ysim) ) %>%
  group_by(model) %>%
  summarize(
    estimate_g = mean(grandmean),
    sd_g = sd(grandmean),
    se_g = sqrt(sd_g / n()),
    lower_g = quantile(grandmean, probs = 0.025),
    upper_g = quantile(grandmean, probs = 0.975),
  ) %>%
  mutate(label = scales::number(estimate_g, accuracy = 0.1)) %>%
  mutate(model = model %>% recode_factor(
    "m1" = "Social Infrastructure\nOverall",
    "m2" = "Community\nSpaces",
    'm3' = "Places\nof Worship",
    "m4" = "Social\nBusinesses",
    "m5" = "Parks"
  ))


qi = read_rds("simulations/si_qi_per_city.rds") %>%
  mutate(order = stringr::str_extract(model, "[0-9]+") %>% as.numeric()) %>%
  # Join in the overall grandmean stats
  left_join(by = c("model"), y = overall) %>%
  mutate(model = model %>% recode_factor(
    "m1" = "Social Infrastructure\nOverall",
    "m2" = "Community\nSpaces",
    'm3' = "Places\nof Worship",
    "m4" = "Social\nBusinesses",
    "m5" = "Parks"
  )) %>%
  left_join(by = c("name"),
            y = read_csv("viz/cities.csv") %>%
              select(name, name_label, pop) ) %>%
  mutate(label = paste0(
    scales::number(estimate * -1, accuracy = 0.1, style_positive = "plus"),
    gtools::stars.pval(prob2)
  )) %>%
  mutate(ytext = estimate0 - upper) %>%
  group_by(model) %>%
  mutate(ytext = ytext + abs(diff(range(estimate0 - upper)))/5 ) %>%
  bind_rows(
    overall %>%
      mutate(name_label = "Grand Mean", pop = 1e8, ytext = estimate_g) %>%
      select(model, name_label, pop, label, ytext, estimate0 = estimate_g)
  )



gg = ggplot() +
  geom_col(data = qi, mapping = aes(
    x = reorder(name_label, pop), y = estimate0,
    fill = "Grand Mean SI Rate"),
    alpha = 0.5) +
  geom_col(
    data = qi,
    mapping = aes(
      x = reorder(name_label, pop), y = estimate1,
      fill = "Predicted SI Rate"),
           alpha = 0.5) +
  geom_linerange(data = qi, mapping = aes(x = reorder(name_label, pop),
                                          ymin = estimate0 - lower, ymax = estimate0 - upper)) +
  facet_grid(cols = vars(model), scales = "free") +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom") +
  scale_fill_manual(values = c("lightgrey", "#648FFF")) +
  shadowtext::geom_shadowtext(
    data = qi %>% filter(name_label != "Grand Mean"),
    mapping = aes(x = reorder(name_label, pop),
                             y = ytext,
                             label = label),
    bg.r = 0.2, bg.color = "white", color = "#648FFF",
    hjust = 0
  ) +
  shadowtext::geom_shadowtext(
    data = qi %>% filter(name_label == "Grand Mean"),
    mapping = aes(x = reorder(name_label, pop),
                  y = ytext,
                  label = label),
    bg.r = 0.2, bg.color = "white", color = "darkgrey",
    hjust = 0
  ) +
  scale_y_continuous(expand = expansion(c(0, 0.5))) +
  theme(strip.background = element_rect(fill = "black")) +
  theme(strip.text = element_text(color = "white", face = "bold"),
        plot.caption = element_text(hjust= 0),
        panel.spacing.x = unit(0.5, "cm")) +
  labs(y = "Mean Predicted Social Infrastructure Rate",
       caption = paste0(
         "Bars show average predictions among all census tracts at observed values, aggregated by city or overall.",
         '\n',
         "Bands show 95% confidence intervals from 1000 Monte Carlo simulations per tract.",
         "\n",
         "Statistical Significance: *** = p < 0.001, ** = p < 0.01, * = p < 0.05."
         ),
       title = "Predicted City Differences in Social Infrastructure Rates",
       fill = "Type",
       x = "Cities, Ordered by Population")

ggsave(gg, filename = "viz/figure_city_si.png", dpi = 500, width = 10, height = 8)
browseURL("viz/figure_city_si.png")
# data = read_rds("viz/dataset_tract.rds") %>%
#   select(name, geoid, total:park) %>%
#   pivot_longer(cols = c(total:park), names_to = "var", values_to = "value")
#
# g1 = ggplot() +
#   geom_text(data = overall,
#             mapping = aes(x = model, y = "Grand Mean", label = label)) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         panel.border = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text.x = element_blank())
#
# # g1 = ggplot() +
# #   geom_histogram(data = data, mapping = aes(x = log(value + 1)),
# #                  fill = "lightgrey", color = 'white') +
# #   facet_wrap(~var, scales = "free_x", nrow = 1) +
# #   theme_bw()
# blank = ggplot() + theme_void()
#
# gtop = ggpubr::ggarrange(
#   plotlist = list(blank, g1),
#   nrow = 1, widths = c(0.05, 0.95))
#
# ggpubr::ggarrange(plotlist = list(gtop, g2), nrow = 2, heights = c(0.1, 0.9))

## tables ######################################

library(dplyr)
library(readr)
library(scales)
library(tidyr)
library(stringr)
library(broom)
m = read_rds("models/models_si.rds")


# Write a function to get vif
get_vif = function(m){ x = (car::vif(m)^2)[,3]; tibble(term = names(x), vif = x) }

tidier = function(m){
  broom::tidy(m) %>%
    rename(se = std.error, stat = statistic, p_value = p.value) %>%
    mutate(stars = gtools::stars.pval(p_value))
}

# Check max vif
myvif = m %>% map_dfr(~get_vif(.), .id = "model") %>%
  group_by(model) %>%
  summarize(vif = max(vif)) # all are fine

myvif

beta = m %>%
  map_dfr(~tidier(.), .id = "model") %>%

  mutate(across(.cols = c(estimate, se),
                .fns = ~scales::number(.x, accuracy = 0.001, scale_cut = scales::cut_short_scale()))) %>%
  mutate(value = paste0(estimate, stars, "<br>(",se,")"))

stat = m %>%
  map_dfr(~broom::glance(.), .id = "model") %>%
  left_join(by = c("model"), y = myvif) %>%
  mutate(
    across(.cols = c(r.squared, adj.r.squared, statistic, vif),
           .fns = ~scales::number(.x, accuracy = 0.01) %>% str_trim(side = "both")),
    sigma = scales::number(sigma, accuracy = 0.001),
    df = as.character(df),
    nobs = as.character(nobs)) %>%
  mutate(statistic = paste0(statistic, gtools::stars.pval(p.value))) %>%
  select(model, all_of(c("sigma", "r.squared", "adj.r.squared", "vif", "statistic", "df", "nobs"))) %>%
  pivot_longer(cols = c(sigma, r.squared, adj.r.squared, vif, statistic, df, nobs), names_to = "term", values_to = "value")


.levels = c("total" = "Total Social<br>Infrastructure",
            "community_space" = "Community<br>Spaces","place_of_worship" = "Places<br>of Worship",
            "social_business" = "Social<br>Businesses",  "park" = "Parks")

# trans = tibble(term = "trans", model = c("social_capital", "bridging", "bonding", "linking")) %>%
#   expand_grid(group = c("total", "subtypes")) %>%
#   mutate(value = "log(y + 1)")

trans = tibble(
  term = "trans",
  model = c("m1","m2","m3","m4","m5"),
  value = "log(y + 1)"
)


table = bind_rows(trans, beta, stat, myvif) %>%
  pivot_wider(id_cols = c(term), names_from = model, values_from = value, values_fill = list(value = "")) %>%
  mutate(group = case_when(
    term == "trans" ~ "trans",
    str_detect(term, "name") ~ "fe",
    term %in% c("sigma", "(Intercept)", "r.squared", "adj.r.squared","vif", "statistic", "df", "nobs") ~ "gof",
    TRUE ~ "cov")) %>%
  filter(!is.na(term))


# Goodness of Fit Stats
table0 = table %>%
  filter(group == "gof") %>%
  mutate(term = term %>% dplyr::recode_factor(
    "(Intercept)" = 'Constant',
    "sigma" = "Sigma (Avg. Error)",
    "r.squared" = "R<sup>2</sup>",
    "adj.r.squared" = "Adj. R<sup>2</sup",
    "vif" = "Max VIF",
    "statistic" = "F statistic (df)",
    "df" = "Degrees of Freedom",
    "nobs" = "N"
  )) %>%
  arrange(term) %>% select(-group)


table3 = table %>%
  filter(group == "trans") %>%
  mutate(term = term %>% dplyr::recode_factor(
    "trans" = "Transformation")) %>%
  select(-group)


# Drop fixed effects
table1 = table %>%
  filter(group == "cov") %>%
  mutate(term = term %>% dplyr::recode_factor(
    "log(total + 1)" = "log(Total SI)",
    "log(community_space + 1)" = "log(Community Spaces)",
    "log(place_of_worship + 1)" = "log(Places of Worship)",
    "log(social_business + 1)" = "log(Social Businesses)",
    "log(park + 1)" = "log(Parks)",
    "pop_density_block" = "Pop Density",
    "black_block"= "% Black",
    "hisplat_block" ="% Hispanic/Latino",
    "asian_block" = "% Asian",
    "some_college_bg" = "% Some College",
    "over_65_bg" = "% Over Age 65",
    "unemployment_bg" = "Unemployment Rate",
    "income_0_60K_bg" = "% Income Under 60K",
    "median_household_income_bg" = "Median Household Income"
  )) %>%
  arrange(term) %>% select(-group)

# Get Fixed Effects
table2 = table %>%
  filter(group == "fe") %>%
  rowwise() %>%
  mutate(term = term %>% str_remove("name") %>%
           str_replace_all("[_]", " ") %>% str_trim("both") %>%
           str_split(" ") %>% unlist() %>%
           map(~paste0(str_sub(.x, 0,1) %>% toupper(), str_sub(.x, 2, -1))) %>%
           paste0(collapse = " ")) %>%
  mutate(term = case_when(term == "Dc" ~ "DC", term == "La" ~ "LA", TRUE ~ term)) %>%
  arrange(term) %>% select(-group) %>%
  ungroup()


bind_rows(table3,table1, table2, table0) %>%
  select(Term = term,
         "Social Infrastructure Overall" = m1,
         "Community Spaces" = m2,
         "Places of Worship" = m3,
         "Social Businesses" = m4,
         "Parks" = m5) %>%
  write_csv("viz/table_models_si.csv")


read_csv("viz/table_models_si.csv") %>%
  knitr::kable(format = "html", escape = FALSE) %>%
  cat(file = "viz/table_models_si.html", sep = "\n")

# MODELING SC #####################################

# We want to figure out,
# what's the best way to model these values

library(dplyr)
library(readr)
library(tidyr)
library(nlme)
library(broom)
library(purrr)

# Let's use overall social capital and overall social infrastructure as our test case
d =   read_rds("viz/dataset_tract.rds") %>%
  na.omit() %>%
  pivot_longer(
    cols = c(social_capital, bonding, bridging, linking),
    names_to = "outcome", values_to = "value") %>%
  filter(outcome == "social_capital") %>%
  mutate(constant = 1)

# Make a logit function with an optional adjustment for 0 cases
logit = function(p, adj = 0) { log( (p + adj) / (1 - (p - adj) ))}

## check 1: logit? ##################################

# should I logit transform the outcome variable?
# One version of the model without the transformation
m1 = d %>%
  lm(formula = value ~ log(total + 1) +
       log(pop_density) +
       logit(black, 0.01) +
       logit(hisplat, 0.01) +
       logit(asian, 0.01) +
       log(median_household_income) +
       logit(income_0_60K, 0.01) +
       logit(some_college, 0.01) +
       logit(over_65, 0.01) +
       log(unemployment + 1) +
       name,
     data = .)
# One version of the model with the transformation
m2 = d %>%
  lm(formula = logit(value) ~ log(total + 1) +
       log(pop_density) +
       logit(black, 0.01) +
       logit(hisplat, 0.01) +
       logit(asian, 0.01) +
       log(median_household_income) +
       logit(income_0_60K, 0.01) +
       logit(some_college, 0.01) +
       logit(over_65, 0.01) +
       log(unemployment + 1) +
       name,
     data = .)

list(m1,m2) %>%
  map_dfr(~broom::glance(.))

# Doesn't really look like a logit improves the model fit,
# but it doesn't hurt it terribly either.
# Given the conceptual benefits of not going beyond 0 or 1,
# I'm going to stick with the logit transformation.


## check 2: colinearity #####################

# Even in the most heavily specified model,
# No real colinearity issues here.
read_rds("viz/dataset_tract.rds") %>%
  na.omit() %>%
  pivot_longer(
    cols = c(social_capital, bonding, bridging, linking),
    names_to = "outcome", values_to = "value") %>%
  filter(outcome == "social_capital") %>%
  mutate(constant = 1) %>%
  lm(formula = logit(value) ~
       log(community_space + 1) +
       log(place_of_worship + 1) +
       log(social_business + 1) + log(park + 1) +
       log(pop_density) +
       logit(black, 0.01) +
       logit(hisplat, 0.01) +
       logit(asian, 0.01) +
       log(median_household_income) +
       logit(income_0_60K, 0.01) +
       logit(some_college, 0.01) +
       logit(over_65, 0.01) +
       log(unemployment + 1) +
       name
  ) %>%
  car::vif() %>%
  { .[,3]^2 } %>%
  range()

read_rds("viz/dataset_tract.rds") %>%
  na.omit() %>%
  pivot_longer(
    cols = c(social_capital, bonding, bridging, linking),
    names_to = "outcome", values_to = "value") %>%
  filter(outcome == "bonding") %>%
  mutate(constant = 1) %>%
  lm(formula = logit(value) ~
       log(community_space + 1) +
       log(place_of_worship + 1) +
       log(social_business + 1) + log(park + 1) +
       log(pop_density) +
       logit(black, 0.01) +
       logit(hisplat, 0.01) +
       logit(asian, 0.01) +
       log(median_household_income) +
       logit(income_0_60K, 0.01) +
       logit(some_college, 0.01) +
       logit(over_65, 0.01) +
       log(unemployment + 1) +
       name
  ) %>%
  car::vif() %>%
  { .[,3]^2 } %>%
  range()

read_rds("viz/dataset_tract.rds") %>%
  na.omit() %>%
  pivot_longer(
    cols = c(social_capital, bonding, bridging, linking),
    names_to = "outcome", values_to = "value") %>%
  filter(outcome == "bridging") %>%
  mutate(constant = 1) %>%
  lm(formula = logit(value) ~
       log(community_space + 1) +
       log(place_of_worship + 1) +
       log(social_business + 1) + log(park + 1) +
       log(pop_density) +
       logit(black, 0.01) +
       logit(hisplat, 0.01) +
       logit(asian, 0.01) +
       log(median_household_income) +
       logit(income_0_60K, 0.01) +
       logit(some_college, 0.01) +
       logit(over_65, 0.01) +
       log(unemployment + 1) +
       name
  ) %>%
  car::vif() %>%
  { .[,3]^2 } %>%
  range()

read_rds("viz/dataset_tract.rds") %>%
  na.omit() %>%
  pivot_longer(
    cols = c(social_capital, bonding, bridging, linking),
    names_to = "outcome", values_to = "value") %>%
  filter(outcome == "linking") %>%
  mutate(constant = 1) %>%
  lm(formula = logit(value) ~
       log(community_space + 1) +
       log(place_of_worship + 1) +
       log(social_business + 1) + log(park + 1) +
       log(pop_density) +
       logit(black, 0.01) +
       logit(hisplat, 0.01) +
       logit(asian, 0.01) +
       log(median_household_income) +
       logit(income_0_60K, 0.01) +
       logit(some_college, 0.01) +
       logit(over_65, 0.01) +
       log(unemployment + 1) +
       name
  ) %>%
  car::vif() %>%
  { .[,3]^2 } %>%
  range()

## check 3: fixed effects or random? #############################

m1 = lme(fixed = logit(value) ~ log(total + 1) +
           log(pop_density) +
           logit(black, 0.01) +
           logit(hisplat, 0.01) +
           logit(asian, 0.01) +
           log(median_household_income) +
           logit(income_0_60K, 0.01) +
           logit(some_college, 0.01) +
           logit(over_65, 0.01) +
           log(unemployment + 1) +
           name,
         random = ~ 1 | constant,
         data = d)

m2 = lme(fixed = logit(value) ~ log(total + 1) +
           log(pop_density) +
           logit(black, 0.01) +
           logit(hisplat, 0.01) +
           logit(asian, 0.01) +
           log(median_household_income) +
           logit(income_0_60K, 0.01) +
           logit(some_college, 0.01) +
           logit(over_65, 0.01) +
           log(unemployment + 1),
         # name,
         random = ~ 1 | name,
         data = d)

# Using random effects does not meaningfully improve the log-likelihood,
# at least not on its own.
lmtest::lrtest(m1,m2)


## check 4: hierarchical model, fixed effects, or random? ########################

# Let's make a basic fixed effects model
m1 = lme(fixed = logit(value) ~ log(total + 1) +
           log(pop_density) +
           logit(black, 0.01) +
           logit(hisplat, 0.01) +
           logit(asian, 0.01) +
           log(median_household_income) +
           logit(income_0_60K, 0.01) +
           logit(some_college, 0.01) +
           logit(over_65, 0.01) +
           log(unemployment + 1) +
           name,
         # This is a trick that produces a standard fixed effects model.
         # You can check against lm()
         random = ~ 1 | constant,
         data = d)

# Let's make a basic random effects model,
# with random effects by city
m2 = lme(fixed = logit(value) ~ log(total + 1) +
           log(pop_density) +
           logit(black, 0.01) +
           logit(hisplat, 0.01) +
           logit(asian, 0.01) +
           log(median_household_income) +
           logit(income_0_60K, 0.01) +
           logit(some_college, 0.01) +
           logit(over_65, 0.01) +
           log(unemployment + 1),
         # name,
         random = ~ 1 | name,
         data = d)

# Let's make a hierarchical random effects model,
# where we allow both the intercept AND the slope of social infrastructure
# to vary by city,
m3 = lme(fixed = logit(value) ~ log(total + 1) +
           log(pop_density) +
           logit(black, 0.01) +
           logit(hisplat, 0.01) +
           logit(asian, 0.01) +
           log(median_household_income) +
           logit(income_0_60K, 0.01) +
           logit(some_college, 0.01) +
           logit(over_65, 0.01) +
           log(unemployment + 1),
         # name,
         random = ~ log(total + 1) | name,
         data = d)
# Does the hierarchical model meaningfully improve on the fixed effects model?
lmtest::lrtest(m1,m3) # yes
# Does the hierarchical model meaningfully improve on the basic random effects model?
lmtest::lrtest(m2,m3) # yes
# Look at them all together
lmtest::lrtest(m1,m2,m3)

# Great. Let's try this hierarchical stuff.

## check 5: hierarchical effects with subtypes #######################


# Let's use overall social capital and social infrastructure subtypes as our test case
# This is the most heavily specified model.
# It tends to only converge when you
# (1) normalize all predictors, and
# (2) increase from 50 max iterations to 100 max iterations
# It should be fine to rescale the predictors, because again,
# the beta coefficients will not be interpretable anyways, due to the log and logit scales.
# We'll do the interpretation using predictions.
m4 = lme(fixed = logit(value) ~
           scale(log(community_space + 1)) +
           scale(log(place_of_worship + 1)) +
           scale(log(social_business + 1)) +
           scale(log(park + 1)) +
           scale(log(pop_density)) +
           scale(logit(black, 0.01)) +
           scale(logit(hisplat, 0.01)) +
           scale(logit(asian, 0.01)) +
           scale(log(median_household_income)) +
           scale(logit(income_0_60K, 0.01)) +
           scale(logit(some_college, 0.01)) +
           scale(logit(over_65, 0.01)) +
           scale(log(unemployment + 1)),
         # name,
         random = ~ scale(log(community_space + 1)) + scale(log(place_of_worship + 1)) +
           scale(log(social_business + 1)) + scale(log(park + 1)) | name,
         data = d,
         control = lmeControl(maxIter = 100, msMaxIter = 100))

## check 6: predictions ######################

# Can I successfully code predictions with this model,
# supplying NORMAL predictor values, not transformed?
m4

d %>%
  group_by(name) %>%
  summarize(
    across(.cols = c(
      community_space, place_of_worship, social_business, park,
      pop,
      pop_density, black, hisplat, asian,
      median_household_income, income_0_60K, some_college,
      over_65, unemployment),
      .f = ~median(.x, na.rm = TRUE)
    )
  ) %>%
  predict(object = m4, newdata = ., level = 1) %>%
  # Backtransform from logit.
  plogis()

# Excellent - works as expected.

rm(list = ls())

## run models ########################################

### setup #########################

library(dplyr)
library(readr)
library(broom)
library(tidyr)
library(nlme)

# Make a home for our models
dir.create("models")

# Make a logit function with an optional adjustment for 0 cases
logit = function(p, adj = 0) { log( (p + adj) / (1 - (p - adj) ))}

dat = read_rds("viz/dataset_tract.rds") %>%
  na.omit() %>%
  pivot_longer(
    cols = c(social_capital, bonding, bridging, linking),
    names_to = "outcome", values_to = "value")

### total models ##############################

# Run models testing effect of total social infrastrucutre

# Total vs. social capital
m1 = dat %>%
  filter(outcome == "social_capital") %>%
  lme(fixed = logit(value) ~
        scale(log(total + 1)) +
        scale(log(pop_density)) +
        scale(logit(black, 0.01)) +
        scale(logit(hisplat, 0.01)) +
        scale(logit(asian, 0.01)) +
        scale(log(median_household_income)) +
        scale(logit(income_0_60K, 0.01)) +
        scale(logit(some_college, 0.01)) +
        scale(logit(over_65, 0.01)) +
        scale(log(unemployment + 1)),
      # name,
      random = ~ scale(log(total + 1)) | name,
      data = .)
# Save and remove
m1 %>% saveRDS("models/m1.rds")
remove(m1)

# Total vs. bonding social capital
m2 = dat %>%
  filter(outcome == "bonding") %>%
  lme(fixed = logit(value) ~
        scale(log(total + 1)) +
        scale(log(pop_density)) +
        scale(logit(black, 0.01)) +
        scale(logit(hisplat, 0.01)) +
        scale(logit(asian, 0.01)) +
        scale(log(median_household_income)) +
        scale(logit(income_0_60K, 0.01)) +
        scale(logit(some_college, 0.01)) +
        scale(logit(over_65, 0.01)) +
        scale(log(unemployment + 1)),
      # name,
      random = ~ scale(log(total + 1)) | name,
      data = .)
# Save and remove
m2 %>% saveRDS("models/m2.rds")
remove(m2)


# Total vs. bridging social capital
m3 = dat %>%
  filter(outcome == "bridging") %>%
  lme(fixed = logit(value) ~
        scale(log(total + 1)) +
        scale(log(pop_density)) +
        scale(logit(black, 0.01)) +
        scale(logit(hisplat, 0.01)) +
        scale(logit(asian, 0.01)) +
        scale(log(median_household_income)) +
        scale(logit(income_0_60K, 0.01)) +
        scale(logit(some_college, 0.01)) +
        scale(logit(over_65, 0.01)) +
        scale(log(unemployment + 1)),
      # name,
      random = ~ scale(log(total + 1)) | name,
      data = .)
# Save and remove
m3 %>% saveRDS("models/m3.rds")
remove(m3)



# Total vs. linking social capital
m4 = dat %>%
  filter(outcome == "linking") %>%
  lme(fixed = logit(value) ~
        scale(log(total + 1)) +
        scale(log(pop_density)) +
        scale(logit(black, 0.01)) +
        scale(logit(hisplat, 0.01)) +
        scale(logit(asian, 0.01)) +
        scale(log(median_household_income)) +
        scale(logit(income_0_60K, 0.01)) +
        scale(logit(some_college, 0.01)) +
        scale(logit(over_65, 0.01)) +
        scale(log(unemployment + 1)),
      # name,
      random = ~ scale(log(total + 1)) | name,
      data = .)
# Save and remove
m4 %>% saveRDS("models/m4.rds")
remove(m4)

### subtype models ###########################

# run models testing effect of social infrastructure subtypes.

# SI subtypes vs. overall social capital
m5 = lme(
  data = dat %>%
    filter(outcome == "social_capital"),
  fixed = logit(value) ~
    scale(log(community_space + 1)) +
    scale(log(place_of_worship + 1)) +
    scale(log(social_business + 1)) +
    scale(log(park + 1)) +
    scale(log(pop_density)) +
    scale(logit(black, 0.01)) +
    scale(logit(hisplat, 0.01)) +
    scale(logit(asian, 0.01)) +
    scale(log(median_household_income)) +
    scale(logit(income_0_60K, 0.01)) +
    scale(logit(some_college, 0.01)) +
    scale(logit(over_65, 0.01)) +
    scale(log(unemployment + 1)),
  # name,
  random = ~ scale(log(community_space + 1)) +
    scale(log(place_of_worship + 1)) +
    scale(log(social_business + 1)) +
    scale(log(park + 1)) | name,
  # Add more iterations, because so many random effects
  control = lmeControl(maxIter = 100, msMaxIter = 100)
)
m5 %>% saveRDS("models/m5.rds")
remove(m5)


# SI subtypes vs. bonding social capital
m6 = lme(
  data = dat %>%
    filter(outcome == "bonding"),
  fixed = logit(value) ~
    log(community_space + 1) +
    log(place_of_worship + 1) +
    log(social_business + 1) +
    log(park + 1) +
    scale(log(pop_density)) +
    scale(logit(black, 0.01)) +
    scale(logit(hisplat, 0.01)) +
    scale(logit(asian, 0.01)) +
    scale(log(median_household_income)) +
    scale(logit(income_0_60K, 0.01)) +
    scale(logit(some_college, 0.01)) +
    scale(logit(over_65, 0.01)) +
    scale(log(unemployment + 1)),
  # name,
  random = ~ log(community_space + 1) +
    log(place_of_worship + 1) +
    log(social_business + 1) +
    log(park + 1) | name,
  # Add more iterations, because so many random effects
  control = lmeControl(maxIter = 150, msMaxIter = 150)
)
m6 %>% saveRDS("models/m6.rds")
remove(m6)
# When modeling effects of social infrastructure subtypes on bonding social capital,
# to achieve model convergence,
# we had to unscale the per-city random effects for each type of social infrastructure.


# SI subtypes vs. bridging social capital
m7 = lme(
  data = dat %>%
    filter(outcome == "bridging"),
  fixed = logit(value) ~
    scale(log(community_space + 1)) +
    scale(log(place_of_worship + 1)) +
    scale(log(social_business + 1)) +
    scale(log(park + 1)) +
    scale(log(pop_density)) +
    scale(logit(black, 0.01)) +
    scale(logit(hisplat, 0.01)) +
    scale(logit(asian, 0.01)) +
    scale(log(median_household_income)) +
    scale(logit(income_0_60K, 0.01)) +
    scale(logit(some_college, 0.01)) +
    scale(logit(over_65, 0.01)) +
    scale(log(unemployment + 1)),
  # name,
  random = ~ scale(log(community_space + 1)) +
    scale(log(place_of_worship + 1)) +
    scale(log(social_business + 1)) +
    scale(log(park + 1)) | name,
  # Add more iterations, because so many random effects
  control = lmeControl(maxIter = 100, msMaxIter = 100)
)
m7 %>% saveRDS("models/m7.rds")
remove(m7)


m7 = read_rds("models/m7.rds")

# SI subtypes vs. linking social capital
m8 = lme(
  data = dat %>%
    filter(outcome == "linking"),
  fixed = logit(value) ~
    scale(log(community_space + 1)) +
    scale(log(place_of_worship + 1)) +
    scale(log(social_business + 1)) +
    scale(log(park + 1)) +
    scale(log(pop_density)) +
    scale(logit(black, 0.01)) +
    scale(logit(hisplat, 0.01)) +
    scale(logit(asian, 0.01)) +
    scale(log(median_household_income)) +
    scale(logit(income_0_60K, 0.01)) +
    scale(logit(some_college, 0.01)) +
    scale(logit(over_65, 0.01)) +
    scale(log(unemployment + 1)),
  # name,
  random = ~
    scale(log(community_space + 1)) +
    scale(log(place_of_worship + 1)) |
    # log(social_business + 1)
    # log(park + 1) |
  name,
  # Add more iterations, because so many random effects
  control = lmeControl(maxIter = 150, msMaxIter = 150)
)

# To aid hierarchical model convergence, we normalized all predictors post-transformation.

# To achieve model convergence, some models needed specific adjustments.

# Our model of social infrastructure subtype effects on linking social capital
# could not include per-city random effects ffor every type of social infrastructure -
# although all direct effects could still be included.
# We had to drop per-city random effects for social businesses and parks,
# which we had no hypotheses about; we rationalized these
# have the weakest conceptual link and so are okay to not model.

# Our model of social infrastructure subtype effects on bonding social capital
# could only converge when the direct and per-city random effects of social infrastructure subtypes
# were not normalized.

m8 %>% saveRDS("models/m8.rds")
remove(m8)

### combine #################################

# bundle these models into a list together

list(total_social_capital = read_rds("models/m1.rds"),
     total_bonding = read_rds("models/m2.rds"),
     total_bridging = read_rds("models/m3.rds"),
     total_linking = read_rds("models/m4.rds"),
     subtypes_social_capital = read_rds("models/m5.rds"),
     subtypes_bonding = read_rds("models/m6.rds"),
     subtypes_bridging = read_rds("models/m7.rds"),
     subtypes_linking = read_rds("models/m8.rds")) %>%
  saveRDS("models/models.rds")


### model fit ########################################

#install.packages("MuMIn")
library(MuMIn)
library(purrr)
library(dplyr)
library(readr)


logit = function(p, adj = 0) { log( (p + adj) / (1 - (p - adj) ))}

m = read_rds("models/models.rds")
m = list(m$total_social_capital,m$subtypes_social_capital,
         m$total_bonding,m$subtypes_bonding,
         m$total_bridging,m$subtypes_bridging,
         m$total_linking,m$subtypes_linking)

m %>%
  map_dfr(~MuMIn::r.squaredGLMM(object = .x) %>%
            as_tibble() %>%
            select(marginal = 1, condition = 2) %>%
            mutate(marginal = round(marginal, 2),
                   condition = round(condition, 2)), .id = "model") %>%
  t()

#install.packages("rcompanion")
# rcompanion::nagelkerke(fit = m)
# broom::glance(m)
# summary(m)

## TABLES ##########################

#install.packages("modelsummary")
library(modelsummary)
library(nlme)
library(dplyr)
library(readr)
library(pandoc)

m = read_rds("models/models.rds")

m = list(
  "Social Capital Overall" = m$total_social_capital,
  "Social Capital Overall" = m$subtypes_social_capital,
  "Bonding Social Capital" = m$total_bonding,
  "Bonding Social Capital" = m$subtypes_bonding,
  "Bridging Social Capital" = m$total_bridging,
  "Bridging Social Capital" = m$subtypes_bridging,
  "Linking Social Capital" = m$total_linking,
  "Linking Social Capital" = m$subtypes_linking
)

m %>%
  modelsummary(
    estimate = "{estimate}{stars}",
    output = "viz/table_hierarchical.docx",

    notes = c(
      paste0(
        "Statistical Significance: *** p < 0.001; ** p < 0.01; * p < 0.05. ",
        "Sigma: Residual Standard Error of Model. Approximates error term in model. ",
        "Rates: Each social infrastructure variable reflects the rate of sites per 1000 residents per sq.km. ",
        "Demographics: sourced from 2020 American Community Survey 5-year averages. ",
      "Rescaling: To aid convergence, each covariate was rescaled as Z-scores. ",
      "Random Effects: Each model includes random intercepts for each city, ",
      "plus per-city random effects of each type of social infrastructure in that model.",
      "Some models required minor adjustments to achieve convergence.",
      "For Model 4, social infrastructure rates had to remain unnormalized.",
      "Model 8 could not include per-city random effects for parks or social businesses."
      )
    ),
    coef_map = c(
      "scale(log(total + 1))" = "ln(SI Overall + 1)",
      "scale(log(community_space + 1))" = "ln(Community Spaces + 1)",
      "scale(log(place_of_worship + 1))" = "ln(Places of Worship + 1)",
      "scale(log(social_business + 1))" = "ln(Social Businesses + 1)",
      "scale(log(park + 1))" = "ln(Parks + 1)",
      "scale(log(pop_density))" = "ln(Pop. Density)",
      "scale(logit(black, 0.01))" = "logit(% Black)",
      "scale(logit(hisplat, 0.01))" = "logit(% Hispanic/Latino)",
      "scale(logit(asian, 0.01))" = "logit(% Asian)",
      "scale(log(median_household_income))" = "ln(Median Household Income)",
      "scale(logit(income_0_60K, 0.01))" = "logit(% Income 0-60K)",
      "scale(logit(some_college, 0.01))" = "logit(% Some College)",
      "scale(logit(over_65, 0.01))" = "logit(% Over Age 65)",
      "scale(ln(unemployment + 1))" = "ln(Unemployment Rate)",
      "(Intercept)" = "Constant",
      "SD (Intercept name)" = "SD (City-Intercepts)",
      "SD (scale(log(total + 1)) name)" = "SD (City-Slope of ln(SI Overall + 1))",
      "SD (scale(log(community_space + 1)) name)" = "SD (City-Slope of ln(Community Spaces + 1))",
      "SD (scale(log(place_of_worship + 1)) name)" = "SD (City-Slope of ln(Places of Worship + 1))",
      "SD (scale(log(social_business + 1)) name)" = "SD (City-Slope of ln(Social Businesses + 1))",
      "SD (scale(log(park + 1)) name)" = "SD (City-Slope of ln(Parks + 1))",
      # "Cor (Intercept~scale(log(total + 1)) name)" = "Cor (City Intercepts x Slope of ln(SI Overall + 1) )",
      # "Cor (Intercept~scale(log(community_space + 1)) name)" = "Cor (City Intercepts x Slope of ln(Community Spaces + 1) )",
      # "Cor (Intercept~scale(log(place_of_worship + 1)) name)" = "Cor (City Intercepts x Slope of ln(Places of Worship + 1) )",
      # "Cor (Intercept~scale(log(social_business + 1)) name)" = "Cor (City Intercepts x Slope of ln(Social Businesses + 1) )",
      # "Cor (Intercept~scale(log(park + 1)) name)" = "Cor (City Intercepts x Slope of ln(Social Businesses + 1) )",
      "SD (Observations)" = "SD (Residuals)"
    )
  )



#nlme::gsummary(m$total_bonding)
