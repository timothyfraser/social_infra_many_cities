#' @name `05_correction_factor.R`
#'
#' @description Script for analyzing dataset.


data("connect")
data("wkb_as_sf")
data("aea")
data("meta")

# Is Boston representative of other cities in our sample?
source("keys.R")
geo = connect()


validated = "https://raw.githubusercontent.com/timothyfraser/trust_but_verify/main/query/social_infrastructure_sites_boston.csv" %>%
  read_csv() %>%
  st_as_sf(coords = c("x","y"), crs = 4326) %>%
  st_transform(crs = aea) %>%
  select(id, name, type, status, source, geometry, zone)

# Let's get Boston Boundariess
bounds = "https://raw.githubusercontent.com/timothyfraser/trust_but_verify/main/shapes/neighborhoods.geojson" %>%
  read_sf() %>%
  st_as_sf(crs = 4326) %>%
  st_transform(crs = 4326)  %>%
  filter(name != "Harbor Islands")

# Cut cells that are mostly water or mostly not in Boston
cut = c(197, 198, 199, 200, 201, 202, 190, 191, 177,
        209, 210, 211, 218, 219, 222, 223, 224, 225, 226,
        1,6, 12, 81,100,112, 129, 38, 39, 50, 72, 3, 213, 214, 215,
        187, 188, 189, 178, 179, 172, 173, 19, 28, 51, 63, 93, 159, 13, 7, 8)

shapes = geo %>% tbl("data_grid") %>%
  filter(name == "boston") %>%
  # Get valid block and block group data
  filter(pop_density_block > 0) %>%
  select(name, cell,
         total:park,
         pop_density_block,
           white_block, black_block, hisplat_block, asian_block,
         median_household_income_bg,income_0_60K_bg,
           some_college_bg, over_65_bg, unemployment_bg,
         social_capital, bonding, bridging, linking, geometry) %>%
  filter(!cell %in% !!cut) %>%
  collect() %>%
  wkb_as_sf() %>%
  st_as_sf(crs = 4326) %>%
  st_transform(crs = aea)

# When we find any, how much should we up or downweight them?
ggplot() +
  geom_sf(data = bounds) +
  geom_sf(data = shapes, fill = NA) +
  geom_sf_text(data = shapes, mapping = aes(label = cell)) +
  geom_sf(data = validated, mapping = aes(color = zone)) +
  theme(legend.position = "none")

validated %>%
  st_join(y = shapes %>% select(cell, geometry), left= TRUE) %>%
  as_tibble() %>%
  group_by(cell) %>%
  summarize(
    total = sum(type %in% c("Community Spaces", "Parks", "Social Businesses", "Places of Worship") & source == "humancoding_fall"),
    community_space = sum(type == "Community Spaces" & source == "humancoding_fall"),
    park = sum(type == "Parks" & source == "humancoding_fall"),
    social_business = sum(type == "Social Businesses" & source == "humancoding_fall"),
    place_of_worship = sum(type == "Places of Worship" & source == "humancoding_fall")
  ) %>%
  right_join(y = shapes %>% select(cell, pop_density_block), by = "cell") %>%
  # Calculate the rate per 1000 persons per square kilometer
  mutate(across(.cols = c(total:place_of_worship), .fns = ~.x / pop_density_block * 1000)) %>%
  pivot_longer(cols = c(total:place_of_worship), names_to = "type", values_to = "human") %>%
  select(-pop_density_block, -geometry) %>%
  right_join(by = c("cell", "type"), y = shapes %>%
               as_tibble() %>% select(-geometry) %>%
               pivot_longer(cols = c(total:park), names_to = "type", values_to = "api")) %>%
  mutate(human = if_else(is.na(human), 0, human) ) %>%
  mutate(nonwhite_block = 1 - white_block) %>%
  saveRDS("viz/comparison_dataset.rds")

dbDisconnect(geo); rm(list = ls()); gc()


tidier = function(m){
  broom::tidy(m) %>%
    rename(se = std.error, stat = statistic, p_value = p.value) %>%
    mutate(stars = gtools::stars.pval(p_value))
}

library(texreg)
library(dplyr)
library(readr)
library(randomForest)
library(caret)
library(broom)

d = read_rds("viz/comparison_dataset.rds")

d %>%
  na.omit() %>%
  filter(type != "total") %>%
  select(api, human, type, pop_density_block, black_block:unemployment_bg) %>%
  randomForest(formula = api ~ ., data = ., proximity=TRUE)
# Random forest is no good.

d = read_rds("viz/comparison_dataset.rds")

d %>%
  ggplot(mapping = aes(y = exp(human), x = api )) +
  geom_point() +
  geom_smooth(method = "lm")

# When the api finds a site, HOW MUCH is it off?

d %>%
  filter(type != "total") %>%
  na.omit() %>%
  lm(formula = human ~ poly(api, 2) * type +
       pop_density_block  +
       poly(nonwhite_block, 2)  * type +
       poly(some_college_bg, 2) * type +
       poly(median_household_income_bg, 3) * type
       #poly(over_65_bg, 2) * type
       #poly(unemployment_bg, 2) * type
       ) %>%
  summary()


d %>%
  filter(type != "total") %>%
  na.omit() %>%
  lm(formula = api ~ human * pop_density_block +
       nonwhite_block +
       poly(median_household_income_bg, 3) * type
       ) %>%
  glance()

d %>%
  filter(type == "total") %>%
  na.omit() %>%
  lm(formula = api ~ human * pop_density_block +
       nonwhite_block + some_college_bg +
       poly(median_household_income_bg, 3)
  ) %>%
  glance()

d %>%
  filter(type == "total") %>%
  na.omit() %>%
  lm(formula = human ~ api * pop_density_block +
       nonwhite_block + some_college_bg +
       poly(median_household_income_bg, 3)
  ) %>%
  glance()


d %>%
  filter(type == "park") %>%
  na.omit() %>%
  lm(formula = human ~ api * pop_density_block +
       nonwhite_block + some_college_bg +
       poly(median_household_income_bg, 3)
  ) %>%
  glance()









m = d %>%

  filter(type != "total") %>%
  na.omit() %>%
  lm(formula = api ~ human +
       pop_density_block +
       black_block +
       hisplat_block +
       asian_block +
       some_college_bg +
       poly(median_household_income_bg, 3)
     )
m %>% broom::glance()

predict(m, se.fit = FALSE)

d %>%
  filter(type == "total") %>%
  na.omit() %>%
  lm(formula = exp(human) ~ exp(api) +
       pop_density_block +
       black_block +
       hisplat_block +
       asian_block +
       some_college_bg +
       poly(median_household_income_bg, 4)
  ) %>%
  glance()


d %>%
  filter(type == "total") %>%
  na.omit() %>%
  lm(formula = exp(api) ~ exp(human) +
       pop_density_block +
       black_block +
       hisplat_block +
       asian_block +
       some_college_bg +
       poly(median_household_income_bg, 3)
  ) %>%
  glance()




d %>%
  filter(type != "total") %>%
  na.omit() %>%
  lm(formula = human ~ poly(api, 2) * type +
       I(pop_density_block * some_college_bg)
       ) %>%
  summary()




screenreg(l = list())

grid %>%
  lm(formula = log(ground_truthed + 1) ~ log(google_api + 1) + type + poly(pop_density_int, 2) +
       poly(pop_black, 2) + poly(pop_asian, 2) + poly(pop_hisplat, 2) + poly(pop_women, 2) +
       poly(pop_some_college, 2) + poly(pop_age_65_plus, 2) + median_income +
       poly(pop_unemployed,2) ) %>%
  glance()

grid %>%
  lm(formula = log(online + 1) ~ log(google_api + 1) + type + poly(pop_density_int, 2) +
       poly(pop_black, 2) + poly(pop_asian, 2) + poly(pop_hisplat, 2) + poly(pop_women, 2) +
       poly(pop_some_college, 2) + poly(pop_age_65_plus, 2) + median_income +
       poly(pop_unemployed,2) ) %>%
  glance()

