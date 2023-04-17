#' @name `05_correction_factor.R`
#'
#' @description Script for analyzing dataset.

data("connect")
data("wkb_as_sf")
data("aea")
geo = connect()

dat = geo %>%
  tbl("data_grid") %>%
  filter(name == "boston") %>%
  collect() %>%
  wkb_as_sf() %>%
  st_as_sf(crs = 4326) %>%
  st_transform(crs = aea)

# Let's get the finalized sites
validated = "https://raw.githubusercontent.com/timothyfraser/trust_but_verify/main/query/social_infrastructure_sites_boston.csv" %>%
  read_csv() %>%
  st_as_sf(coords = c("x","y"), y = )

validated


grid = "https://raw.githubusercontent.com/timothyfraser/trust_but_verify/main/data/figure_rates.csv" %>%
  read_csv() %>%
  filter(figure == 3) %>%
  filter(zone == "validation") %>%
  mutate(panel = panel %>% tolower() %>% str_replace_all(" ", "_")) %>%
  pivot_wider(
    id_cols = c(cell_id, type, pop_density_int),
    names_from = panel, values_from = c(rate)) %>%
  # Join in covariates
  left_join(
    by = "cell_id",
    y =  "https://raw.githubusercontent.com/timothyfraser/trust_but_verify/main/shapes/grid_covariates.geojson" %>%
              read_sf() %>% as_tibble() %>% select(-pop_density_int)
  )
grid %>% glimpse()

tidier = function(m){ broom::tidy(m) %>% rename(se = std.error, stat = statistic, p_value = p.value) %>% mutate(stars = gtools::stars.pval(p_value))}
library(broom)

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

