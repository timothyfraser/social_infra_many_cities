#' @name `05_correction_factor.R`
#'
#' @description Script for analyzing dataset.

# DATASET ##########################

data("connect")
data("wkb_as_sf")
data("aea")
data("meta")

# Is Boston representative of other cities in our sample?
source("keys.R")
geo = connect()

library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(broom)
library(tidyr)
library(stringr)

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

joined = validated %>%
  st_join(y = shapes %>% select(cell, geometry), left= TRUE) %>%
  as_tibble()

bind_rows(
  # Get all validated sites
  joined %>%
    group_by(cell) %>%
    summarize(
      total = sum(type %in% c("Community Spaces", "Parks", "Social Businesses", "Places of Worship") ),
      community_space = sum(type == "Community Spaces"),
      park = sum(type == "Parks"),
      social_business = sum(type == "Social Businesses"),
      place_of_worship = sum(type == "Places of Worship"),
      # Clarify type
      status = "checked"
    ) %>%
    pivot_longer(cols = c(total:place_of_worship), names_to = "type", values_to = "human"),

  # Get all sites visited in person
  joined %>%
    group_by(cell) %>%
    summarize(

      total = sum(type %in% c("Community Spaces", "Parks", "Social Businesses", "Places of Worship") & str_detect(status, "visited")),
      community_space = sum(type == "Community Spaces"  & str_detect(status, "visited")),
      park = sum(type == "Parks" & str_detect(status, "visited")),
      social_business = sum(type == "Social Businesses"  & str_detect(status, "visited")),
      place_of_worship = sum(type == "Places of Worship"  & str_detect(status, "visited")),
      # Clarify type
      status = "visited"
    ) %>%
    pivot_longer(cols = c(total:place_of_worship), names_to = "type", values_to = "human")
) %>%
  inner_join(y = shapes %>% as_tibble() %>% select(cell, pop_density_block), by = "cell") %>%
  pivot_wider(id_cols = c(cell, type), names_from = status, values_from = human) %>%
  #select(-pop_density_block, -geometry) %>%
  right_join(by = c("cell", "type"), y = shapes %>%
               as_tibble() %>% select(-geometry) %>%
               pivot_longer(cols = c(total:park), names_to = "type", values_to = "api")) %>%
  mutate(across(.cols = c(checked, visited), .fns = ~if_else(is.na(.x), 0, .x) )) %>%
  # Calculate the rate per 1000 persons per square kilometer
  mutate(across(.cols = c(checked, visited), .fns = ~.x / pop_density_block * 1000)) %>%
  mutate(nonwhite_block = 1 - white_block) %>%
  saveRDS("viz/comparison_dataset.rds")


dbDisconnect(geo); rm(list = ls()); gc()


# Table: Variation Explained ###########################

library(dplyr)
library(readr)
library(broom)
library(purrr)
library(texreg)

d = read_rds("viz/comparison_dataset.rds") %>%
  select(name, cell, type, checked, visited, api, pop_density_block, nonwhite_block, some_college_bg, median_household_income_bg)  %>%
  na.omit()

# Get the cells which were ground truthed
groundtruthed =  d %>% filter(visited != 0) %>% with(cell) %>% unique()

# Verify the correlation between the overall validated sites (checked) and the ground truthed sites (visited)
d %>%
  filter(cell %in% groundtruthed) %>%
  summarize(cor = cor(checked, visited))


# Construct best fitting models
bind_rows(
  d %>% split(.$type) %>%
    map(~lm(formula = checked ~ poly(api, 2) + pop_density_block + nonwhite_block + some_college_bg + poly(median_household_income_bg, 3), data = .), .id = "model") %>%
    map_dfr(~glance(.), .id = "model"),

  d %>% split(.$type) %>%
    map(~lm(formula = log(checked + 1) ~ poly(api, 2) + pop_density_block + nonwhite_block + some_college_bg + poly(median_household_income_bg, 3), data = .), .id = "model") %>%
    map_dfr(~glance(.), .id = "model"),

  d %>% split(.$type) %>%
    map(~lm(formula = exp(checked) ~ poly(api, 2) + pop_density_block + nonwhite_block + some_college_bg + poly(median_household_income_bg, 3), data = .), .id = "model") %>%
    map_dfr(~glance(.), .id = "model"),

  d %>% split(.$type) %>%
    map(~lm(formula = I(checked^2) ~ poly(api, 2) + pop_density_block + nonwhite_block + some_college_bg + poly(median_household_income_bg, 3), data = .), .id = "model") %>%
    map_dfr(~glance(.), .id = "model"),

  d %>% split(.$type) %>%
    map(~lm(formula = I(checked^.5) ~ poly(api, 2) + pop_density_block + nonwhite_block + some_college_bg + poly(median_household_income_bg, 3), data = .), .id = "model") %>%
    map_dfr(~glance(.), .id = "model"),
  d %>% split(.$type) %>%
    map(~lm(formula = I(checked^.1) ~ poly(api, 2) + pop_density_block + nonwhite_block + some_college_bg + poly(median_household_income_bg, 3), data = .), .id = "model") %>%
    map_dfr(~glance(.), .id = "model"),
  d %>% split(.$type) %>%
    map(~lm(formula = I(checked^10) ~ poly(api, 2) + pop_density_block + nonwhite_block + some_college_bg + poly(median_household_income_bg, 3), data = .), .id = "model") %>%
    map_dfr(~glance(.), .id = "model"),
  d %>% split(.$type) %>%
    map(~lm(formula = I(checked^3) ~ poly(api, 2) + pop_density_block + nonwhite_block + some_college_bg + poly(median_household_income_bg, 3), data = .), .id = "model") %>%
    map_dfr(~glance(.), .id = "model"),
  d %>% split(.$type) %>%
    map(~lm(formula = I(checked^5) ~ poly(api, 2) + pop_density_block + nonwhite_block + some_college_bg + poly(median_household_income_bg, 3), data = .), .id = "model") %>%
    map_dfr(~glance(.), .id = "model"),


  .id = "trans"
) %>%
  mutate(trans = trans %>% dplyr::recode(!!!c("1" = "y", "2" =  "log(y + 1)", "3" =  "exp(y)", "4" = "y^2", "5" = "sqrt(y)", "6" = "y^0.1",  "7" = "y^10", "8"= "y^3","9" =  "y^5")) ) %>%
  mutate(model = model %>% dplyr::recode_factor( "total" = "Total", "community_space" = "Community Spaces","place_of_worship" = "Places of Worship", "social_business" = "Social Businesses",  "park" = "Parks")) %>%
  select(trans, model, rsq = r.squared) %>%
  mutate(rsq = scales::number(rsq, accuracy = 0.01)) %>%
  pivot_wider(id_cols = model, names_from = trans, values_from = rsq) %>%
  arrange(model) %>%
  write_csv("viz/table_correction_model_comparison.csv")



# Table: Models ################################
library(dplyr)
library(readr)
library(broom)
library(purrr)

read_rds("viz/comparison_dataset.rds") %>%
  select(name, cell, type, checked, visited, api, pop_density_block, nonwhite_block, some_college_bg, median_household_income_bg)  %>%
  na.omit() %>%
  split(.$type) %>%
  map(~{
    if(unique(.$type) == "park"){
    lm(formula = checked ~ poly(api, 2) +
         pop_density_block + nonwhite_block + some_college_bg +
         poly(median_household_income_bg, 3), data = .)
      }else{
        lm(formula = I(checked^10) ~ poly(api, 2) +
             pop_density_block + nonwhite_block + some_college_bg +
             poly(median_household_income_bg, 3), data = .) }
    }, .id = "model") %>%
  saveRDS("viz/models_correction.rds")


# Table: VIF #########################################

read_rds("viz/models_correction.rds") %>%
  map_dfr(~car::vif(.)^2 %>% .[,3] %>%
            tibble(vif = ., term = names(.)), .id = "model") %>%
  pivot_wider(id_cols = term, names_from = model, values_from = vif) %>%
  { bind_rows(
    ... = .,
    summarize(
      .data = ., term = "max",
      across(.cols = community_space:total, .fn = ~max(.x, na.rm = TRUE)))) }



tidier = function(m){
  broom::tidy(m) %>%
    rename(se = std.error, stat = statistic, p_value = p.value) %>%
    mutate(stars = gtools::stars.pval(p_value))
}

sci = function(x, digits = 3){
  require(stringr)
  require(dplyr)
  # Get scientific notation
  x = format(x, digits = digits, scientific = TRUE, big.mark = ",")
  # Make scientific notation prettier
  x = case_when(
    str_detect(x, "e[+]00|e[+]01|e[+]02|e[-]00|e[-]01|e[-]02") ~ as.character(as.numeric(x)),
                    TRUE ~ paste0(
                      str_remove(x, "e[+][0-9]{2}|e[-][0-9]{2}"),
                      " x 10<sup>",
                      as.integer(str_extract(str_extract(x, "e[+].*|e[-].*"), "[0-9]+")),
                      "</sup>") )
}



library(scales)
beta = read_rds("viz/models_correction.rds") %>%
  map_dfr(~tidier(.), .id = "model") %>%
  # Get scientific notation
  mutate(across(.cols = c(estimate, se), .fns = ~sci(.x))) %>%
  mutate(value = paste0(estimate, stars, "<br>(",se,")"))

stat = read_rds("viz/models_correction.rds") %>%
    map_dfr(~glance(.), .id = "model") %>%
  mutate(
    across(.cols = c(r.squared, adj.r.squared, statistic),
           .fns = ~scales::number(.x, accuracy = 0.01) %>% str_trim(side = "both")),
    sigma = sci(sigma),
    df = as.character(df),
    nobs = as.character(nobs)) %>%
  mutate(statistic = paste0(statistic, gtools::stars.pval(p.value))) %>%
  select(model, all_of(c("sigma", "r.squared", "adj.r.squared", "statistic", "df", "nobs"))) %>%
  pivot_longer(cols = c(sigma, r.squared, adj.r.squared, statistic, df, nobs), names_to = "term", values_to = "value")

.levels = c("total" = "Total Social<br>Infrastructure",
            "community_space" = "Community<br>Spaces","place_of_worship" = "Places<br>of Worship",
            "social_business" = "Social<br>Businesses",  "park" = "Parks")

trans = tibble(term = "trans", model = names(.levels)) %>%
  mutate(value = model %>% dplyr::recode(
    "total" = "y<sup>10</sup>",
    "community_space" = "y<sup>10</sup>",
    "place_of_worship" = "y<sup>10</sup>",
    "social_business" = "y<sup>10</sup>",
    "park" = "y"))

table = bind_rows(trans, beta, stat) %>%
  mutate(model = model %>% dplyr::recode_factor(!!!.levels )) %>%
  pivot_wider(id_cols = term, names_from = model, values_from = value) %>%
  mutate(term = term %>% dplyr::recode_factor(
    "trans" = "Transformation",
    "poly(api, 2)1" = "API Rate",
    "poly(api, 2)2" = "API Rate<sup>2</sup>",
    "pop_density_block" = "Pop Density",
    "nonwhite_block" = "% Nonwhite",
    "some_college_bg" = "% Some College",
    "poly(median_household_income_bg, 3)1" = "Income",
    "poly(median_household_income_bg, 3)2" = "Income<sup>2</sup>",
    "poly(median_household_income_bg, 3)3" = "Income<sup>3</sup>",
    "(Intercept)" = 'Constant',
    "sigma" = "Sigma (Avg. Error)",
    "r.squared" = "R<sup>2</sup>",
    "adj.r.squared" = "Adj. R<sup>2</sup",
    "statistic" = "F statistic (df)",
    "df" = "Degrees of Freedom",
    "nobs" = "N"
  )) %>%
  arrange(term) %>%
  select(Term = term, all_of(unname(.levels)))

library(knitr)
library(kableExtra)

kable(table, format = "html", caption = "Table X: Ordinary Least Squares Models of Social Infrastructure Rates",
#      col.names = rep(c(""), 6),
      align = c('l', rep('c', 5)), escape = FALSE) %>%
  kableExtra::kable_styling(full_width = TRUE) %>%
  #kableExtra::add_header_above(header = c("Transformation",  rep("y<sup>10</sup>", 4), "y"), escape = FALSE) %>%
  #kableExtra::add_header_above(header = c("Term", unname(.levels)), escape = FALSE) %>%
  kableExtra::add_header_above(header = c(" ", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5")) %>%
  kableExtra::pack_rows(group_label = "Dependent Variable", start_row = 1, end_row = 1) %>%
  kableExtra::pack_rows(group_label = "Independent Variable", start_row = 2, end_row = 3) %>%
  kableExtra::pack_rows(group_label = "Census Covariates", start_row = 4, end_row = 9) %>%
  kableExtra::pack_rows(group_label = "Constants", start_row = 10, end_row = 11) %>%
  kableExtra::pack_rows(group_label = "Goodness of Fit", start_row = 12, end_row = 16) %>%
  {for(i in 2:6){ . = kableExtra::column_spec(., column = i, bold = str_detect(unlist(table[,i]), "[*]")) }; .} %>%
  kableExtra::footnote(threeparttable = TRUE,
    general = paste0(
      "<b>Statistical Significance</b>: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.10. <b>Bold</b> highlights p < 0.05.",
      "<br>",
      "<b>Sigma</b>: Residual Standard Error of model. Can be used to approximate error term in model.",
      "<br>",
      "<b>Covariates:</b> Sourced from 2020 Decennial Census estimates, at lowest level possible per variable. Calculated as the median value among blocks/block groups overlapping each grid cell.",
      " Covariates derived from Block-level include Pop Density & % Nonwhite; from Block Group-level include % Some College & Income."),
    escape = FALSE) %>%
  cat(file = "viz/table_correction.html", sep = "\n")


browseURL("viz/table_correction.html")
