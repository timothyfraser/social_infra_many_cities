#' @name `05_analyze.R`
#'
#' @description Script for analyzing dataset.

## Data #########################################

library(sf)
library(dplyr)
library(DBI)

source("keys.R")
data("meta")
data("connect")
geo = connect()
geo %>% dbListTables()
dat = geo %>%
  tbl("data_grid") %>%
  filter(name %in% !!meta$top) %>%
  filter(pop_density_block > 0) %>%
  collect() %>%
  group_by(name) %>%
  mutate_at(vars(pop_density_block,
                 black_block, hisplat_block, median_household_income_bg,
                 some_college_bg, over_65_bg, unemployment_bg), list(~scale(.))) %>%
  ungroup()

dat %>%
  lm(formula = social_capital ~ total ) %>%
  summary()



dat %>%
  na.omit() %>%
  lm(formula = bonding ~ log(total + 1) +
       pop_density_block + black_block + hisplat_block + asian_block +
       median_household_income_bg + income_0_60K_bg +
       some_college_bg + over_65_bg + unemployment_bg + name) %>%
  broom::glance()


dat %>%
  na.omit() %>%
  lm(formula = bridging ~ log(total + 1) +
       pop_density_block + black_block + hisplat_block + asian_block +
       median_household_income_bg + income_0_60K_bg +
       some_college_bg + over_65_bg + unemployment_bg + name) %>%
  broom::glance()

dat %>%
  na.omit() %>%
  lm(formula = bridging ~ log(community_space + 1) +
       log(place_of_worship + 1) +
       log(social_business + 1) + log(park + 1) +
       pop_density_block + black_block + hisplat_block + asian_block +
       median_household_income_bg + income_0_60K_bg +
       some_college_bg + over_65_bg + unemployment_bg + name) %>%
  broom::glance()


dat %>%
  na.omit() %>%
  lm(formula = linking ~ log(community_space + 1) + log(place_of_worship + 1) +
       log(social_business + 1) + log(park + 1) +
       pop_density_block + black_block + hisplat_block + asian_block +
       median_household_income_bg + income_0_60K_bg +
       some_college_bg + over_65_bg + unemployment_bg + name) %>%
  broom::glance()


dat %>%
  na.omit() %>%
  lm(formula = social_capital ~ log(total + 1) +
       pop_density_block + black_block + hisplat_block + asian_block +
       median_household_income_bg + income_0_60K_bg +
       some_college_bg + over_65_bg + unemployment_bg + name) %>%
  broom::glance()

dat %>%
  na.omit() %>%
  lm(formula = social_capital ~ log(community_space + 1) +
       log(place_of_worship + 1) +
       log(social_business + 1) + log(park + 1) +
       pop_density_block +
       black_block + hisplat_block + asian_block +
       median_household_income_bg + income_0_60K_bg +
       some_college_bg + over_65_bg + unemployment_bg + name) %>%
  broom::glance()

## Logged or Not? ##########################################

m1 <- dat %>%
  lm(formula = total ~ bonding + bridging + linking +  pop_density_block +
       black_block + hisplat_block + median_household_income_bg +
       some_college_bg + over_65_bg + unemployment_bg + name)

m2 <- dat %>%
  lm(formula = log(total*10 + 1) ~ bonding + bridging + linking +  pop_density_block +
       black_block + hisplat_block + median_household_income_bg +
       some_college_bg + over_65_bg + unemployment_bg + name)

# Much better to use log of outcome
lmtest::lrtest(m1,m2)

remove(m1,m2)


## Models  #####################################################################################

m0 <- dat %>%
  lm(formula = log(total+ 1) ~ social_capital + pop_density_block + name)

m1 <- dat %>%
  lm(formula = log(total*10 + 1) ~ social_capital + pop_density_block +
       black_block + hisplat_block + median_household_income_bg + some_college_bg + over_65_bg + unemployment_bg + name)

m2 <- dat %>%
  lm(formula = log(total*10 + 1) ~ bonding + bridging + linking + pop_density_block + name)

m3 <- dat %>%
  lm(formula = log(total*10 + 1) ~ bonding + bridging + linking + pop_density_block +
       black_block + hisplat_block + median_household_income_bg + some_college_bg + over_65_bg + unemployment_bg + name)

texreg::htmlreg(
  list(m0,m1,m2,m3),
  omit.coef = "name",
  custom.header = list("Effects of Social Capital" = 1:2,
                       "Effects of Social Capital Subtypes" = 3:4),
  caption = "<b>OLS Models of Logged Rates of Social Infrastructure (per 10,000 residents per km<sup>2</sup>)</b><br><i>in 21,259 1 km<sup>2</sup> grid cells in 25 most populous US cities</i>",
  caption.above = TRUE,
  single.row = TRUE,bold = 0.10, stars = c(0.001, 0.01, 0.05, 0.10),
  custom.coef.map = list(
    "social_capital" = "Social Capital",
    "bonding" = "Bonding Social Capital",
    "bridging" = "Bridging Social Capital",
    "linking" = "Linking Social Capital",
    "pop_density_block" = "Population Density",
    "black_block" = "% Black",
    "hisplat_block" = "% Hispanic/Latino",
    "some_college_bg" = "% Some college or more",
    "over_65_bg" = "% Over Age 65",
    "median_household_income_bg" = "Median Household Income",
    "unemployment_bg" = "Unemployment Rate",
    "(Intercept)" = "Constant"),
  custom.note = "<b>Statistical Significance</b>: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.10.
  <br>
  <b>Estimates</b>: show projected increase in logged rates of social infrastructure as predictor increases by one standard deviation. Effect sizes can be compared.
  <b>Fixed Effects</b>: Fixed effects for 25 cities. <b>Collinearity</b>: All Variance Inflation Factor scores below 2.5.",
  file = "viz/table1.html")


remove(m0,m1,m2,m3)


## ANOVA #########################################################

dat %>%
  lm(formula = total ~ name) %>% summary()

dat %>%
  ggplot(mapping = aes(x = log(total*10 + 1), y = name)) +
  geom_point()

## Polynomials ###########################################################

datp = dat %>%
  select(cell, name, total, community_space, place_of_worship, social_business, park,
         social_capital:linking, pop_density_block,
         black_block, hisplat_block, median_household_income_bg, some_college_bg,
         over_65_bg, unemployment_bg) %>%
  na.omit()

m0 <- datp %>%
  lm(formula = log(total*10+ 1) ~ poly(social_capital, 2) + poly(pop_density_block, 2) + name)

m0 %>% summary()

m1 <- datp %>%
  lm(formula = log(total*10 + 1) ~ poly(social_capital, 2) + poly(pop_density_block, 2) +
       poly(black_block, 2) + poly(hisplat_block, 2) +
       poly(median_household_income_bg, 2) + poly(some_college_bg, 2) +
       poly(over_65_bg, 2) + poly(unemployment_bg, 2) +
       name)

m2 <- datp %>%
  lm(formula = log(total*10 + 1) ~ poly(bonding, 2) + poly(bridging, 2) + poly(linking, 2) +
       poly(pop_density_block, 2) + name)
m2 %>% summary()

m3 <- datp %>%
  lm(formula = log(total*10 + 1) ~ poly(bonding, 2) + poly(bridging, 2) + poly(linking, 2) + poly(pop_density_block,2) +
       poly(black_block, 2) + poly(hisplat_block, 2) + poly(median_household_income_bg, 2) + poly(some_college_bg,2) + poly(over_65_bg, 2) + poly(unemployment_bg,2) + name)

m3 %>% summary()

## Visualizing Overall Models ############################################

out <- bind_rows(
  # Total
  dat %>%
    lm(formula = log(total*10 + 1) ~ bonding + bridging + linking + pop_density +
         black + hisplat + median_household_income + some_college + over_65 + unemployment + name) %>%
    broom::tidy() %>%
    filter(term %in% c("bonding", "bridging", "linking")) %>%
    mutate(type = "total"),

  # Total
  dat %>%
    lm(formula = log(community_space*10 + 1) ~ bonding + bridging + linking + pop_density +
         black + hisplat + median_household_income + some_college + over_65 + unemployment + name) %>%
    broom::tidy() %>%
    filter(term %in% c("bonding", "bridging", "linking")) %>%
    mutate(type = "community_space"),

  dat %>%
    lm(formula = log(place_of_worship*10 + 1) ~ bonding + bridging + linking + pop_density +
         black + hisplat + median_household_income + some_college + over_65 + unemployment + name) %>%
    broom::tidy() %>%
    filter(term %in% c("bonding", "bridging", "linking")) %>%
    mutate(type = "place_of_worship"),

  dat %>%
    lm(formula = log(park*10 + 1) ~ bonding + bridging + linking + pop_density +
         black + hisplat + median_household_income + some_college + over_65 + unemployment + name) %>%
    broom::tidy() %>%
    filter(term %in% c("bonding", "bridging", "linking")) %>%
    mutate(type = "park"),


  dat %>%
    lm(formula = log(social_business*10 + 1) ~ bonding + bridging + linking + pop_density +
         black + hisplat + median_household_income + some_college + over_65 + unemployment + name) %>%
    broom::tidy() %>%
    filter(term %in% c("bonding", "bridging", "linking")) %>%
    mutate(type = "social_business")
) %>%
  mutate(label = paste(round(estimate, 2),
                       gtools::stars.pval(p.value), sep = ""))

g1 <- out %>%
  mutate(term = term %>% recode_factor(
                 "social_capital" = "Social\nCapital",
                 "bonding" = "Bonding",
                 "bridging" = "Bridging",
                 "linking" = "Linking"),
                type = type %>% recode_factor(
                  "park" = "Parks",
                  "social_business" = "Social\nBusinesses",
                  "place_of_worship" = "Places\nof Worship",
                  "community_space" = "Community\nSpaces",
                   "total" = "Total")) %>%
  ggplot(mapping = aes(x = type, y = estimate, label = label, fill = estimate)) +
  geom_col(color = "#373737", size = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#373737", size = 1, alpha = 0.5) +
  facet_grid(~term) +
  coord_flip() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0,
                       guide = guide_colorbar(barheight = 1, barwidth = 20)) +
  shadowtext::geom_shadowtext(bg.r = 0.2, bg.color = "white", color = "#373737") +
  theme_classic(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, color = "#373737", size = 0.2),
        strip.background = element_blank(),
        legend.position = "bottom") +
  scale_x_discrete(expand = expansion(add = c(0.75, 0.75))) +
  scale_y_continuous(expand = expansion(add = c(0.1, 0.1))) +
  labs(y = "Standardized Effect on Logged Rates of Social Infrastructure",
       x = NULL, fill = "Effect Size")

ggsave(g1, filename = "viz/matrix_sc_overall.png", dpi = 500, width = 8, height = 4)
```


##

```{r}

out <- bind_rows(
  # Total
  dat %>%
    #filter(name != "dc") %>%
    group_by(name) %>%
    summarize(lm(formula = log(total*10 + 1) ~ bonding + bridging + linking + pop_density +
         black + hisplat + median_household_income + some_college + over_65 + unemployment) %>%
    broom::tidy() %>%
    filter(term %in% c("bonding", "bridging", "linking"))) %>%
    mutate(type = "total"),

  dat %>%
     #   filter(name != "dc") %>%
    group_by(name) %>%
    summarize(lm(formula = log(community_space*10 + 1) ~ bonding + bridging + linking + pop_density +
         black + hisplat + median_household_income + some_college + over_65 + unemployment) %>%
    broom::tidy() %>%
    filter(term %in% c("bonding", "bridging", "linking"))) %>%
    mutate(type = "community_space"),

  dat %>%  #  filter(name != "dc") %>%
    group_by(name) %>%
    summarize(lm(formula = log(place_of_worship*10 + 1) ~ bonding + bridging + linking + pop_density +
         black + hisplat + median_household_income + some_college + over_65 + unemployment) %>%
    broom::tidy() %>%
    filter(term %in% c("bonding", "bridging", "linking"))) %>%
    mutate(type = "place_of_worship"),

  dat %>%  #  filter(name != "dc") %>%
    group_by(name) %>%
    summarize(lm(formula = log(park*10 + 1) ~ bonding + bridging + linking + pop_density +
         black + hisplat + median_household_income + some_college + over_65 + unemployment) %>%
    broom::tidy() %>%
    filter(term %in% c("bonding", "bridging", "linking"))) %>%
    mutate(type = "park"),


  dat %>%  #  filter(name != "dc") %>%
    group_by(name) %>%
    summarize(lm(formula = log(social_business*10 + 1) ~ bonding + bridging + linking + pop_density +
         black + hisplat + median_household_income + some_college + over_65 + unemployment) %>%
    broom::tidy() %>%
    filter(term %in% c("bonding", "bridging", "linking"))) %>%
    mutate(type = "social_business")
) %>%
  mutate(label = paste(round(estimate, 2),
                       gtools::stars.pval(p.value), sep = "")) %>%
  left_join(by = "name", y = read_rds("viz/poptally.rds"))


# Visualize
g1 <- out %>%
  ggplot(mapping = aes(x = reorder(name_label, pop), y = 1,
                       label = label, fill = estimate)) +
  geom_tile() +
  shadowtext::geom_shadowtext(bg.color = "white", bg.r = 0.2, color = "#373737") +
  facet_nested(
    cols = vars(term, type),
    strip = strip_nested(bleed = FALSE),
        nest_line = element_line(linetype = "solid", color = "#373737"),

                      labeller = labeller(
               term = c(
                 "social_capital" = "Social\nCapital",
                 "bonding" = "Bonding",
                 "bridging" = "Bridging",
                 "linking" = "Linking"),
                type = c(
                   "total" = "Total",
                  "community_space" = "Community\nSpaces",
                  "park" = "Parks",
                  "social_business" = "Social\nBusinesses",
                  "place_of_worship" = "Places\nof Worship"))) +
  theme_classic(base_size = 14) +
  coord_flip() +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, size = 9),
        strip.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
        strip.background = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        plot.caption = element_text(hjust = 0)) +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limits = c(-1, 1), guide = guide_colorbar(barheight = 1, barwidth = 20)) +
  labs(x = NULL, y = NULL, fill = "Standardized Beta",
       caption = "Standardized Beta for effect of social capital from simple OLS regression models.\nModel Equation: Log(Social Infrastructure Rate) ~ Bonding + Bridging + Linking + Pop Density + % Black + %Hispanic/Latino + Income + \n% Some College + % Over Age 65 + Unemployment.")
library(ggh4x)

ggsave(g1, filename = "viz/matrix_sc.png", dpi = 500, width = 12.5, height = 6.5)

# Import 1 km tallies
```

