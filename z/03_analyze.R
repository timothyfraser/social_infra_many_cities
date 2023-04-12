# 5. Visualize

## Maps

Let's put together a simple, beautiful map of these five cities.

```{r}
library(tidyverse)
library(sf)
library(ggpubr)


# Get EPSG:4326 (WGS 84) projection
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Get Sites
sites <- read_csv("nyc/results.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = wgs)
# Get bounds
bounds <- read_sf("nyc/bounds.geojson") %>%
  mutate(area = as.numeric(st_area(.$geometry)) / 1000000,
         area = round(area, 0))

# Get Grid
grid <- read_sf("nyc/grid.geojson")

g1 <- ggplot() +
  geom_sf(data = bounds, color = "black", fill = "#373737") +
  geom_sf(data = grid, color = "grey", fill = NA) +
  #geom_sf_text(data = grid,
  #             mapping = aes(label = cell), color = "black", size = 1) +
  geom_sf(data = sites, mapping = aes(fill = type),
          shape = 21, color = "white", stroke = 0.1, size = 1) +
  scale_fill_manual(
    breaks = c("Community Space", "Place of Worship",
               "Social Business", "Park"),
    values = c("#0D0887", "#6A00A8", "#E16462" ,"#FCA636"),
    guide = guide_legend(override.aes = list(size = 5))) +
  theme_void(base_size = 14) +
  labs(x = NULL, y = NULL, fill = "Type",
       subtitle = paste(
         "New York City\n",
                  "8.6 milion\n",
         nrow(sites), " sites\n",
         " (", bounds$area, " sq.km.)",
         sep = ""))



# Get Sites
sites <- read_csv("la/results.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = wgs)
# Get bounds
bounds <- read_sf("la/bounds.geojson") %>%
  mutate(area = as.numeric(st_area(.$geometry)) / 1000000,
         area = round(area, 0))

# Get Grid
grid <- read_sf("la/grid.geojson")

g2 <- ggplot() +
  geom_sf(data = bounds, color = "black", fill = "#373737") +
  geom_sf(data = grid, color = "grey", fill = NA) +
  #geom_sf_text(data = grid,
  #             mapping = aes(label = cell), color = "black", size = 1) +
  geom_sf(data = sites, mapping = aes(fill = type),
          shape = 21, color = "white", stroke = 0.1, size = 1) +
  scale_fill_manual(
    breaks = c("Community Space", "Place of Worship",
               "Social Business", "Park"),
    values = c("#0D0887", "#6A00A8", "#E16462" ,"#FCA636"),
    guide = guide_legend(override.aes = list(size = 5))) +
  theme_void(base_size = 14) +
  labs(x = NULL, y = NULL, fill = "Type",
       subtitle = paste(
         "Los Angeles\n",
                  "4.1 milion\n",
         nrow(sites), " sites\n",
         " (", bounds$area, " sq.km.)",
         sep = ""))



# Get Sites
sites <- read_csv("chicago/results.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = wgs)
# Get bounds
bounds <- read_sf("chicago/bounds.geojson") %>%
  mutate(area = as.numeric(st_area(.$geometry)) / 1000000,
         area = round(area, 0))

# Get Grid
grid <- read_sf("chicago/grid.geojson")

g3 <- ggplot() +
  geom_sf(data = bounds, color = "black", fill = "#373737") +
  geom_sf(data = grid, color = "grey", fill = NA) +
  #geom_sf_text(data = grid,
  #             mapping = aes(label = cell), color = "black", size = 1) +
  geom_sf(data = sites, mapping = aes(fill = type),
          shape = 21, color = "white", stroke = 0.1, size = 1) +
  scale_fill_manual(
    breaks = c("Community Space", "Place of Worship",
               "Social Business", "Park"),
    values = c("#0D0887", "#6A00A8", "#E16462" ,"#FCA636"),
    guide = guide_legend(override.aes = list(size = 5))) +
  theme_void(base_size = 14) +
  labs(x = NULL, y = NULL, fill = "Type",
       subtitle = paste(
         "Chicago\n",
                  "2.7 milion\n",
         nrow(sites), " sites\n",
         " (", bounds$area, " sq.km.)",
         sep = ""))



# Get Sites
sites <- read_csv("houston/results.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = wgs)
# Get bounds
bounds <- read_sf("houston/bounds.geojson") %>%
  mutate(area = as.numeric(st_area(.$geometry)) / 1000000,
         area = round(area, 0))

# Get Grid
grid <- read_sf("houston/grid.geojson")

g4 <- ggplot() +
  geom_sf(data = bounds, color = "black", fill = "#373737") +
  geom_sf(data = grid, color = "grey", fill = NA) +
  #geom_sf_text(data = grid,
  #             mapping = aes(label = cell), color = "black", size = 1) +
  geom_sf(data = sites, mapping = aes(fill = type),
          shape = 21, color = "white", stroke = 0.1, size = 1) +
  scale_fill_manual(
    breaks = c("Community Space", "Place of Worship",
               "Social Business", "Park"),
    values = c("#0D0887", "#6A00A8", "#E16462" ,"#FCA636"),
    guide = guide_legend(override.aes = list(size = 5))) +
  theme_void(base_size = 14) +
  labs(x = NULL, y = NULL, fill = "Type",
       subtitle = paste(
         "Houston\n",
         "2.4 milion\n",
         nrow(sites), " sites\n",
         " (", bounds$area, " sq.km.)",
         sep = ""))



# Get Sites
sites <- read_csv("phoenix/results.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = wgs)
# Get bounds
bounds <- read_sf("phoenix/bounds.geojson") %>%
  mutate(area = as.numeric(st_area(.$geometry)) / 1000000,
         area = round(area, 0))

# Get Grid
grid <- read_sf("phoenix/grid.geojson")

g5 <- ggplot() +
  geom_sf(data = bounds, color = "black", fill = "#373737") +
  geom_sf(data = grid, color = "grey", fill = NA) +
  #geom_sf_text(data = grid,
  #             mapping = aes(label = cell), color = "black", size = 1) +
  geom_sf(data = sites, mapping = aes(fill = type),
          shape = 21, color = "white", stroke = 0.1, size = 1) +
  scale_fill_manual(
    breaks = c("Community Space", "Place of Worship",
               "Social Business", "Park"),
    values = c("#0D0887", "#6A00A8", "#E16462" ,"#FCA636"),
    guide = guide_legend(override.aes = list(size = 5))) +
  theme_void(base_size = 14) +
  labs(x = NULL, y = NULL, fill = "Type",
       subtitle = paste(
         "Phoenix\n",
                  "1.6 milion\n",
         nrow(sites), " sites\n",
         " (", bounds$area, " sq.km.)",
         sep = ""))


dir.create("viz")

a1 <- ggpubr::ggarrange(g1,g2, ncol = 2, common.legend = TRUE, legend = "none")
a2 <- ggpubr::ggarrange(g3,g4, g5, ncol = 3, common.legend = TRUE, legend = "bottom")

combo <- ggpubr::ggarrange(a1, a2, ncol = 1)

ggsave(combo, filename = "viz/cities_5.png", dpi = 500, width = 8, height = 9)
```

## Bars

```{r}
library(tidyverse)
library(sf)

# First, get population for each place
read_rds("census2020/bg_data.rds") %>%
  mutate(tract = str_sub(geoid, 1, 11)) %>%
  left_join(by = c("tract" = "geoid"), y = read_csv("census2020/sample_tracts.csv")) %>%
  group_by(name) %>%
  summarize(pop = sum(pop, na.rm = TRUE)) %>%
  mutate(name_label = name %>% str_split(pattern = "_") %>%
           map(~paste(str_sub(., 1,1) %>% toupper(),
                      str_sub(., 2,-1), sep = "")) %>%
           map(~paste(., collapse = " ")) %>% unlist(),
         name_label = if_else(name_label %in% c("Nyc", "La", "Dc"), toupper(name_label), name_label),
         name_label = if_else(name_label == "Oklahoma", "Oklahoma City", name_label)) %>%
  mutate(pop_label = round(pop / 1e6, 2)) %>%
  saveRDS("viz/poptally.rds")

tally <- read_sf("all/tally1kmbg.geojson") %>%
  as_tibble() %>%
  select(name, pop_density, community_space:park) %>%
  pivot_longer(cols = c(community_space:park), names_to = "type", values_to = "rate") %>%
  group_by(name,
           type = type %>% recode_factor(
             "community_space" = "**Community Spaces**",
             "place_of_worship" = "**Places of Worship**",
             "social_business" = "**Social Businesses**",
             "park" = "**Parks**")) %>%
  summarize(mean = mean(rate*10, na.rm = TRUE)) %>%
  mutate(label = round(mean, 1)) %>%
  # Join in population
  left_join(by = "name",
            y = read_rds("viz/poptally.rds"))

tally_stat <- tally %>%
  arrange(desc(pop), type) %>%
  group_by(type) %>%
  mutate(order = 1:n()) %>%
  group_by(type) %>%
  mutate(add = mean / 75,
         fill = if_else(str_detect(name_label, "Boston"), "Case", "Other")) %>%
  ungroup() %>%
  filter(!name %in% c("Worcester", "Ithaca") )


g1 <- tally_stat %>%
  ggplot(mapping = aes(x = reorder(name_label, -order), y = mean + add, label = label)) +
  geom_col(mapping = aes(y = mean + add*20), color = NA, fill = NA) +
  geom_col(mapping = aes(y = mean, fill = fill)) +
  ggtext::geom_richtext(mapping = aes(y = mean + add),
                        hjust = 0,  color = "#373737",
                        fill = NA, label.color = NA) +
  facet_grid(cols = vars(type), scales = "free_x") +
  theme_bw(base_size =14) +
  theme(strip.background = element_blank(),
        strip.text.x = ggtext::element_markdown(hjust = 0.5),
        axis.text.y = ggtext::element_markdown(hjust = 0, size = 12),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = ggtext::element_markdown(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing.y = unit(0, "cm"),
        panel.spacing.x = unit(1, "cm")) +
  coord_flip() +
  scale_fill_manual(values = c("#00338e", "darkgrey"), guide = 'none') +
  scale_y_continuous(expand = expansion(add = c(0, 2)), trans = "log",
                     breaks = c(1,  10, 100, 1000)) +
  labs(x = NULL, y = "Mean Sites per 10,000 residents, per 1 km<sup>2</sup>",
       subtitle = "Average Rates of Social Infrastructure by City")

ggsave(g1, filename = "viz/cities_avg_all.png", dpi = 500, width = 10, height = 5.5)
```

# 11. Export

```{r, message=FALSE, warning = FALSE}
dir.create("export")
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Get equal area conic projection
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Export tallies by 1 square kilometer, with averaged block group traits
read_sf("all/tally1kmbg.geojson") %>%
  st_set_crs(value = aea) %>%
  st_transform(crs = wgs) %>%
  st_write("export/tally1kmbg.shp",  layer="tally1kmbg",
           driver="ESRI Shapefile", delete_dsn = TRUE)

# Export city bounds
read_sf("all/bounds.geojson") %>%
  st_transform(crs = wgs) %>%
  st_write("export/bounds.shp",  layer="bounds",
           driver="ESRI Shapefile", delete_dsn = TRUE)

# Export points
read_sf("all/sites.geojson") %>%
  st_transform(crs = wgs) %>%
  st_write("export/sites.shp",  layer="sites",
           driver="ESRI Shapefile", delete_dsn = TRUE)

# Export points
read_sf("all/bg.geojson") %>%
  st_transform(crs = wgs) %>%
  st_write("export/bg.shp",  layer="block_groups",
           driver="ESRI Shapefile", delete_dsn = TRUE)


read_sf("all/tracts.geojson") %>%
  st_join(read_sf("all/bounds.geojson") %>% select(geometry), left = FALSE) %>%
  # Join in social capital indices for these cities
  left_join(by = "geoid", y = read_rds("census2020/index_census_tract.rds") %>%
  filter(year == 2018) %>%
  select(geoid, social_capital:linking)) %>%
  # Set to WGS projection
  st_transform(crs = wgs) %>%
  st_write("export/sci_tracts.shp",  layer="sci_tracts",
           driver="ESRI Shapefile", delete_dsn = TRUE)
```


# 6. Social Capital Models



```{r, message = FALSE ,warning = FALSE, fig.height = 4}
# Get EPSG:4326 (WGS 84) projection
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Get equal area conic projection
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


# Import tracts for our cities
tracts <- read_sf("all/tracts.geojson") %>%
  # Join in social capital indices for these cities
  left_join(by = "geoid", y = read_rds("census2020/sci.rds")) %>%
  filter(year == 2020) %>%
  select(geoid, social_capital:linking) %>%
  # Set to WGS projection
  st_transform(crs = wgs)

# Join in the tract values to the cell grids
tally_sc <- read_sf("all/tally1km.geojson") %>%
  st_set_crs(wgs) %>%
  select(cell, name) %>%
  # Join tracts
  st_join(tracts %>% select(social_capital:linking)) %>%
  # Convert to tibble
  as_tibble() %>%
  # For each cell-name combo,
  group_by(cell, name) %>%
  # Take the average
  summarize(across(social_capital:linking, ~mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  # And set NAN to NA
  mutate(across(social_capital:linking, ~if_else(is.nan(.), NA_real_, .)))


# Import 1 km tallies
read_sf("all/tally1kmbg.geojson") %>%
  st_set_crs(aea) %>%
  st_transform(wgs) %>%
  select(cell, name, pop_density, community_space:park,
         # Covariates
         black, hisplat, median_household_income, some_college, over_65, unemployment) %>%
  left_join(by = c("cell", "name"), y = tally_sc) %>%
  as_tibble() %>%
  # Zoom into just populated sites
  filter(pop_density > 0) %>%
  #pivot_longer(cols = c(community_space:park), names_to = "site_type", values_to = "sites") %>%
  group_by(name) %>%
  mutate_at(vars(pop_density, social_capital, bonding, bridging, linking,
                 black, hisplat, median_household_income,
                 some_college, over_65, unemployment), list(~scale(.))) %>%
  ungroup() %>%
  mutate(total = community_space + park + social_business + place_of_worship) %>%
  write_rds("all/dataset.rds")


remove(tally_sc, tracts)
```

## Overall Models

```{r}
dat <- read_rds("all/dataset.rds") %>%
  filter(!name %in% c("worcester", "ithaca"))

m1 <- dat %>%
  lm(formula = total ~ bonding + bridging + linking +  pop_density +
       black + hisplat + median_household_income +
       some_college + over_65 + unemployment + name)

m2 <- dat %>%
  lm(formula = log(total*10 + 1) ~ bonding + bridging + linking + pop_density +
       black + hisplat + median_household_income +
       some_college + over_65 + unemployment + name)

# Much better to use log of outcome
lmtest::lrtest(m1,m2)

remove(m1,m2)


m0 <- dat %>%
  lm(formula = log(total+ 1) ~ social_capital + pop_density + name)

m1 <- dat %>%
  lm(formula = log(total*10 + 1) ~ social_capital + pop_density +
       black + hisplat + median_household_income + some_college + over_65 + unemployment + name)

m2 <- dat %>%
  lm(formula = log(total*10 + 1) ~ bonding + bridging + linking + pop_density + name)

m3 <- dat %>%
  lm(formula = log(total*10 + 1) ~ bonding + bridging + linking + pop_density +
       black + hisplat + median_household_income + some_college + over_65 + unemployment + name)

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
    "pop_density" = "Population Density",
    "black" = "% Black",
    "hisplat" = "% Hispanic/Latino",
    "some_college" = "% Some college or more",
    "over_65" = "% Over Age 65",
    "median_household_income" = "Median Household Income",
    "unemployment" = "Unemployment Rate",
    "(Intercept)" = "Constant"),
  custom.note = "<b>Statistical Significance</b>: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.10.
  <br>
  <b>Estimates</b>: show projected increase in logged rates of social infrastructure as predictor increases by one standard deviation. Effect sizes can be compared.
  <b>Fixed Effects</b>: Fixed effects for 25 cities. <b>Collinearity</b>: All Variance Inflation Factor scores below 2.5.",
  file = "viz/table1.html")


remove(m0,m1,m2,m3)

```

## Visualizing Overall Models

```{r}

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

