# 04_export.R


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

# 10. Extensions

## Export

### Ithaca MyMap

```{r}
read_csv("ithaca/results.csv") %>%
  select(type, term, name, rating, business_status, lng,lat) %>%
  write_csv("ithaca/sites_export.csv")


read_sf("ithaca/grid1km.geojson") %>%
  st_write("ithaca/grid1km_export.kml", delete_dsn = TRUE)
```

### Worcester MyMap

```{r}
sites <- read_sf("all/sites.geojson") %>%
  st_join(read_sf("worcester/bounds.geojson"), left = FALSE) %>%
  st_write("worcester/sites_export.kml", delete_dsn = TRUE)

read_sf("worcester/bounds.geojson") %>%
  st_write("worcester/bounds.kml")

read_sf("all/tally1km.geojson") %>%
  st_set_crs(wgs) %>%
  filter(name == "worcester") %>%
  select(cell, pop_density, community_space:park, black, white, asian, hisplat) %>%
  mutate(pop_density = pop_density) %>%
  mutate_at(vars(community_space:park), list(~. * 10)) %>%
  mutate_at(vars(community_space:park, black, white, asian, hisplat),
            list(~round(.,2))) %>%
  mutate(pop_density_cat = cut(
    pop_density, breaks = 4,
    labels = c("Lowest", "Lower Middle", "Upper Middle", "Highest")),
    nonwhite = 1 - white,
    pop_nonwhite_cat = ntile(nonwhite, 4) %>%
      recode_factor(
        "1" = "Lowest (9-28%)",
        "2" = "Lower Middle (29-37%)",
        "3" = "Upper Middle (38-46%)",
        "4" = "Upper (46-73%)")) %>%
  mutate_at(vars(nonwhite, black, white, hisplat, asian,),
            list(~round(. * 100, 0) )) %>%
  select(
    cell,
    `Pop Density` = pop_density,
    `Pop Density (Quartiles)` = pop_density_cat,
    `Community Spaces` = community_space,
    `Places of Worship` = place_of_worship,
    `Social Businesses` = social_business,
    `Parks` = park,
    `(%) Non-White` = nonwhite,
    `(%) Non-White (Quartiles)` = pop_nonwhite_cat,
    `(%) Black` = black,
    `(%) White` = white,
    `(%) Hispanic/Latino` = hisplat,
    `(%) Asian` = asian) %>%
  st_write("worcester/tally1km.kml", delete_dsn = TRUE)


tally <- read_sf("all/tallyblock.geojson") %>%
  filter(name == "worcester") %>%
  select(geoid, pop, pop_density, community_space:park, black, white, asian, hisplat) %>%
  st_set_crs(aea) %>%
  st_transform(crs = wgs) %>%
  st_write("worcester/tallyblock.kml", delete_dsn = TRUE)
```

## Neighborhoods

```{r}
# Extract the name-geoid crosswalk I created on Google Mymaps,
read_sf("worcester/tracts_crosswalk.kml") %>%
  as_tibble() %>%
  select(name = Name, geoid = GEOID10) %>%
  distinct() %>%
  # and save it as a simple data.frame
  saveRDS("worcester/tracts_crosswalk.rds")


# Get EPSG:4326 (WGS 84) projection
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Get equal area conic projection
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#myvars <- tidycensus::load_variables(year = 2019, dataset = "acs5")
#remove(myvars)

# Get census tract data from 2019 ACS 5-year averages
mytracts <- tidycensus::get_acs(
  survey = "acs5",
  year = 2019,
  geography = "tract",
  county = "Worcester",
  state = "MA",
  key = censuskey,
  variables = c(
    "B01001_001", # pop
    "B02001_002", # white
    "B02001_003", # black
    "B02001_005", # asian
    "B03001_003" # hisplat
  )) %>%
  mutate(variable = variable %>% dplyr::recode(
    "B01001_001" = "pop",
    "B02001_002" = "white",
    "B02001_003" = "black",
    "B02001_005" = "asian",
    "B03001_003" = "hisplat")) %>%
  select(geoid = GEOID, variable, estimate) %>%
  pivot_wider(id_cols = c(geoid),
              names_from = variable, values_from = estimate)



neigh <- tigris::tracts(
  state = "MA", county = "Worcester",
  cb = FALSE, year = 2019) %>%
  st_as_sf() %>%
  select(geoid = GEOID, area_land = ALAND, geometry) %>%
  # Zoom into just our tracts in Worcester, and join in names
  inner_join(by = "geoid", y = read_rds("worcester/tracts_crosswalk.rds")) %>%
  # Join in 2020 data, aggregated from blocks
  left_join(by = c("geoid"),
            y = mytracts) %>%
  group_by(name) %>%
  summarize_at(vars(pop:hisplat, area_land),
               list(~sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  # Calculate population density, transforming from sqm to sqkm
  mutate(pop_density = pop / (area_land / 1e6) ) %>%
  # Convert demographics to percentages
  mutate_at(vars(white, black, asian, hisplat),
            list(~round(./pop, 2) )) %>%
  # Fix up some names
  mutate(name = case_when(
    name == "Biotech Park / Booth Apartments Area" ~ "Biotech Park / Booth Apartments",
    TRUE ~ name)) %>%
  mutate(pop_density_cat = cut(
    pop_density, breaks = 4,
    labels = c("Lowest", "Lower Middle", "Upper Middle", "Highest")),
    nonwhite = 1 - white,
    pop_nonwhite_cat = ntile(nonwhite, 4) %>%
      recode_factor(
        "1" = "Lowest (11-24%)",
        "2" = "Lower Middle (25-31%)",
        "3" = "Upper Middle (32-38%)",
        "4" = "Upper (39-48%)")) %>%
  st_transform(crs = wgs)

# Get Sites
sites <- read_sf("all/sites.geojson") %>%
  st_transform(crs = aea)

# How many points are in the grid?
# What's the approximate population density in the grid?

# Get tallies by grid cell

site_tally <- neigh %>%
  st_transform(crs = aea) %>%
  # Join in block traits
  st_join(sites %>% select(type), left = TRUE) %>%
  as_tibble() %>%
  group_by(name) %>%
  summarize(
    total = sum(!is.na(type), na.rm = TRUE),
    community_space = sum(type == "Community Space", na.rm = TRUE),
    place_of_worship = sum(type == "Place of Worship", na.rm = TRUE),
    social_business = sum(type == "Social Business", na.rm = TRUE),
    park = sum(type == "Park", na.rm = TRUE))


neigh %>%
  left_join(by = "name", y = site_tally) %>%
  mutate_at(vars(total:park), list(~./pop_density)) %>%
  mutate_at(vars(total:park), list(~round(., 4))) %>%
  mutate_at(vars(total:park), list(~if_else(pop_density == 0, NA_real_, .))) %>%
  mutate(pop_density = round(pop_density, 0)) %>%

  mutate(total_cat = ntile(total, 4) %>%
           recode_factor(
             "1" = "Lowest",
             "2" = "Lower Middle",
             "3" = "Upper Middle",
             "4" = "Upper")) %>%
  select(
    neighborhood = name,
    `Population` = pop,
    `Pop Density` = pop_density,
    `Pop Density (Quartiles)` = pop_density_cat,
    `Social Infrastructure Rate` = total,
    `Social Infrastructure (Quartiles)` = total_cat,
    `Community Spaces Rate` = community_space,
    `Places of Worship Rate` = place_of_worship,
    `Social Businesses Rate` = social_business,
    `Parks Rate` = park,
    `(%) Non-White` = nonwhite,
    `(%) Non-White (Quartiles)` = pop_nonwhite_cat,
    `(%) Black` = black,
    `(%) White` = white,
    `(%) Hispanic/Latino` = hisplat,
    `(%) Asian` = asian) %>%
  st_write("worcester/neighborhoods.kml", delete_dsn = TRUE)


```

## Worcester Map

Let's put together a simple, beautiful map of Worcester.

```{r}
library(tidyverse)
library(sf)
library(ggpubr)


# Get EPSG:4326 (WGS 84) projection
#https://spatialreference.org/ref/epsg/wgs-84/
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Get Sites
sites <- read_csv("worcester/results.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(crs = wgs)
# Get bounds
bounds <- read_sf("worcester/bounds.geojson") %>%
  mutate(area = as.numeric(st_area(.$geometry)) / 1000000,
         area = round(area, 0))

# Get Grid
grid <- read_sf("worcester/grid.geojson")

g1 <- ggplot() +
  geom_sf(data = bounds, color = "black", fill = "#373737", alpha = 0.25) +
  geom_sf(data = grid, color = "white", size = 1, fill = NA) +
  geom_sf(data = grid, color = "grey", size = 0.2, fill = NA) +
  #geom_sf_text(data = grid,
  #             mapping = aes(label = cell), color = "black", size = 1) +
  geom_sf(data = sites, mapping = aes(fill = type),
          shape = 21, color = "white", stroke = 0.5, size = 3) +
  scale_fill_manual(
    breaks = c("Community Space", "Place of Worship",
               "Social Business", "Park"),
    values = c("#0D0887", "#6A00A8", "#E16462" ,"#FCA636"),
    guide = guide_legend(override.aes = list(size = 5))) +
  theme_void(base_size = 14) +
  labs(x = NULL, y = NULL, fill = NULL,
       subtitle = paste(
         "Worcester, MA (",
         nrow(sites), " sites, ", bounds$area, " sq.km.)",
         sep = "")) +
  theme(legend.position = c(0.1, 0.8),
        legend.text = element_text(color = "white"),
        plot.subtitle = element_text(color = "white"))
ggsave(g1, filename = "viz/worcester_map.png", dpi = 500, width = 5, height= 4.5)
```






## 3.4 Interpolate En Masse

```{r}
library(tidyverse)
library(sf)
library(gstat)
library(automap)
library(viridis)

# Get equal area conic projection
aea <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# First, let's calculate population density for each census block.
b <- read_sf("raw_data/blocks.geojson") %>%
  filter(area_land > 0) %>%
  left_join(by = c("geoid"),
            y = read_csv("raw_data/census2020/census_blocks.csv",
                         col_types = list(geoid = col_character()))) %>%
  # Calculate population density
  mutate(pop_density = pop / (area_land / (1000^2) )  ) %>%
  st_transform(crs = aea)

# Get long list of points
p <- b %>%
  pivot_longer(cols = c(pop_white:pop_hisplat, units_occupied, pop_density),
               names_to = "variable", values_to = "value") %>%
  # Filter to just populated spots,
  filter(pop > 0) %>%
  # and then get the centroid
  mutate(geometry = st_centroid(geometry)) %>%
  select(geoid, variable, value, geometry)


# Problematically, some places have a population density of 0, which doesn't really make sense in the middle of Boston - sure, maybe people don't live exactly in block X, but you better believe it's a densely settled area up in most of the North End. Let's spatially interpolate a guestimate of the density in each area.

# Get the outside bounds
city <- b %>%
  select(geometry) %>%
  summarize(geometry = st_union(geometry))


# Finally, make a grid!
b$area_land %>% summary()
# How big is the median census block? Well, looks like the median is 11,000 meters in area,
# with the IQR varying between 6000 and 21410.
# So, we're going to need a pretty nice and wide max-distance for this to work.

# Get grid
gbox <- city %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_make_grid(
    cellsize = c(500, 500), # 500m pixel size
    what = "polygons") %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  # Get just grid cells within the bounds
  st_join(city, left = FALSE)

# Get grid points
g <- gbox %>%
  mutate(geometry = st_centroid(geometry)) %>%
  # Bind in the coordinates
  bind_cols(.,
            st_coordinates(.) %>%
              as_tibble() %>%
              rename(x = 1, y = 2))

```


Let's get the interpolated values from w3_1000 for each of the census block variables.

```{r}
p %>%
  as_tibble() %>%
  split(.$variable) %>%
  map_dfr(~gstat::idw(
    formula = value ~ 1,
    st_as_sf(., crs = aea) %>% select(value) %>% filter(!is.na(value)) %>% as("Spatial"),
    g %>% as("Spatial"),
    # Inverse Distance Weighting Power
    idp = 2, maxdist = 1000) %>%
      st_as_sf(crs = aea) %>%
      # Join interpolated points back into the grid
      st_join(x = gbox, y = .) %>%
      # Join grid into overlapping census blocks
      st_join(b %>% select(geoid), left = TRUE) %>%
      as_tibble() %>%
      # Get data.frame of interpolated values
      group_by(geoid) %>%
      summarize(value = mean(var1.pred, na.rm = TRUE)) %>%
      ungroup(), .id = "variable") %>%
  # Pivot back wider
  pivot_wider(id_cols = c(geoid), names_from = variable, values_from = value) %>%
  magrittr::set_colnames(value = c("geoid", names(.)[-1] %>% paste(., "_int", sep = ""))) %>%
  write_csv("raw_data/census2020/census_blocks_int.csv")

rm(list = ls())
```


## 3.5 Average to Grid

```{r}
# First, let's calculate population density for each census block.
b <- read_sf("raw_data/blocks.geojson") %>%
  filter(area_land > 0) %>%
  left_join(by = c("geoid"),
            y = read_csv("raw_data/census2020/census_blocks_int.csv",
                         col_types = list(geoid = col_character())))

# Average into cell estimates
read_sf("raw_data/grid.geojson") %>%
  st_join(b) %>%
  as_tibble() %>%
  group_by(cell) %>%
  summarize_at(vars(contains("int")), list(~mean(., na.rm = TRUE))) %>%
  ungroup() %>%
  right_join(by = "cell", y = read_sf("raw_data/grid.geojson")) %>%
  st_write("raw_data/grid_data.geojson")
```

```{r}



```


