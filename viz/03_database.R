#' @name generate.R
#'
#' @description A script to generate supplemental or formatted information
#' based on our previous API calls.
#'


# 0. Packages  ################################################

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)
library(sf)
library(RSQLite)
library(DBI)

# 1. Polygons############################################

data("meta") # get city name metadata
data("wgs") # Get EPSG:4326 (WGS 84) projection (https://spatialreference.org/ref/epsg/wgs-84/)
data("aea") # Get equal area conic projection


# Let's initialize the geospatial database, using a blank table called 'starter'
tibble(geometry = "POINT(42.4534 -76.4735)") %>%
  st_as_sf(wkt = "geometry", crs = 4326) %>%
  st_write(obj = ., dsn = Sys.getenv("DATABASE"), layer = "starter", delete_dsn = TRUE, delete_layer = TRUE)


# Write a combine function
combine = function(name, type, ext = ".geojson", area = FALSE){
  print(name);
  # Get the shapes
  shapes = paste0("search/", name, "/", type, ext) %>%
    read_sf() %>% mutate(name = name)
  # IF area == TRUE,
  if(area == TRUE){
    data("aea")
    # Calculate the area, in square kilometers
    shapes = shapes %>%
      st_transform(crs = aea) %>%
      mutate(area = as.numeric(st_area(geometry)) / 1000000) %>%
      st_transform(crs = 4326)
  }
  return(shapes)
}


# Now connect to it using SQLite
# Now, let's add a bunch of data
data("meta")
data("connect") # Get connect() function
data("wkb_as_sf") # Get function to convert from wkb to sf format
# Establish connection
geo = connect()

## 1.1 Bounds ####################
# Get all bounds and save to table (overwrite the table)
meta$cities %>%
  map(~combine(name = ., type = "bounds", ext = ".geojson", area = TRUE)) %>%
  dplyr::bind_rows() %>%
  st_write(obj = ., dsn = geo, layer = "bounds", delete_layer = TRUE, format = "WKB")

gc(); # Clear cache

# Test view it
# test = geo %>% tbl("bounds") %>% head(1) %>% collect()
# wkb_as_sf(test)$geometry %>% plot()
# remove(test)

## 1.2 Block Group ##################
# Get all block group boundaries and save to file  (overwrite table)
meta$cities %>%
  map(~combine(name = ., type = "bg", ext = ".geojson", area = TRUE)) %>%
  dplyr::bind_rows() %>%
  st_write(obj = ., dsn = geo, layer = "bg", delete_layer = TRUE, format = "WKB")

gc(); # Clear cache

## 1.3 Grid 1km ######################
# Get all grids and save to file (overwrite table)
meta$cities %>%
  # Skip area calculation - they're all 1 square kilometer!
  map(~combine(name = ., type = "grid1km", ext = ".geojson", area = FALSE)) %>%
  bind_rows() %>%
  st_write(obj = ., dsn = geo, layer = "grid1km", delete_layer = TRUE, format = "WKB")

gc(); # Clear cache

## 1.4 Tracts #######################
# Get all tracts and save to file (overwrite table)
meta$cities %>%
  map(~combine(name = ., type = "tracts", ext = ".geojson", area = TRUE)) %>%
  bind_rows() %>%
  st_write(obj = ., dsn = geo, layer = "tracts", delete_layer = TRUE, format = "WKB")

gc(); # Clear cache

## 1.5 Blocks #####################
# Get all blocks and save to file (overwrite table)
data("meta")
# For each city...
for(i in meta$cities){
  combine(name = i, type = "blocks", ext = ".geojson", area = FALSE) %>%
    st_write(obj = ., dsn = geo, layer = "blocks", delete_layer = FALSE, format = "WKB", append = TRUE)
  gc()
}


# 2. Add Census Traits to Database ###############################


## 2.1 Block Data ################################
gc()
data("connect")
geo = connect()

geo %>%
  tbl("blocks") %>%
  select(geoid, name, area_land) %>%
  collect() %>%
  # Now join in the block group data
  left_join(
    by = "geoid",
    y = read_rds("census/block_data.rds")) %>%
  # Calculate population density, while you're at it.
  mutate(pop_density = pop / (area_land / (1000^2) ) ) %>%
  # And write it to file.
  dbWriteTable(
    conn = geo, name = "block_data",
    value = .,
    delete_layer = TRUE, overwrite = TRUE, append = FALSE)

# geo %>% tbl("blocks")
# read_rds("census/block_data.rds") %>% filter(geoid == "131210078061008")

# Great! Now our block data is queryable by name!


## 2.2 Block Group Data ################################

data("connect")
geo = connect()

geo %>%
  tbl("bg") %>%
  select(geoid, name, area_land) %>%
  collect() %>%
  # Now join in the block group data
  left_join(
    by = "geoid",
    y = read_rds("census/bg_data.rds") %>%
      select(-median_monthly_housing_costs, -gini) %>%
      mutate(median_household_income = if_else(
        condition = median_household_income < 0,
        true= NA_integer_, false = median_household_income)
      )
  ) %>%
  # Calculate population density, while you're at it.
  mutate(pop_density = pop / (area_land / (1000^2) ) ) %>%
  # And write it to file.
  dbWriteTable(
    conn = geo, name = "bg_data",
    value = .,
    delete_layer = TRUE, overwrite=  TRUE, append = FALSE)

#geo %>% tbl("bg_data")

## 2.3 Tract Data ##########################################

data("connect")
geo = connect()

geo %>%
  tbl("tracts") %>%
  select(geoid, name, area_land) %>%
  collect() %>%
  mutate(area = area_land / (1000^2)) %>%
  # join in census variables by geoid
  inner_join(by = "geoid", y = read_rds("census/tract_data.rds")) %>%
  # Calculate population density
  mutate(pop_density = pop / area) %>%
  # And write it to file.
  dbWriteTable(
    conn = geo, name = "tract_data",
    value = .,
    delete_layer = TRUE,
    overwrite = TRUE, append = FALSE)

# geo %>%
#   tbl("tracts") %>%
#   select(geoid, name, area_land) %>%
#   collect() %>%
#   # Now join in the block group data
#   left_join(
#     by = "geoid",
#     y = read_rds("census/tract_data.rds")) %>%
#   # Calculate population density, while you're at it.
#   mutate(pop_density = pop / (area_land / (1000^2) ) ) %>%
#   # And write it to file.
#   dbWriteTable(
#     conn = geo, name = "tract_data",
#     value = .,
#     delete_layer = TRUE,
#     overwrite = TRUE, append = FALSE)

# geo %>% tbl("tract_data")

dbDisconnect(geo)

rm(list = ls()); gc()



# 3. Sites ###################################################

data("meta")
data("connect")
geo = connect()

# Write a function to import a specific city's data and format it
filter_sites = function(.name){

  data("aea")

  # Get shapes for that city
  shapes = paste0("search/", .name, "/bounds.geojson") %>% read_sf() %>% st_transform(crs = aea) %>%
    mutate(area = as.numeric(st_area(geometry)) / 1000000)

  # Get points for that city
  points = paste0("search/", .name, "/results.csv") %>% read_csv() %>% select(place_id:term) %>%
    mutate(place_name = name, name = .name) %>%
    # Get distinct searches, by grabbing just the first result of any multiples that appear.
    group_by(place_id) %>%
    summarize(across(.cols = everything(), .fns = ~.x[1])) %>%
    ungroup() %>%
    # Make into an sf object
    st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
    st_transform(crs = aea)

  # Overwrite, by narrowing into just points in our cities
  points = points %>%
    # Filter to just sites within city
    st_join(y = shapes %>% select(geometry), left = FALSE) %>%
    # Turn back to simple formatting
    st_transform(crs = 4326)
  return(points)
}

# Get all sites, filted to within the bounds!
meta$cities %>%
  map(~filter_sites(.name = .)) %>%
  bind_rows() %>%
  st_write(obj = ., dsn = geo, layer = "sites", delete_layer = TRUE, format = "WKB")


# 3. Quantities of Interest #######################################

data("wgs")
data("aea")
data("meta")
data("connect")
data("wkb_as_sf")
geo = connect()

geo %>% dbListTables()

# For city i....
.name = meta$cities[1]

# # Get bounds....
# bounds = geo %>%
#   tbl("bounds") %>%
#   filter(name == !!.name) %>%
#   collect() %>%
#   wkb_as_sf() %>%
#   st_as_sf(crs = 4326) %>%
#   st_transform(crs = aea)

#' @name tally_it
#' @description
#' Super function for tallying up the rate of sites with covariates at a given geographic level.
#' Supports grid, block, and soon, tract.
tally_it = function(.name, .type = "grid"){
  # test values
  # .name = "atlanta"
  .type = "block"
  # .type = "tract"

  ## get polygons ###################################
  # If grid, get grid cells
  if(.type == "grid"){
    # Get grid cells...
    grid = geo %>%
      tbl("grid1km") %>%
      filter(name == !!.name) %>%
      collect()  %>%
      wkb_as_sf() %>%
      st_as_sf(crs = 4326) %>%
      st_transform(crs = aea)
  }
  if(.type == "block" | .type == "grid"){
    # Get blocks...
    blocks = geo %>%
      tbl("blocks") %>%
      filter(name == !!.name, area_land > 0) %>%
      select(geoid, name, geometry) %>%
      left_join(by = c("geoid", "name"),
                y = tbl(geo, "block_data") %>% filter(name == !!.name)) %>%
      filter(pop_density > 0) %>%
      collect() %>%
      wkb_as_sf() %>%
      st_as_sf(crs = 4326) %>%
      st_transform(crs = aea)
  }
  if(.type == "grid"){
    # What's the approximate population density in the grid?
    grid_block_traits = grid %>%
      # Join in block traits
      st_join(
        y = blocks %>%
          select(pop_density, white:hisplat, units_occupied),
        left = TRUE) %>%
      as_tibble() %>%
      group_by(name, cell) %>%
      summarize_at(vars(pop_density, white:hisplat, units_occupied),
                   list(~median(., na.rm = TRUE))) %>%
      rename(pop_density_block = pop_density,
             white_block = white,
             black_block = black,
             hisplat_block = hisplat,
             asian_block = asian,
             natam_block = natam,
             units_occupied_block = units_occupied) %>%
      ungroup()
  }
  if(.type == "block"){

    block_block_traits = blocks %>%
      as_tibble() %>%
      select(geoid, name, pop, pop_density, white:hisplat, units_occupied) %>%
      rename(pop_block = pop,
             pop_density_block = pop_density,
             white_block = white,
             black_block = black,
             hisplat_block = hisplat,
             asian_block = asian,
             natam_block = natam,
             units_occupied_block = units_occupied)

  }
  if(.type == "block" | .type == "grid"){
    # Get block group traits!
    bg = geo %>%
      tbl("bg") %>%
      filter(name == !!.name, area_land > 0) %>%
      select(geoid, name, geometry) %>%
      left_join(by = c("geoid", "name"), y = tbl(geo, "bg_data") %>% filter(name == !!.name)) %>%
      filter(pop_density > 0) %>%
      collect() %>%
      wkb_as_sf() %>%
      st_as_sf(crs = 4326) %>%
      st_transform(crs = aea)
  }
  if(.type == "grid"){
    grid_bg_traits = grid %>%
      select(cell, name) %>%
      st_join(y = bg %>% select(women:unemployment, pop_density), left = TRUE) %>%
      as_tibble() %>% select(-geometry) %>%
      group_by(cell, name) %>%
      summarize_at(vars(women:unemployment, pop_density), list(~median(., na.rm = TRUE))) %>%
      rename(pop_density_bg = pop_density,
             women_bg = women,
             white_bg = white,
             black_bg = black,
             asian_bg = asian,
             hisplat_bg = hisplat,
             natam_bg = natam,
             pacific_bg = pacific,
             some_college_bg = some_college,
             income_0_60K_bg = income_0_60K,
             median_household_income_bg = median_household_income,
             unemployment_bg = unemployment) %>%
      ungroup()
  }

  if(.type == "block"){
    block_bg_traits = blocks %>%
      mutate(bg = str_sub(geoid, 1, 12)) %>%
      as_tibble() %>%
      select(geoid, name, bg) %>%
      left_join(by = c("bg" = "geoid", "name"),
                y = bg %>% as_tibble() %>% select(name, geoid, women:unemployment, pop_density)) %>%
      rename(pop_density_bg = pop_density,
             women_bg = women,
             white_bg = white,
             black_bg = black,
             asian_bg = asian,
             hisplat_bg = hisplat,
             natam_bg = natam,
             pacific_bg = pacific,
             some_college_bg = some_college,
             income_0_60K_bg = income_0_60K,
             median_household_income_bg = median_household_income,
             unemployment_bg = unemployment) %>%
      ungroup()
  }

  ### tracts ################################

  if(.type == "tract"){
    # Get tracts for my city
    tracts = geo %>%
      tbl("tracts") %>%
      filter(name == !!.name) %>%
      collect()  %>%
      wkb_as_sf() %>%
      st_as_sf(crs = 4326) %>%
      st_transform(crs = aea)

    tract_data = geo %>%
      tbl("tract_data") %>%
      filter(name == !!.name) %>%
      collect()

    # # tract data for my city
    # tract_data = read_rds("census/tract_data.rds") %>%
    #   # join in area and name
    #   inner_join(by = "geoid", y = tracts %>% as_tibble() %>% select(geoid, name, area)) %>%
    #   # Calculate population density
    #   mutate(pop_density = pop / area)
  }



  ## sites ####################################

  # Get Sites...
  sites = geo %>%
    tbl("sites") %>%
    filter(name == !!.name) %>%
    collect() %>%
    wkb_as_sf() %>%
    st_as_sf(crs = 4326) %>%
    st_transform(crs = aea)


  ## tally per polygon #######################
  ### tally per grid #########################
  if(.type == "grid"){
    # How many points are in the grid?
    grid_site_traits = grid %>%
      st_join(y = sites %>% select(type), left = TRUE) %>%
      as_tibble() %>%
      group_by(name, cell) %>%
      summarize(community_space = sum(type == "Community Space", na.rm = TRUE),
                place_of_worship = sum(type == "Place of Worship", na.rm = TRUE),
                social_business = sum(type == "Social Business", na.rm = TRUE),
                park = sum(type == "Park", na.rm = TRUE)) %>%
      ungroup()
  }
  ### tally per block #########################
  if(.type == "block"){
    # How many points are in the blcok?
    block_site_traits = blocks %>%
      st_join(y = sites %>% select(type), left = TRUE) %>%
      as_tibble() %>%
      group_by(name, geoid) %>%
      summarize(community_space = sum(type == "Community Space", na.rm = TRUE),
                place_of_worship = sum(type == "Place of Worship", na.rm = TRUE),
                social_business = sum(type == "Social Business", na.rm = TRUE),
                park = sum(type == "Park", na.rm = TRUE)) %>%
      ungroup()


  }
  ### tally per tract #########################
  if(.type == "tract"){
    # How many points are in the tract?
    tract_site_traits = tracts %>%
      st_join(y = sites %>% select(type), left = TRUE) %>%
      as_tibble() %>%
      group_by(name, geoid) %>%
      summarize(community_space = sum(type == "Community Space", na.rm = TRUE),
                place_of_worship = sum(type == "Place of Worship", na.rm = TRUE),
                social_business = sum(type == "Social Business", na.rm = TRUE),
                park = sum(type == "Park", na.rm = TRUE)) %>%
      ungroup()

  }




  if(.type == "grid"){
    # Join them all together!
    output = grid %>%
      left_join(by = c("name", "cell"),
                y = grid_site_traits)  %>%
      left_join(by = c("name", "cell"),
                y = grid_block_traits)  %>%
      left_join(by = c("name", "cell"),
                y = grid_bg_traits) %>%
      mutate_at(vars(community_space:park),
                list(~./pop_density_block * 1000)) %>%
      st_transform(crs = 4326)

    output %>%
      st_write(obj = ., dsn = geo, layer = "tally_grid",
               delete_layer = FALSE, append = TRUE, format = "WKB")
  }

  if(.type == "block"){

    output = blocks %>%
      left_join(by = c("name", "geoid"),
                y = block_site_traits) %>%
      left_join(by = c("name", "geoid"),
                y = block_block_traits) %>%
      left_join(by = c("name", "geoid"),
                y = block_bg_traits) %>%
      mutate_at(vars(community_space:park),
                list(~./pop_density_block * 1000)) %>%
      st_transform(crs = 4326)

    output %>%
      st_write(obj = ., dsn = geo, layer = "tally_block",
               delete_layer = FALSE, append = TRUE, format = "WKB")
  }


  print(.name)

  gc()
}

## 3.1 Get Tally by Grid ###########################

data("connect"); data("meta"); data("wkb_as_sf"); data("aea")
geo = connect()
geo %>% dbRemoveTable("tally_grid")
for(i in meta$cities){ tally_it(.name = i, .type = "grid") }

dbDisconnect(geo); rm(list = ls()); gc()

## 3.2 Get Tally by Block ##########################
data("connect"); data("meta"); data("wkb_as_sf"); data("aea")
geo = connect()
geo %>% dbRemoveTable("tally_block")
for(i in meta$cities){ tally_it(.name = i, .type = "block") }


dbDisconnect(geo); rm(list = ls()); gc()


## 3.3 Get Tally by Tract #############################

tally_it_tract = function(.name = "atlanta"){

  geo = connect()

  # Get tracts for my city
  tracts = geo %>%
    tbl("tracts") %>%
    filter(name == !!.name) %>%
    collect()  %>%
    wkb_as_sf() %>%
    st_as_sf(crs = 4326) %>%
    st_transform(crs = aea)

  tract_data = geo %>%
    tbl("tract_data") %>%
    filter(name == !!.name) %>%
    collect()

  ## sites ####################################

  # Get Sites...
  sites = geo %>%
    tbl("sites") %>%
    filter(name == !!.name) %>%
    collect() %>%
    wkb_as_sf() %>%
    st_as_sf(crs = 4326) %>%
    st_transform(crs = aea)

  ### tally per tract #########################
  # How many points are in the tract?
  tract_site_traits = tracts %>%
    st_join(y = sites %>% select(type), left = TRUE) %>%
    as_tibble() %>%
    group_by(name, geoid) %>%
    summarize(community_space = sum(type == "Community Space", na.rm = TRUE),
              place_of_worship = sum(type == "Place of Worship", na.rm = TRUE),
              social_business = sum(type == "Social Business", na.rm = TRUE),
              park = sum(type == "Park", na.rm = TRUE)) %>%
    ungroup()

  output = tracts %>%
    select(name, geoid, geometry) %>%
    left_join(by = c("name", "geoid"),
              y = tract_site_traits) %>%
    left_join(by = c("name", "geoid"),
              y = tract_data) %>%
    # Social infrastructure sites are measured AS:
    # sites per 1000 residents per square kilometer
    mutate(across(.cols = community_space:park, .f = ~.x/pop_density * 1000)) %>%
    st_transform(crs = 4326)

  output %>%
    st_write(obj = ., dsn = geo, layer = "tally_tract",
             delete_layer = FALSE, append = TRUE, format = "WKB")

  print(.name)
  dbDisconnect(geo)
}

data("connect"); data("meta"); data("wkb_as_sf"); data("aea")
#geo = connect()
#geo %>% dbRemoveTable("tally_tract")
for(i in meta$cities){ tally_it_tract(.name = i) }

geo %>% tbl("tally_tract") %>% filter(name == "berkeley")
geo %>% tbl("tally_tract") %>%
  group_by(name) %>%
  count()

dbDisconnect(geo); rm(list = ls()); gc()




## 3.3 Get Social Capital #############################

#First, let's download the most up to date versions of our social capital indices.

library(dataverse)

data("connect")
geo = connect()
shapes = geo %>%
  tbl("tracts") %>%
  select(geoid, name) %>%
  collect()

dataverse::get_dataframe_by_name(
  filename = "index_tracts_V3_04_10_2022.tab",
  dataset = "doi:10.7910/DVN/OSVCRC",
  server = "dataverse.harvard.edu") %>%
  # Download values needed
  filter(geoid %in% unique(read_csv("census/sample_tracts.csv")$geoid)) %>%
  # Save
  saveRDS("census/sci.rds")

# Save to file
read_rds("census/sci.rds") %>%
  dbWriteTable(conn = geo, name = "sci", value = ., overwrite = TRUE, append = FALSE)

dbDisconnect(geo); rm(list = ls())



## 3.4 Get Dataset ############################################

### by grid ##########################################
library(sf)
library(dplyr)
library(RSQLite)
library(DBI)


data_it = function(.name){

  data("aea")
  data("wkb_as_sf")

  tracts = geo %>%
    tbl("sci") %>%
    filter(year == 2020) %>%
    select(geoid, social_capital:linking) %>%
    right_join(by = c("geoid"), y = tbl(geo, "tracts") %>% filter(name == !!.name)) %>%
    collect() %>%
    wkb_as_sf() %>%
    st_as_sf(crs = 4326) %>%
    st_transform(crs = aea)

  # Get social capital on a grid
  grid = geo %>%
    tbl("tally_grid") %>%
    select(cell, name, geometry) %>%
    filter(name == !!.name) %>%
    collect() %>%
    wkb_as_sf() %>%
    st_as_sf(crs = 4326) %>%
    st_transform(crs = aea) %>%
    st_join(y = tracts %>% select(-name), left = TRUE) %>%
    # Convert to tibble
    as_tibble() %>%
    # For each cell-name combo,
    group_by(cell, name) %>%
    # Take the average
    summarize(across(social_capital:linking, ~mean(., na.rm = TRUE))) %>%
    ungroup() %>%
    # And set NAN to NA
    mutate(across(social_capital:linking, ~if_else(is.nan(.), NA_real_, .)))

  # Get entire !!.name on a grid
  output = geo %>%
    tbl("tally_grid") %>%
    filter(name == !!.name) %>%
    mutate(total = community_space + park + social_business + place_of_worship) %>%
    # Zoom into just populated sites
    filter(pop_density_block > 0) %>%
    select(cell, name, total, community_space:park, pop_density_block, pop_density_bg, geometry,
           white_block, black_block, asian_block, hisplat_block,
           median_household_income_bg, income_0_60K_bg, some_college_bg, over_65, unemployment_bg) %>%
    collect() %>%
    # Join in SCI
    left_join(by = c("cell", "name"), y = grid)

  # Write to file!
  output %>%
    st_write(obj = ., dsn = geo, layer = "data_grid",
             delete_layer = FALSE, append = TRUE, format = "WKB")
  print(.name)
}

data("meta")
data("connect")
geo = connect()

geo %>% dbRemoveTable("data_grid")
for(i in meta$cities){ data_it(.name = i) }

# Fix a name...
geo %>%
  tbl("data_grid") %>%
  rename(over_65_bg = over_65) %>%
  collect() %>%
  st_write(obj = ., dsn = geo, layer = "data_grid",
           delete_layer = TRUE,format = "WKB")

dbDisconnect(geo); rm(list = ls()); gc()

### by tract ###################################


data("meta")
data("connect")
geo = connect()

geo %>%
  tbl("tally_tract") %>%
  # Join in SCI
  left_join(by = "geoid", y = geo %>% tbl("sci") %>%
              filter(year == max(year, na.rm = TRUE)) %>%
              select(geoid, social_capital, bonding, bridging, linking)) %>%
  # Calculate rate of total social infrastructure
  # by summing the rates (they are all normalized by the same population density per tract)
  mutate(total = community_space + park + social_business + place_of_worship) %>%
  select(name, geoid, total, community_space:park, pop_density, pop, area, geometry,
       white, black, asian, hisplat,
       median_household_income, income_0_60K, some_college, over_65, unemployment,
       social_capital:linking) %>%
  collect() %>%
  # And set NAN to NA
  mutate(across(.cols = any_of(c("social_capital", "bonding", "bridging", "linking")),
                .f = ~if_else(is.nan(.x), NA_real_, .x))) %>%
  st_write(obj = ., dsn = geo, layer = "data_tract",
           delete_layer = TRUE,format = "WKB")

dbDisconnect(geo); rm(list = ls()); gc()



