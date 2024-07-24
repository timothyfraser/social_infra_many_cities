#' @name `04_census.R`
#'
#' @description Code for generating census covariates.

### Get List of Relevant Tracts

# Let's get a list of all census tracts of interest.
# This means, all census tracts located within or overlapping with our city boundaries.

library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tidyr)
library(sf)

# 1. Download from Census ################################

dir.create("census")

# Get EPSG:4326 (WGS 84) projection
data("wgs")
data("connect")

data("meta")

meta$cities %>%
  paste0("search/", ., "/tracts.geojson") %>%
  purrr::map(~read_sf(.) %>% select(geoid)) %>%
  bind_rows() %>%
  write_csv("census/sample_tracts.csv")

# geo %>%
#   tbl("tracts") %>%
#   select(geoid) %>%
#   distinct()  %>%
#   collect() %>%
#   write_csv("census/sample_tracts.csv")

# Get list of unique tracts
geo %>%
  tbl("tracts") %>%
  select(geoid) %>%
  distinct()  %>%
  collect() %>%
  write_csv("census/sample_tracts.csv")

# Simpler version


### Get Census Block Data ##############################

library(censusapi)

myvars <- list("GEO_ID" = "geoid",
               "P1_001N" = "pop",
               "P1_003N" = "white",
               "P1_004N" = "black",
               "P1_005N" = "natam",
               "P1_006N" = "asian",
               "P2_002N" = "hisplat",
               "H1_001N" = "units",
               "H1_002N" = "units_occupied") %>%
  as_tibble() %>%
  pivot_longer(cols = -c(), names_to = "id", values_to = "variable")


library(future)
library(furrr)

# Set up parallel processing

# Check current worksers
nbrOfWorkers()
# check available workers
numworkers <- availableWorkers() %>% length()
# Initiate parallel processing
# Always use at least 1 fewer than the total available workers
plan(multisession, workers = numworkers - 1)

myunits <- read_csv("census/sample_tracts.csv") %>%
  mutate(state = str_sub(geoid, 1,2),
         county = str_sub(geoid, 3,5),
         tract = str_sub(geoid, 6,-1)) %>%
  mutate(geoid = str_sub(geoid, 1,5)) %>%
  select(geoid, state, county) %>%
  distinct()

# For reference!
# https://api.census.gov/data/2020/dec/pl/examples.html
# https://api.census.gov/data/2020/dec/pl/variables.html
myunits %>%
  split(.$geoid) %>%
  future_map_dfr(
    ~censusapi::getCensus(
      key = Sys.getenv("CENSUS_API_KEY"),
      vintage = "2020",
      name = "dec/pl",
      region = "block:*",
      regionin = paste(
        "state:", .$state, # Choose just blocks in this state
        "&", "county:", .$county,  # in this county
        #        "&", "tract:", .$tract, # in this tract
        sep = ""),
      # Grab these variables
      vars = myvars$id),
    .progress = TRUE) %>%
  # Keep just the rows with our specified names
  select(myvars$id) %>%
  # Rename them to simpler, more easily understandable names
  magrittr::set_colnames(value = c(myvars$variable)) %>%
  # better format the geoid
  mutate(geoid = str_remove(geoid, "1000000US"),
         tract = str_sub(geoid, 1, 11)) %>%
  # Filter to just blocks in our tracts of interest
  filter(tract %in% read_csv("census/sample_tracts.csv")$geoid) %>%
  select(-tract) %>%
  # Convert subcategories into percentages
  mutate(white = white / pop,
         black = black / pop,
         asian = asian / pop,
         natam = asian / pop,
         hisplat = hisplat / pop,
         units_occupied = units_occupied / units) %>%
  # Save to file
  saveRDS("census/block_data.rds")

read_rds("census/block_data.rds") %>%
  mutate(
    across(.cols = c(white, black, natam, asian, hisplat, units_occupied),
           .fns = ~if_else(
             condition = is.nan(.x) | is.infinite(.x),
             true = NA_real_,
             false = round(.x, 4) ))
  ) %>%
  saveRDS("census/block_data.rds")


plan(sequential) # end parallel processing
gc() # clear cache




### Get Census Block Group Data ###############################

library(censusapi)

myvars <- list(
  # 2020 ACS5 variables
  "GEO_ID" = "geoid",
  "B01003_001E" = "pop", # Total Population
  # Race
  "B02001_001E" = "pop_race",
  "B02001_002E" = "white",
  "B02001_003E" = "black",
  "B02001_004E" = "natam",
  "B02001_005E" = "asian",
  "B02001_006E" = "pacific",
  # Ethnicity
  "B03003_001E" = "pop_ethnicity",
  "B03003_003E" = "hispanic",
  # Age
  "B01001_020E" = "pop_age_65_66_male",
  "B01001_021E" = "pop_age_67_69_male",
  "B01001_022E" = "pop_age_70_74_male",
  "B01001_023E" = "pop_age_75_79_male",
  "B01001_024E" = "pop_age_80_84_male",
  "B01001_025E" = "pop_age_85_over_male",

  "B01001_044E" = "pop_age_65_66_female",
  "B01001_045E" = "pop_age_67_69_female",
  "B01001_046E" = "pop_age_70_74_female",
  "B01001_047E" = "pop_age_75_79_female",
  "B01001_048E" = "pop_age_80_84_female",
  "B01001_049E" = "pop_age_85_over_female",

  "B01001_026E" = "pop_women", # Gender

  #"EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER"
  "B15003_001E" = "pop_education",
  "B15003_019E" = "edu_some_college_under_1",
  "B15003_020E" = "edu_some_college_over_1",
  "B15003_021E" = "edu_associates",
  "B15003_022E" = "edu_bachelors",
  "B15003_023E" = "edu_masters",
  "B15003_024E" = "edu_professional",
  "B15003_025E" = "edu_doctoral",
  # Median household income in the past 12 months (in 2020 inflation-adjusted dollars)
  "B19013_001E" = "median_household_income",
  # FAMILY INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS)
  "B19101_001E" = "pop_income",	#Estimate!!Total:
  "B19101_002E" = "pop_under_10000",	#Estimate!!Total:!!Less than $10,000
  "B19101_003E" = "pop_10000_14999",	#Estimate!!Total:!!$10,000 to $14,999
  "B19101_004E" = "pop_15000_19999",	#Estimate!!Total:!!$15,000 to $19,999
  "B19101_005E" = "pop_20000_24999",	#Estimate!!Total:!!$20,000 to $24,999
  "B19101_006E" = "pop_25000_29999",	#Estimate!!Total:!!$25,000 to $29,999
  "B19101_007E" = "pop_30000_34999",	#Estimate!!Total:!!$30,000 to $34,999
  "B19101_008E" = "pop_35000_39999",	#Estimate!!Total:!!$35,000 to $39,999
  "B19101_009E" = "pop_40000_44999",	#Estimate!!Total:!!$40,000 to $44,999
  "B19101_010E" = "pop_45000_49999",	#Estimate!!Total:!!$45,000 to $49,999
  "B19101_011E" = "pop_50000_59999",	#Estimate!!Total:!!$50,000 to $59,999
  "B19101_012E" = "pop_60000_74999",	#Estimate!!Total:!!$60,000 to $74,999
  "B19101_013E" = "pop_75000_99999",	#Estimate!!Total:!!$75,000 to $99,999
  "B19101_014E" = "pop_100000_124999",	#Estimate!!Total:!!$100,000 to $124,999
  "B19101_015E" = "pop_125000_149999",	#Estimate!!Total:!!$125,000 to $149,999
  "B19101_016E" = "pop_150000_199999",	#Estimate!!Total:!!$150,000 to $199,999
  "B19101_017E" = "pop_200000_plus",	#Estimate!!Total:!!$200,000 or more
  # Gini Index of Income Inequality
  "B19083_001E" = "gini", # GINI INDEX OF INCOME INEQUALITY
  # Housing Costs
  "B25105_001E" = "median_monthly_housing_costs", # Median Monthly Housing Costs in USD
  # Employment
  "B23025_003E" = "pop_labor_force", # Estimate!!Total!!In labor force!!Civilian labor force
  "B23025_004E" = "pop_employed", #Estimate!!Total!!In labor f orce!!Civilian labor force!!Employed
  "B23025_005E" = "pop_unemployed", # Estimate!!Total:!!In labor force:!!Civilian labor force:!!Unemployed
  "B29001_001E" = "pop_voting_age" # Citizen, Voting Age Population
) %>%
  as_tibble() %>%
  pivot_longer(cols = -c(), names_to = "id", values_to = "variable")


library(future)
library(furrr)

# Set up parallel processing

# Check current worksers
nbrOfWorkers()
# check available workers
numworkers <- availableWorkers() %>% length()
# Initiate parallel processing
# Always use at least 1 fewer than the total available workers
plan(multisession, workers = numworkers - 1)

myunits <- read_csv("census/sample_tracts.csv") %>%
  mutate(state = str_sub(geoid, 1,2),
         county = str_sub(geoid, 3,5),
         tract = str_sub(geoid, 6,-1)) %>%
  mutate(geoid = str_sub(geoid, 1,5)) %>%
  select(geoid, state, county) %>%
  distinct()

myunits %>%
  split(.$geoid) %>%
  future_map_dfr(
    ~censusapi::getCensus(
      name = "acs/acs5",
      vintage = 2020,
      key = Sys.getenv("CENSUS_API_KEY"),
      # Grab these variables
      vars = myvars$id,
      region = "block group:*",
      regionin = paste(
        "state:", .$state, # Choose just blocks in this state
        "&", "county:", .$county,  # in this county
        #        "&", "tract:", .$tract, # in this tract
        sep = "")),
    .progress = TRUE) %>%
  # Keep just the rows with our specified names
  select(myvars$id) %>%
  # Rename them to simpler, more easily understandable names
  magrittr::set_colnames(value = c(myvars$variable)) %>%
  # better format the geoid
  mutate(geoid = str_remove(geoid, "1500000US"),
         tract = str_sub(geoid, 1, 11)) %>%
  # Filter to just block groups in our tracts of interest
  filter(tract %in% read_csv("census/sample_tracts.csv")$geoid) %>%
  select(-tract) %>%
  # Save to file
  saveRDS("census/bg_data.rds")

plan(sequential)


# Adjust data
read_rds("census/bg_data.rds") %>%
  # Convert subcategories into percentages
  mutate(
    women = pop_women / pop,
    white = white / pop_race,
    black = black / pop_race,
    asian = asian / pop_race,
    natam = natam / pop_race,
    pacific = pacific / pop_race,
    hisplat = hispanic / pop_ethnicity,
    unemployment = pop_unemployed / pop_labor_force * 1000,
    some_college = (edu_some_college_under_1 + edu_some_college_over_1 + edu_associates +
                      edu_bachelors + edu_masters + edu_professional + edu_doctoral) / pop_education, # % of those over 25 with ANY college education
    over_65 = (pop_age_65_66_male + pop_age_67_69_male +
                 pop_age_70_74_male + pop_age_75_79_male +
                 pop_age_80_84_male + pop_age_85_over_male +
                 pop_age_65_66_female + pop_age_67_69_female +
                 pop_age_70_74_female + pop_age_75_79_female +
                 pop_age_80_84_female + pop_age_85_over_female) / pop,
    income_0_60K = (pop_under_10000 + pop_10000_14999 +
                      pop_15000_19999 + pop_20000_24999 +
                      pop_25000_29999 + pop_30000_34999 +
                      pop_35000_39999 + pop_40000_44999 +
                      pop_45000_49999 + pop_50000_59999) / pop_income) %>%
  # Reset any nans to NA
  mutate(across(.cols = c(women, white, black, asian, natam, pacific, hisplat,
                          unemployment, some_college, over_65, income_0_60K,
                          median_household_income, median_monthly_housing_costs, gini),
            .fns = ~if_else(is.nan(.x) | is.infinite(.x), NA_real_, as.numeric(.x)))) %>%
  select(geoid, pop, women, over_65, hisplat, white, black, asian, natam, pacific,
         some_college, income_0_60K, median_household_income,
         median_monthly_housing_costs, gini, unemployment) %>%
  saveRDS("census/bg_data.rds")


gc()

### Get Census Tract Data ###############################
library(censusapi)
library(stringr)
library(dplyr)
library(readr)

# myvars <- list("GEO_ID" = "geoid",
#                "P1_001N" = "pop",
#                "P1_003N" = "white",
#                "P1_004N" = "black",
#                "P1_005N" = "natam",
#                "P1_006N" = "asian",
#                "P2_002N" = "hisplat",
#                "H1_001N" = "units",
#                "H1_002N" = "units_occupied") %>%
#   as_tibble() %>%
#   pivot_longer(cols = -c(), names_to = "id", values_to = "variable")


myvars <- list(
  # 2020 ACS5 variables
  "GEO_ID" = "geoid",
  "B01003_001E" = "pop", # Total Population
  # Race
  "B02001_001E" = "pop_race",
  "B02001_002E" = "white",
  "B02001_003E" = "black",
  "B02001_004E" = "natam",
  "B02001_005E" = "asian",
  "B02001_006E" = "pacific",
  # Ethnicity
  "B03003_001E" = "pop_ethnicity",
  "B03003_003E" = "hispanic",
  # Age
  "B01001_020E" = "pop_age_65_66_male",
  "B01001_021E" = "pop_age_67_69_male",
  "B01001_022E" = "pop_age_70_74_male",
  "B01001_023E" = "pop_age_75_79_male",
  "B01001_024E" = "pop_age_80_84_male",
  "B01001_025E" = "pop_age_85_over_male",

  "B01001_044E" = "pop_age_65_66_female",
  "B01001_045E" = "pop_age_67_69_female",
  "B01001_046E" = "pop_age_70_74_female",
  "B01001_047E" = "pop_age_75_79_female",
  "B01001_048E" = "pop_age_80_84_female",
  "B01001_049E" = "pop_age_85_over_female",

  "B01001_026E" = "pop_women", # Gender

  #"EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER"
  "B15003_001E" = "pop_education",
  "B15003_019E" = "edu_some_college_under_1",
  "B15003_020E" = "edu_some_college_over_1",
  "B15003_021E" = "edu_associates",
  "B15003_022E" = "edu_bachelors",
  "B15003_023E" = "edu_masters",
  "B15003_024E" = "edu_professional",
  "B15003_025E" = "edu_doctoral",
  # Median household income in the past 12 months (in 2020 inflation-adjusted dollars)
  "B19013_001E" = "median_household_income",
  # FAMILY INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS)
  "B19101_001E" = "pop_income",	#Estimate!!Total:
  "B19101_002E" = "pop_under_10000",	#Estimate!!Total:!!Less than $10,000
  "B19101_003E" = "pop_10000_14999",	#Estimate!!Total:!!$10,000 to $14,999
  "B19101_004E" = "pop_15000_19999",	#Estimate!!Total:!!$15,000 to $19,999
  "B19101_005E" = "pop_20000_24999",	#Estimate!!Total:!!$20,000 to $24,999
  "B19101_006E" = "pop_25000_29999",	#Estimate!!Total:!!$25,000 to $29,999
  "B19101_007E" = "pop_30000_34999",	#Estimate!!Total:!!$30,000 to $34,999
  "B19101_008E" = "pop_35000_39999",	#Estimate!!Total:!!$35,000 to $39,999
  "B19101_009E" = "pop_40000_44999",	#Estimate!!Total:!!$40,000 to $44,999
  "B19101_010E" = "pop_45000_49999",	#Estimate!!Total:!!$45,000 to $49,999
  "B19101_011E" = "pop_50000_59999",	#Estimate!!Total:!!$50,000 to $59,999
  "B19101_012E" = "pop_60000_74999",	#Estimate!!Total:!!$60,000 to $74,999
  "B19101_013E" = "pop_75000_99999",	#Estimate!!Total:!!$75,000 to $99,999
  "B19101_014E" = "pop_100000_124999",	#Estimate!!Total:!!$100,000 to $124,999
  "B19101_015E" = "pop_125000_149999",	#Estimate!!Total:!!$125,000 to $149,999
  "B19101_016E" = "pop_150000_199999",	#Estimate!!Total:!!$150,000 to $199,999
  "B19101_017E" = "pop_200000_plus",	#Estimate!!Total:!!$200,000 or more
  # Gini Index of Income Inequality
  "B19083_001E" = "gini", # GINI INDEX OF INCOME INEQUALITY
  # Housing Costs
  "B25105_001E" = "median_monthly_housing_costs", # Median Monthly Housing Costs in USD
  # Employment
  "B23025_003E" = "pop_labor_force", # Estimate!!Total!!In labor force!!Civilian labor force
  "B23025_004E" = "pop_employed", #Estimate!!Total!!In labor f orce!!Civilian labor force!!Employed
  "B23025_005E" = "pop_unemployed", # Estimate!!Total:!!In labor force:!!Civilian labor force:!!Unemployed
  "B29001_001E" = "pop_voting_age" # Citizen, Voting Age Population
) %>%
  as_tibble() %>%
  pivot_longer(cols = -c(), names_to = "id", values_to = "variable")




library(future)
library(furrr)

# Set up parallel processing

# Check current worksers
nbrOfWorkers()
# check available workers
numworkers <- availableWorkers() %>% length()
# Initiate parallel processing
# Always use at least 1 fewer than the total available workers
plan(multisession, workers = numworkers - 1)

myunits <- read_csv("census/sample_tracts.csv") %>%
  mutate(state = str_sub(geoid, 1,2),
         county = str_sub(geoid, 3,5),
         tract = str_sub(geoid, 6,-1)) %>%
  mutate(geoid = str_sub(geoid, 1,5)) %>%
  select(geoid, state, county) %>%
  distinct()

# For reference!
# https://api.census.gov/data/2020/dec/pl/examples.html
# https://api.census.gov/data/2020/dec/pl/variables.html

myunits %>%
  split(.$geoid) %>%
  future_map_dfr(~censusapi::getCensus(
      key = Sys.getenv("CENSUS_API_KEY"),
      name = "acs/acs5",
      vintage = 2020,
      region = "tract:*",
      regionin = paste(
        "state:", .$state, # Choose just tracts in this state
        "&", "county:", .$county,  # in this county
        sep = ""),
      # Grab these variables
      vars = myvars$id),
      .progress = TRUE
  ) %>%
  saveRDS("census/tract_data.rds")

# Filter the data
read_rds("census/tract_data.rds") %>%
  # Keep just the rows with our specified names
  select(any_of(myvars$id)) %>%
  # Rename them to simpler, more easily understandable names
  magrittr::set_colnames(value = c(myvars$variable)) %>%
  # better format the geoid
  mutate(geoid = str_remove(geoid, "1400000US")) %>%
  # Filter to just tracts in our tracts of interest
  filter(geoid %in% read_csv("census/sample_tracts.csv")$geoid) %>%
  saveRDS("census/tract_data.rds")


# Format the data as final variables
read_rds("census/tract_data.rds") %>%
  mutate(
    women = pop_women / pop,
    white = white / pop_race,
    black = black / pop_race,
    asian = asian / pop_race,
    natam = natam / pop_race,
    pacific = pacific / pop_race,
    hisplat = hispanic / pop_ethnicity,
    unemployment = pop_unemployed / pop_labor_force * 1000,
    some_college = (edu_some_college_under_1 + edu_some_college_over_1 + edu_associates +
                      edu_bachelors + edu_masters + edu_professional + edu_doctoral) / pop_education, # % of those over 25 with ANY college education
    over_65 = (pop_age_65_66_male + pop_age_67_69_male +
                 pop_age_70_74_male + pop_age_75_79_male +
                 pop_age_80_84_male + pop_age_85_over_male +
                 pop_age_65_66_female + pop_age_67_69_female +
                 pop_age_70_74_female + pop_age_75_79_female +
                 pop_age_80_84_female + pop_age_85_over_female) / pop,
    income_0_60K = (pop_under_10000 + pop_10000_14999 +
                      pop_15000_19999 + pop_20000_24999 +
                      pop_25000_29999 + pop_30000_34999 +
                      pop_35000_39999 + pop_40000_44999 +
                      pop_45000_49999 + pop_50000_59999) / pop_income) %>%
  # Reset any nans to NA
  mutate(across(.cols = c(women, white, black, asian, natam, pacific, hisplat,
                          unemployment, some_college, over_65, income_0_60K,
                          median_household_income, median_monthly_housing_costs, gini),
                .fns = ~if_else(
                  condition = is.nan(.x) | is.infinite(.x) | .x == -666666666,
                  true = NA_real_, false = as.numeric(.x)))) %>%
  select(geoid, pop, women, over_65, hisplat, white, black, asian, natam, pacific,
         some_college, income_0_60K, median_household_income,
         median_monthly_housing_costs, gini, unemployment) %>%
  saveRDS("census/tract_data.rds")



# read_rds("census/tract_data.rds") %>% glimpse()
# read_rds("census/tract_data2.rds") %>% glimpse()

#
# myunits %>%
#   split(.$geoid) %>%
#   future_map_dfr(
#     ~censusapi::getCensus(
#       key = Sys.getenv("CENSUS_API_KEY"),
#       vintage = "2020",
#       name = "dec/pl",
#       region = "tract:*",
#       regionin = paste(
#         "state:", .$state, # Choose just blocks in this state
#         "&", "county:", .$county,  # in this county
#         sep = ""),
#       # Grab these variables
#       vars = myvars$id),
#     .progress = TRUE) %>%
#   # Keep just the rows with our specified names
#   select(myvars$id) %>%
#   # Rename them to simpler, more easily understandable names
#   magrittr::set_colnames(value = c(myvars$variable)) %>%
#   # Convert subcategories into percentages
#   mutate(white = white / pop,
#          black = black / pop,
#          asian = asian / pop,
#          natam = natam / pop,
#          hisplat = hisplat / pop,
#          units_occupied = units_occupied / units) %>%
#   # Reset any nans to NA
#   mutate_at(vars(white:units_occupied),
#             list(~if_else(is.nan(.) | is.infinite(.), NA_real_, as.numeric(.)))) %>%
#   # better format the geoid
#   mutate(geoid = str_remove(geoid, "1400000US")) %>%
#   # Filter to just blocks in our tracts of interest
#   filter(geoid %in% read_csv("census/sample_tracts.csv")$geoid) %>%
#   # Save to file
#   saveRDS("census/tract_data.rds")

plan(sequential)
gc()
rm(list = ls())





