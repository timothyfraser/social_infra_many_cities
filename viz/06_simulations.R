# SIMULATION ##############################





### functions ###########################################


get_metadata = function(path_models = "models/models.rds"){


  data = path_models %>%
    read_rds() %>%
    names() %>%
    tibble(model = .) %>%
    mutate(modelid = 1:n()) %>%
    mutate(type = str_extract(model, "total|subtypes")) %>%
    mutate(outcome = str_extract(model, "bonding|bridging|linking|social_capital"))

  group1 = data %>%
    filter(type == "total") %>%
    mutate(treatment = "total")
  group2 = data %>%
    filter(type == "subtypes") %>%
    expand_grid(treatment = c("park", "community_space", "place_of_worship", "social_business"))

  output = bind_rows(group1, group2)   %>%
    mutate(groupid = 1:n())
  return(output)
}


# For each model, we'll make a tibble that contains the
# values that should be used to rescale a predictor scenario value

get_rescalers = function(model){
  library(stringr)
  library(dplyr)
  library(readr)
  library(tidyr)

  # Testing value
  # model = read_rds("models/models.rds")[[5]]

  # Get a set of labels
  labels = tibble(
    # Get the term label for the fixed effects
    term = model %>%
      with(terms) %>%
      attr("term.labels"),
    # Is it rescaled?
    scaled = str_detect(term, "scale"),
    # Extract the raw variable name
    var = term %>% str_extract(
      pattern = paste0(
        c("total",
          "community_space",
          "park",
          "place_of_worship",
          "social_business",
          "pop_density",
          "black",
          "hisplat",
          "asian",
          "median_household_income",
          "income_0_60K",
          "some_college",
          "over_65",
          "unemployment"),
        collapse = "|"
      )
    )
  )



  # Make a logit function with an optional adjustment for 0 cases
  logit = function(p, adj = 0) { log( (p + adj) / (1 - (p - adj) ))}


  frame = model %>%
    with(data) %>%
    select_if(is.numeric) %>%
    pivot_longer(cols = -c(),
                 names_to = "var",
                 values_to = "x") %>%
    # Join in the terms matching those variables in the dataset
    inner_join(by = "var", y = labels) %>%
    # Log/logit transform any applicable variables
    # all models consistently transform variables,
    # (though some models normalized the transformed variables too.
    #  we'll deal with that next.)
    mutate(xhat = case_when(
      var == "intercept" ~ x,
      var == "total" ~ log(x + 1),
      var == "park" ~ log(x + 1),
      var == "community_space" ~ log(x + 1),
      var == "social_business" ~ log(x + 1),
      var == "place_of_worship" ~ log(x + 1),

      var == "pop_density" ~ log(x),
      var == "black" ~ logit(x, 0.01),
      var == "hisplat" ~logit(x, 0.01),
      var == "asian" ~logit(x, 0.01),
      var == "median_household_income" ~ log(x),
      var == "income_0_60K" ~ logit(x, 0.01),
      var == "some_college" ~ logit(x, 0.01),
      var == "over_65" ~ logit(x, 0.01),
      var == "unemployment" ~ log(x + 1),
      TRUE ~ x
    ))

  stats = frame %>%
    group_by(var) %>%
    summarize(mean = mean(xhat, na.rm = TRUE),
              sd = sd(xhat, na.rm = TRUE))

  output = labels %>%
    left_join(by = "var", y = stats)
  return(output)
}


# Write a function to get sigma for every model.
get_sigma = function(path_models = "models/models.rds"){

  path_models = "models/models.rds"
  m = path_models %>% read_rds()

  output = names(m) %>%
    map_dfr(~tibble(modelid = .x, sigma = m[[.x]]$sigma))
  return(output)
}

get_effects = function(model){
  # Testing values
  # model = read_rds("models/models.rds")[1]

  # This is the population level terms
  fixed = tibble(name = model$groups$name %>% unique()) %>%
    group_by(name) %>%
    reframe(
      tibble(
        term = model$coefficients$fixed %>% names(),
        beta = model$coefficients$fixed)
    ) %>%
    mutate(type = "fixed") %>%
    mutate(var = term %>% dplyr::recode(
      "(Intercept)" = "intercept",

      "scale(log(total + 1))" = "total",
      "scale(log(community_space + 1))" = "community_space",
      "scale(log(place_of_worship + 1))" = "place_of_worship",
      "scale(log(social_businesse + 1))" = "social_businesse",
      "scale(log(park + 1))" = "park",
      "scale(log(pop_density))" =  "pop_density",

      "scale(logit(black, 0.01))" = "black",
      "scale(logit(hisplat, 0.01))" =  "hisplat",

      "scale(logit(asian, 0.01))" = "asian",
      "scale(log(median_household_income))" = "median_household_income",
      "scale(logit(income_0_60K, 0.01))"= "income_0_60K",
      "scale(logit(some_college, 0.01))"= "some_college",
      "scale(logit(over_65, 0.01))"= "over_65",
      "scale(log(unemployment + 1))" = "unemployment",

      "log(total + 1)" = "total",
      "log(community_space + 1)" = "community_space",
      "log(place_of_worship + 1)" = "place_of_worship",
      "log(social_businesse + 1)" = "social_businesse",
      "log(park + 1)" = "park",
      "log(pop_density)" =  "pop_density",

      "logit(black, 0.01)" = "black",
      "logit(hisplat, 0.01)" =  "hisplat",

      "logit(asian, 0.01)" = "asian",
      "log(median_household_income)" = "median_household_income",
      "logit(income_0_60K, 0.01)"= "income_0_60K",
      "logit(some_college, 0.01)"= "some_college",
      "logit(over_65, 0.01)"= "over_65",
      "log(unemployment + 1)" = "unemployment")
    ) %>%
    group_by(name) %>%
    mutate(order = 1:n())

  random = tibble(
    name = model$coefficients$random$name %>% rownames(),
    model$coefficients$random$name %>% as_tibble()
  ) %>%
    pivot_longer(cols = -c(name), names_to = "term", values_to = "beta") %>%
    mutate(type = "random")  %>%
    mutate(var = term %>% dplyr::recode(
      "(Intercept)" = "intercept",
      "scale(log(total + 1))" = "total",
      "scale(log(community_space + 1))" = "community_space",
      "scale(log(place_of_worship + 1))" = "place_of_worship",
      "scale(log(social_businesse + 1))" = "social_businesse",
      "scale(log(park + 1))" = "park",
      "log(total + 1)" = "total",
      "log(community_space + 1)" = "community_space",
      "log(place_of_worship + 1)" = "place_of_worship",
      "log(social_businesse + 1)" = "social_businesse",
      "log(park + 1)" = "park"
    )) %>%
    # Join in the order id for these terms
    left_join(by = c("name", "var"), y = fixed %>% select(name, var, order))

  effects = bind_rows(random, fixed)

  # Each scenario has an overall intercept an a group-specific intercept
  # as well as an overall slope for 'total' and a group-specific slope for 'total'
  # if we can add these per variable,
  # then we can still use a multivariate normal distribution for simulation.

  effects = effects %>%
    ungroup() %>%
    group_by(name, term, var, order) %>%
    summarize(beta = sum(beta, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(name, order)


  return(effects)
}



# Get model scenariodata
get_scenarios = function(case, path_models = "models/models.rds"){
  # Testing values
  # case = read_csv("simulations/metadata.csv")[5,]; path_models = "models/models.rds"

  # Case should be a single slice of the metadata

  .type = case$type[1]
  .outcome = case$outcome[1]
  .treatment = case$treatment[1]
  .modelid = case$modelid[1]

  # Get the model for that type and outcome
  m = read_rds(path_models)[[.modelid]]
  # Now generate the scenario data
  x0 = m$data %>% mutate(scenario = 1)
  if(.type == "total"){
    x1 = m$data %>% mutate(scenario = 2) %>% mutate(total = total + 1)
  }else if(.type == "subtypes" & .treatment == "park"){
    x1 = m$data %>% mutate(scenario = 2) %>% mutate(park = park + 1)
  }else if(.type == "subtypes" & .treatment == "community_space"){
    x1 = m$data %>% mutate(scenario = 2) %>% mutate(community_space = community_space + 1)
  }else if(.type == "subtypes" & .treatment == "place_of_worship"){
    x1 = m$data %>% mutate(scenario = 2) %>% mutate(place_of_worship = place_of_worship + 1)
  }else if(.type == "subtypes" & .treatment == "social_business"){
    x1 = m$data %>% mutate(scenario = 2) %>% mutate(social_business = social_business + 1)
  }
  # bundle into one dataframe
  data = bind_rows(x0, x1)
  # Cleanup
  remove(x0,x1)

  return(data)
}

get_many_scenarios = function(case, x = 1, path_models = "models/models.rds"){

  .type = case$type[1]
  .outcome = case$outcome[1]
  .treatment = case$treatment[1]
  .modelid = case$modelid[1]

  # Get the model for that type and outcome
  m = read_rds(path_models)[[.modelid]]
  if(.type == "total"){
    x1 = m$data %>% mutate(scenario = 1) %>% mutate(total = total + x)
  }else if(.type == "subtypes" & .treatment == "total"){
    x1 = m$data %>% mutate(scenario = 1) %>% mutate(total = total + x)
  }
  return(x1)
}


# For each model, generate the 'frame',
# Meaning the combination of beta coefficients and x values for each scenario
get_equations = function(data, path_metadata = "simulations/metadata.csv",
                         path_rescalers = "simulations/rescalers.csv",
                         path_effects = "simulations/effects.csv"
){

  # data = read_rds("simulations/scenarios.rds") %>%
  #    filter(groupid == 13)
  # path_metadata = "simulations/metadata.csv";
  # path_rescalers = "simulations/rescalers.csv";
  # path_effects = "simulations/effects.csv"


  # Testing values
  # model = m3
  # type = "total"
  # n = 2
  metadata = path_metadata %>% read_csv()
  # Just rows that matter
  type = c("park", "community_space", "place_of_worship", "social_business", "total")

  newdata = data %>%
    select(groupid, scenario, name, any_of("geoid"), any_of(type), park, pop_density,
           black, hisplat, asian, income_0_60K, median_household_income,
           some_college, over_65, unemployment) %>%
    mutate(intercept = 1) %>%
    pivot_longer(cols = -c(groupid, scenario, name, geoid), names_to = "var", values_to = "x")


  # Make a logit function with an optional adjustment for 0 cases
  logit = function(p, adj = 0) { log( (p + adj) / (1 - (p - adj) ))}

  # Perform first-stage variable transformations,
  # which are consistent across models
  newdata = newdata %>%
    mutate(xhat = case_when(
      var == "intercept" ~ x,
      var == "total" ~ log(x + 1),
      var == "park" ~ log(x + 1),
      var == "community_space" ~ log(x + 1),
      var == "social_business" ~ log(x + 1),
      var == "place_of_worship" ~ log(x + 1),

      var == "pop_density" ~ log(x),
      var == "black" ~ logit(x, 0.01),
      var == "hisplat" ~logit(x, 0.01),
      var == "asian" ~logit(x, 0.01),
      var == "median_household_income" ~ log(x),
      var == "income_0_60K" ~ logit(x, 0.01),
      var == "some_college" ~ logit(x, 0.01),
      var == "over_65" ~ logit(x, 0.01),
      var == "unemployment" ~ log(x + 1),
      # Otherwise, keep as is
      TRUE ~ x
    ))


  # For every simulation effect group id,
  # get the rescaling data for the terms in that group's model.
  rescalers = metadata %>%
    select(groupid, modelid, model) %>%
    left_join(by = c("model" = "modelid"), y = path_rescalers %>% read_csv(), multiple = "all") %>%
    # Next, we'll take our mean and standard deviation and say
    # Hey, was this variable scaled?
    # If so, keep the mean and standard deviation as is; we'll need them for rescaling.
    # If this variable was not scaled, though, we'll just switch the values to mean = 0 and sd = 1
    # This means that when we rescale later, (x - 0) / 1 = x
    # so those values will be unaffected
    mutate(mean = case_when(scaled == TRUE ~ mean, scaled == FALSE ~ 0),
           sd = case_when(scaled == TRUE ~ sd, scaled == FALSE ~ 1)) %>%
    # Don't need scale indicator anymore
    select(groupid, var, mean, sd)


  test = newdata %>%
    # Join in the rescaling values...
    left_join(by = c("groupid", "var"), y = rescalers) %>%
    # Rescale any applicable values
    mutate(xhat = (xhat - mean) / sd) %>%
    # Keep primary columns
    select(groupid, scenario, name, geoid, var, xhat)

  # Get the effects for each model
  effects = path_effects %>%
    read_csv() %>%
    left_join(by = c("modelid" = "model"), y= metadata %>% select(model, modelid) %>% distinct()) %>%
    select(-modelid) %>%
    rename(modelid = modelid.y)

  # Create a data.frame containing the components of each area's equation
  equations = test %>%
    # Join in the modelid for that group
    left_join(by = "groupid", y = metadata %>% select(groupid, modelid)) %>%
    # Join in the effects for that model
    inner_join(by = c("modelid", "name", "var"), y = effects %>% select(-term))

  # Set the intercept to a constant 1.
  equations = equations %>%
    mutate(xhat = if_else(var == "intercept", true = 1, false = xhat))

  return(equations)
}


get_qi = function(data){
  data %>%
    summarize(
      estimate0 = median(x0, na.rm = TRUE),
      estimate1 = median(x1, na.rm = TRUE),
      estimate = median(diff, na.rm = TRUE),
      lower = quantile(diff, probs = 0.025, na.rm = TRUE),
      upper = quantile(diff, probs = 0.975, na.rm = TRUE),
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
      prob2 = prob1*2
    ) %>%
    ungroup()
}


## packages #####################

library(dplyr)
library(readr)
library(tidyr)
library(broom)
library(purrr)
library(nlme)
# Create a simulations directory.
dir.create("simulations")

### helper data #######################

# Let's generate, bit by bit, our helper data.

# First, let's make our metadata table
get_metadata() %>% write_csv("simulations/metadata.csv")

# We're going to simulate 20 kinds of effects, using 8 models.
# The unique ID for each effect is in groupid.
# The unique ID for each model is in modelid.
read_csv("simulations/metadata.csv")


# Next, let's get some rescaling data.
# If we ever need to rescale a variable,
# we will whether that model's variable is scaled,
# and what its mean and sd are.
read_rds("models/models.rds") %>%
  map_dfr(~get_rescalers(model = .x), .id = "modelid") %>%
  write_csv("simulations/rescalers.csv")

# Next, get the residual standard error for each model.
get_sigma("models/models.rds") %>%
  write_csv("simulations/sigma.csv")

# Get the overall beta coefficients for these models.
# This involves calculating for each group (city name)
# the group-specific effects and the non-group specific effects
read_rds("models/models.rds") %>%
  map_dfr(~get_effects(model = .x), .id = "modelid") %>%
  write_csv("simulations/effects.csv")

# Get scenario data for every simulation group you're going to run.
# Produces a BIG FILE
read_csv("simulations/metadata.csv") %>%
  split(.$groupid) %>%
  map_dfr(~get_scenarios(case = .x, path_models = "models/models.rds"), .id = "groupid") %>%
  mutate(groupid = as.numeric(groupid)) %>%
  write_rds("simulations/scenarios.rds", compress = "gz")

# About 428,000 rows
read_rds("simulations/scenarios.rds") %>% nrow()

# Get equations of scenario data and effects for each model for each simulation effects group
get_equations(data = read_rds("simulations/scenarios.rds"),
              path_metadata = "simulations/metadata.csv",
              path_rescalers = "simulations/rescalers.csv",
              path_effects = "simulations/effects.csv") %>%
  write_rds("simulations/equations.rds", compress = "gz")

# Produce 5,393,304 rows
read_rds("simulations/equations.rds") %>% nrow()

# Distill into a sampling frame sets
# How many sets of simulations will you end up with?
read_rds("simulations/equations.rds")  %>%
  select(groupid, modelid, scenario, name, geoid) %>%
  distinct() %>%
  mutate(setid = 1:n()) %>%
  write_rds("simulations/sets.rds", compress = "gz")

# Okay, 428,040 sets
read_rds("simulations/sets.rds") %>% nrow()

## expected values
#
# metadata = read_csv("simulations/metadata.csv")
# for(i in 1:length(metadata$groupid) ){
#
#   # Get that model's variance covariance matrix
#   vcov = read_rds("models/models.rds")[[metadata$model[i]]] %>% vcov()
#
#   sigma = read_csv("simulations/sigma.csv") %>%
#     rename(model = modelid) %>%
#     left_join(by = "model",
#               y= read_csv("simulations/metadata.csv") %>%
#                 select(model, modelid) %>% distinct()) %>%
#     select(modelid, sigma) %>%
#     # Get the sigma for that model
#     filter(modelid == metadata$groupid[i]) %>% with(sigma)
#
#   system.time({
#     read_rds("simulations/equations.rds") %>%
#       filter(groupid == metadata$groupid[i]) %>%
#       # For each set, get the predicted value
#       group_by(groupid, modelid, scenario, name, geoid) %>%
#       reframe({
#         # Generate a matrix of multivariate normal simulations
#         mat = MASS::mvrnorm(n = 1000, mu = beta, Sigma = vcov) %>% t()
#         # Multiply each column of simulations by the column of xhat values
#         # equivalent to beta_hat * xhat
#         # Take the column sums to get the vector of y-predicted values.
#         yhat = colSums(mat * xhat)
#         # For as many simulations as got made,
#         mysim = map_dbl(
#           .x = yhat,
#           # Randomly sample 1000 values, and get the mean
#           .f = ~rnorm(n = 1, mean = .x, sd = sigma))
#         # Return the mysim
#         tibble(pv = mysim)
#       }) %>%
#       # Back-transform
#       ungroup() %>%
#       mutate(pv = exp(pv) / (1 + exp(pv)) ) %>%
#       # Save to its own file
#       saveRDS(paste0("simulations/ev_", i, ".rds"))
#   })
# }

# This is too slow. Can't do it.


## predictions ###################################################

# Let's calculate some simple predictions effects

sigmas = read_csv("simulations/sigma.csv") %>%
  rename(model = modelid) %>%
  left_join(by = "model",
            y= read_csv("simulations/metadata.csv") %>%
              select(model, modelid) %>% distinct()) %>%
  select(modelid, sigma)

read_rds("simulations/equations.rds") %>%
  # For each set, get the predicted value
  group_by(groupid, modelid, scenario, name, geoid) %>%
  summarize(yhat = sum(xhat * beta, na.rm = TRUE)) %>%
  ungroup() %>%
  # Join in the sigmas
  left_join(by = "modelid", y = sigmas) %>%
  write_rds("simulations/yhat.rds", compress = "gz")

## marginal effects ############################################


metadata = read_csv("simulations/metadata.csv")
sigmas = read_csv("simulations/sigma.csv")
# For each of 20 groups, I'm going to run some simulations...
#for(i in metadata$groupid){
for(i in 1:20){

  # Get that models residual standard error.
  sigma = sigmas %>%
    filter(modelid == metadata$model[i]) %>% with(sigma)

  # For each geoid, we're going to...
  time = system.time({
    predictions = read_rds("simulations/yhat.rds") %>%
      filter(groupid == metadata$groupid[i])  %>%
      arrange(groupid, modelid, scenario, name, geoid) %>%
      select(-sigma) %>%
      group_by(groupid, modelid, scenario) %>%
      reframe({
        # Take each value in yhat and get 1000 copies of it.
        tibble(name, geoid, yhat) %>%
          split(.$geoid) %>%
          map_dfr(.f = ~rnorm(n = 1000, mean = .x$yhat, sd = sigma) %>%
                    tibble(ysim = ., name = .x$name), .id = "geoid")
      }) %>%
      # Back-transform
      ungroup() %>%
      mutate(ysim = exp(ysim) / (1 + exp(ysim)) ) %>%
      # Save to its own file
      saveRDS(paste0("simulations/sims_", i, ".rds"))
  })
  # Clear cache
  gc();
  print(paste0("done: ", i))
}

# Cleanup
rm(list = ls()); gc()


## predicted change ############################

# Let's apply this simulation strategy to see,
# as we increase the rate of social infrastructure overall by 1, 2, 3, 4, 5,
# what do we expect will happen?
# library(dplyr)
# library(readr)
# library(tidyr)
# library(purrr)
#
# # First, let's make our metadata table
# read_csv("simulations/metadata.csv") %>%
#   # Let's do this for...
#   filter(groupid %in% c(1,2,3,4))
#
# # Get scenario data for every simulation group you're going to run.
# # Produces a BIG FILE
# read_csv("simulations/metadata.csv") %>%
#   # Let's do this for...
#   filter(groupid %in% c(1,2,3,4)) %>%
#   tidyr::expand_grid(x = c(0,1,2,3,4)) %>%
#   mutate(scenario = 1:n()) %>%
#   split(.$scenario) %>%
#   purrr::map_dfr(~get_many_scenarios(case = .x, x = .x$x, path_models = "models/models.rds"),
#                  .id = "groupid") %>%
#   mutate(groupid = as.numeric(groupid)) %>%
#   write_rds("simulations/scenarios_ribbon.rds", compress = "gz")
#
#
# # Get equations of scenario data and effects for each model for each simulation effects group
# get_equations(data = read_rds("simulations/scenarios_ribbon.rds"),
#               path_metadata = "simulations/metadata.csv",
#               path_rescalers = "simulations/rescalers.csv",
#               path_effects = "simulations/effects.csv") %>%
#   write_rds("simulations/equations_ribbon.rds", compress = "gz")
#
#
# sigmas = read_csv("simulations/sigma.csv") %>%
#   rename(model = modelid) %>%
#   left_join(by = "model",
#             y= read_csv("simulations/metadata.csv") %>%
#               select(model, modelid) %>% distinct()) %>%
#   select(modelid, sigma)
#
# read_rds("simulations/equations_ribbon.rds") %>%
#   # For each set, get the predicted value
#   group_by(groupid, modelid, scenario, name, geoid) %>%
#   summarize(yhat = sum(xhat * beta, na.rm = TRUE)) %>%
#   ungroup() %>%
#   # Join in the sigmas
#   left_join(by = "modelid", y = sigmas) %>%
#   write_rds("simulations/yhat_ribbon.rds", compress = "gz")
#
#
# sigmas = read_csv("simulations/sigma.csv")
#
# scenarios = read_rds("simulations/yhat_ribbon.rds") %>%
#   select(groupid) %>%
#   distinct()
#
# # For each of 20 groups, I'm going to run some simulations...
# #for(i in metadata$groupid){
# for(i in scenarios$groupid){
#
#   # For each geoid, we're going to...
#   time = system.time({
#     predictions = read_rds("simulations/yhat_ribbon.rds") %>%
#       filter(groupid == i)  %>%
#       arrange(groupid, modelid, scenario, name, geoid) %>%
#       group_by(groupid, modelid, scenario) %>%
#       reframe({
#         # Take each value in yhat and get 1000 copies of it.
#         tibble(name, geoid, yhat) %>%
#           split(.$geoid) %>%
#           map_dfr(.f = ~rnorm(n = 1000, mean = .x$yhat, sd = sigma[1]) %>%
#                     tibble(ysim = ., name = .x$name), .id = "geoid")
#       }) %>%
#       # Back-transform
#       ungroup() %>%
#       mutate(ysim = exp(ysim) / (1 + exp(ysim)) ) %>%
#       # Save to its own file
#       saveRDS(paste0("simulations/sims_ribbon_", i, ".rds"))
#   })
#   # Clear cache
#   gc();
#   print(paste0("done: ", i))
# }
#
# # Cleanup
# rm(list = ls()); gc()

## N #################################

read_rds("models/models.rds")[[1]]$data %>% nrow()

# QUANTITIES OF INTEREST ##################################

# Now that we have our simulations,
# we're going to get quantities of interest at several levels.

# Takes about 25 seconds for one group
#time

## OVERALL ###############################################

# Give me the AVERAGE effect for census tracts in ANY city

holder = tibble()
# Get simulated differences per geoid
for(i in paste0("simulations/sims_", 1:20, ".rds")){

  marginal = i %>% read_rds() %>%
    group_by(groupid, name, geoid) %>%
    reframe(
      x0 = ysim[scenario == 1],
      x1 = ysim[scenario == 2]
    ) %>%
    group_by(groupid, name, geoid) %>%
    mutate(id = 1:n()) %>%
    ungroup() %>%
    # Get the AVERAGE effect for census tracts in ANY city
    group_by(groupid, id) %>%
    summarize(
      # Average initial outcome across census tracts
      x0 = mean(x0, na.rm = TRUE),
      # Average treated outcome across census tracts
      x1 = mean(x1, na.rm = TRUE),
      # Difference in average treated outcome across census tracts
      diff = x1 - x0) %>%
    # Finally, for each group, return to me our basic quantities of interest
    group_by(groupid) %>%
    get_qi()

  holder = bind_rows(holder, marginal)
  remove(marginal)
}
# Save to file
holder %>%
  write_csv("simulations/marginal_overall.csv")
rm(list = ls())

## PER CITY #############################################


# Get simulated differences per geoid
time = system.time({

  holder = tibble()

  # Get simulated differences per geoid
  for(i in paste0("simulations/sims_", 1:20, ".rds")){

    marginal = i %>%
      read_rds() %>%
      group_by(groupid, name, geoid) %>%
      reframe(
        x0 = ysim[scenario == 1],
        x1 = ysim[scenario == 2]
      ) %>%
      group_by(groupid, name, geoid) %>%
      mutate(id = 1:n()) %>%
      ungroup() %>%
      # FOR EACH CITY,
      # Get the AVERAGE effect for census tracts in EACH city
      group_by(groupid, name, id) %>%
      summarize(
        # Average initial outcome across census tracts
        x0 = mean(x0, na.rm = TRUE),
        # Average treated outcome across census tracts
        x1 = mean(x1, na.rm = TRUE),
        # Difference in average treated outcome across census tracts
        diff = x1 - x0) %>%
      # Finally, for each group AND CITY, return to me our basic quantities of interest
      group_by(groupid, name) %>%
      get_qi()

    holder = bind_rows(holder, marginal)
    remove(marginal)
  }
})

holder %>%
  # Save to file
  write_csv("simulations/marginal_per_city.csv")

## OVERALL RIBBONS #################################
holder = tibble()
for(i in paste0("simulations/sims_ribbon_", 1:20, ".rds")){

  values = i %>%
    read_rds() %>%
    group_by(groupid, name, geoid, scenario) %>%
    mutate(id = 1:n()) %>%
    ungroup() %>%
    # Get the average across all those geoids
    group_by(groupid, scenario, id) %>%
    summarize(ysim = mean(ysim, na.rm = TRUE)) %>%
    group_by(groupid, scenario) %>%
    summarize(estimate = median(ysim, na.rm = TRUE),
              lower = quantile(ysim, probs = 0.025),
              upper = quantile(ysim, probs = 0.975))

  holder = bind_rows(holder, values)
  remove(values)
  print(i)
}
holder %>%
  # Save to file
  write_csv("simulations/ribbon.csv")

# VISUALIZE #############################################


## EFFECTS OVERALL #######################################

### TABLE ###################################
effect = read_csv("simulations/marginal_overall.csv") %>%
  left_join(by = "groupid", y = read_csv("simulations/metadata.csv")) %>%
  mutate(treatment = treatment %>% dplyr::recode_factor(
    "park" = "Parks",
    "social_business" = "Social Businesses",
    "place_of_worship" = "Places of Worship",
    "community_space" = "Community Spaces",
    "total" = "Total",
  ),
  outcome = outcome %>% dplyr::recode_factor(
    "social_capital" = "Overall Social Capital",
    "bonding" = "Bonding Social Capital",
    "bridging" = "Bridging Social Capital",
    "linking" = "Linking Social Capital",
  )) %>%
  mutate(stars = gtools::stars.pval(prob2)) %>%
  mutate(estimate = scales::number(estimate, accuracy = 0.001),
         lower = scales::number(lower, accuracy = 0.001),
         upper = scales::number(upper, accuracy = 0.001),
         prob2 = scales::number(prob2, accuracy = 0.001)
  ) %>%
  select(outcome, treatment, estimate, lower, upper, p_value = prob2, stars, modelid) %>%
  write_csv("viz/table_marginal_effects.csv")


### VISUAL ##################################

# Visualize those simulations
library(dplyr)
library(readr)
library(ggplot2)
library(viridis)

# Check sample size
# read_rds("models/models.rds")[[1]]$data %>% nrow()

# Load in overall effects, and get the metadata joined in
marginal = read_csv("simulations/marginal_overall.csv") %>%
  left_join(by = "groupid", y = read_csv("simulations/metadata.csv")) %>%
  mutate(treatment = treatment %>% dplyr::recode_factor(
    "park" = "Parks",
    "social_business" = "Social Businesses",
    "place_of_worship" = "Places of Worship",
    "community_space" = "Community Spaces",
    "total" = "Total",
  ),
  outcome = outcome %>% dplyr::recode_factor(
    "social_capital" = "Overall Social Capital",
    "bonding" = "Bonding Social Capital",
    "bridging" = "Bridging Social Capital",
    "linking" = "Linking Social Capital",
  )) %>%
  mutate(sig = case_when(
    prob2 < 0.05 ~ "significant\n(p < 0.05)",
    TRUE ~ "insignificant\n")) %>%
  mutate(stars = gtools::stars.pval(prob2)) %>%
  mutate(label = scales::number(estimate, style_positive = "plus", style_negative = "minus", accuracy = 0.001),
         label = paste0(label, stars))

gg = ggplot() +
  geom_linerange(data = marginal, mapping = aes(x = treatment, ymin = lower, ymax= upper, alpha = sig), linewidth = 3) +
  geom_point(data = marginal, mapping = aes(x = treatment, y = estimate, alpha = sig),
             shape = 21, fill = "white", color = "black", size = 5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  shadowtext::geom_shadowtext(
    data = marginal, mapping = aes(x = treatment, y = estimate, label = label, color = sig),
    bg.r = 0.2, bg.color = "white",  nudge_x = +0.3
  ) +
  facet_wrap(~outcome, nrow = 2) +
  coord_flip() +
  labs(x = "Type of Social Infrastructure",
       y = "Predicted Change in Social Capital Index (0-1)\n[95% Confidence Intervals]",
       alpha = "Statistical Significance",
       title = paste0("Mean Predicted Change in Social Capital"),
       subtitle = paste0("from +1 Extra Social Infrastructure Site per person per sq.km.",
                         "\n",
                         "in 10,701 census tracts in 25 most populous US cities")) +
  scale_color_manual(values = c("grey", "#373737")) +
  theme_bw(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, color = "#373737"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white", face = "bold"),
        legend.position = "bottom") +
  guides(color = "none")

ggsave(gg, filename = "viz/marginal_overall.png", dpi = 500, width = 8, height = 8)
browseURL("viz/marginal_overall.png")

# marginal %>%
#   glimpse()
# citation("nlme")

## EFFECTS COMPARATIVELY ####################################

marginal = read_csv("simulations/marginal_overall.csv") %>%
  left_join(by = "groupid", y = read_csv("simulations/metadata.csv")) %>%
  filter(outcome %in% c("bonding", "bridging")) %>%
# Recode variable names
  mutate(treatment = treatment %>% dplyr::recode_factor(
    "total" = "SI Overall",
    "community_space" = "Community\nSpaces",
    "place_of_worship" = "Places\nof Worship",
    "social_business" = "Social\nBusinesses",
    "park" = "Parks",
  ),
  outcome = outcome %>% dplyr::recode_factor(
    "social_capital" = "Overall",
    "bonding" = "Bonding",
    "bridging" = "Bridging",
    "linking" = "Linking",
  )) %>%
  mutate(sig = case_when(
    prob2 < 0.05 ~ "significant\n(p < 0.05)",
    TRUE ~ "insignificant\n")) %>%
  mutate(stars = gtools::stars.pval(prob2)) %>%
  mutate(label = scales::number(estimate, style_positive = "plus", style_negative = "minus", accuracy = 0.001),
         label = paste0(label, stars))
marginal
ggplot() +
  geom_col(data = marginal, mapping = aes(x = treatment, y = estimate, group = groupid, fill = outcome),
           position = position_dodge())

## EFFECTS BY CITY ###################################

### SUMMARY #######################################


# Load in overall effects, and get the metadata joined in
marginal = read_csv("simulations/marginal_per_city.csv") %>%
  left_join(by = "groupid", y = read_csv("simulations/metadata.csv")) %>%
  left_join(by = c("name"),
            y = read_csv("viz/cities.csv") %>%
              select(name, name_label, pop, social_capital_mean, bridging_mean) ) %>%
  # Recode variable names
  mutate(treatment = treatment %>% dplyr::recode_factor(
    "total" = "SI Overall",
    "community_space" = "Community\nSpaces",
    "place_of_worship" = "Places\nof Worship",
    "social_business" = "Social\nBusinesses",
    "park" = "Parks",
  ),
  outcome = outcome %>% dplyr::recode_factor(
    "social_capital" = "Overall",
    "bonding" = "Bonding",
    "bridging" = "Bridging",
    "linking" = "Linking",
  )) %>%
  mutate(sig = case_when(
    prob2 < 0.05 ~ "significant\n(p < 0.05)",
    TRUE ~ "insignificant\n")) %>%
  mutate(stars = gtools::stars.pval(prob2)) %>%
  mutate(label = scales::number(estimate, style_positive = "plus", style_negative = "minus", accuracy = 0.001),
         label = paste0(label, stars))

# What was the range of significant effects?
marginal %>%
  filter(outcome == "Bridging") %>%
  filter(estimate > 0 & prob2 < 0.05) %>%
  summarize(min = min(estimate, na.rm = TRUE),
            min_name = name[estimate == min(estimate,na.rm = TRUE)],
            min_label = label[estimate == min(estimate)],
            max = max(estimate, na.rm = TRUE),
            max_name = name[estimate == max(estimate)],
            max_label = label[estimate == max(estimate)])


# What was the range of significant effects?
marginal %>%
  filter(outcome == "Bridging") %>%
  filter(estimate > 0 & prob2 < 0.05) %>%
  filter(name == "houston")

marginal %>%
  filter(outcome == "Bridging") %>%
  filter(estimate > 0 & prob2 < 0.05) %>%
  filter(treatment == "Community\nSpaces") %>%
  arrange(desc(estimate)) %>%
  select(name_label, label)


marginal %>%
  filter(outcome == "Bridging") %>%
  filter(treatment == "Community\nSpaces") %>%
  filter(estimate > 0 & prob2 < 0.05) %>%
  select(name_label, estimate, label, social_capital_mean, bridging_mean) %>%
  mutate(percent = estimate / bridging_mean) %>%
  arrange(desc(percent))

### VISUAL ##################################################

#### MAIN VISUAL #######################################

# Visualize those simulations
library(dplyr)
library(readr)
library(ggplot2)
library(viridis)

# Load in overall effects, and get the metadata joined in
marginal = read_csv("simulations/marginal_per_city.csv") %>%
  left_join(by = "groupid", y = read_csv("simulations/metadata.csv")) %>%
  left_join(by = c("name"),
            y = read_csv("viz/cities.csv") %>%
              select(name, name_label, pop) ) %>%
  # Recode variable names
  mutate(treatment = treatment %>% dplyr::recode_factor(
    "total" = "SI Overall",
    "community_space" = "Community\nSpaces",
    "place_of_worship" = "Places\nof Worship",
    "social_business" = "Social\nBusinesses",
    "park" = "Parks",
  ),
  outcome = outcome %>% dplyr::recode_factor(
    "social_capital" = "Overall",
    "bonding" = "Bonding",
    "bridging" = "Bridging",
    "linking" = "Linking",
  )) %>%
  mutate(sig = case_when(
    prob2 < 0.05 ~ "significant\n(p < 0.05)",
    TRUE ~ "insignificant\n")) %>%
  mutate(stars = gtools::stars.pval(prob2)) %>%
  mutate(label = scales::number(estimate, style_positive = "plus", style_negative = "minus", accuracy = 0.001),
         label = paste0(label, stars))


get_viz = function(marginal, .outcome = "Bridging", .title = "stufff"){

  text = marginal %>%
    filter(outcome == .outcome) %>%
    filter(prob2 <= 0.05) %>%
    select(name_label, pop, label, estimate, treatment, outcome) %>%
    mutate(side = case_when(estimate > 0 ~ TRUE, estimate < 0 ~ FALSE))

  ggplot() +
    geom_hline(yintercept = 0, linetype = "solid", color = "#373737") +
    geom_col(
      data = marginal %>%
        filter(outcome == .outcome) %>%
        filter(prob2 >= 0.05),
      mapping = aes(x = reorder(name_label, pop), y = estimate,
                    fill = "Insignificant"),
      fill = "grey",
      alpha = 0.25) +
    geom_linerange(
      marginal %>%
        filter(outcome == .outcome) %>%
        filter(prob2 >= 0.05),
      mapping = aes(x = reorder(name_label, pop), ymin = lower, ymax = upper,
                    color = "Insignificant"),
      color = "darkgrey",
      alpha = 0.25
    ) +

    # Positive
    geom_col(
      data = marginal %>%
        filter(outcome == .outcome) %>%
        filter(prob2 < 0.05, estimate > 0),
      mapping = aes(x = reorder(name_label, pop), y = estimate,
                    fill = "Significant"),
      alpha = 1, fill = "#9AB4F9") +
    geom_linerange(
      marginal %>%
        filter(outcome == .outcome) %>%
        filter(prob2 < 0.05, estimate > 0),
      mapping = aes(x = reorder(name_label, pop), ymin = lower, ymax = upper,
                    color = "Significant"),
      color = "#648FFF"
    ) +

    # Negative
    geom_col(
      data = marginal %>%
        filter(outcome == .outcome) %>%
        filter(prob2 < 0.05, estimate < 0),
      mapping = aes(x = reorder(name_label, pop), y = estimate,
                    fill = "Significant"),
      fill = "#F99558",
      alpha = 1) +
    geom_linerange(
      marginal %>%
        filter(outcome == .outcome) %>%
        filter(prob2 < 0.05, estimate < 0),
      mapping = aes(x = reorder(name_label, pop), ymin = lower, ymax = upper,
                    color = "Significant"),
      color =  "#FE6100"
    ) +
    facet_wrap(~treatment, nrow = 1) +
    theme_bw(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = "bottom",
          strip.background = element_rect(fill = "black"),
          strip.text = element_text(color = "white", face= "bold")) +
    coord_flip() +
    # scale_fill_manual(
    #   breaks = c("Significant", "Insignificant"),
    #   values = c( "grey")) +
    # scale_color_manual(
    #   breaks = c("Significant", "Insignificant"),
    #   values = c( 'darkgrey')) +
    labs(y = "Average Predicted Change in Social Capital (with 95% Confidence Intervals)") +
    shadowtext::geom_shadowtext(
      data = text %>% filter(side == TRUE),
      mapping = aes(x = reorder(name_label, pop),
                    y = 0, label = label),
      bg.color = "white", bg.r = 0.05, color = "#648FFF",
      hjust = 1,
    ) +
    shadowtext::geom_shadowtext(
      data = text %>% filter(side == FALSE),
      mapping = aes(x = reorder(name_label, pop),
                    y = 0, label = label),
      bg.color = "white", bg.r = 0.05, color = "#FE6100",
      hjust = 0,
    ) +
    labs(fill = "Statistical Significance (p < 0.05)",
         x = "City, Ordered by Population") +
    guides(color = "none") +
    labs(title = .title,
         subtitle ="by Type of Social Infrastructure",
         caption = "Bars reflect predicted change in social capital index\nwhen rate of social infrastructure of given type increases by 1 site.\nBands show confidence intervals, derived from 1000 monte carlo simulations per census tract.\nEstimates show statistically significant effects (p < 0.05).") +
    theme(plot.caption = element_text(hjust = 0))

}

g1 = marginal %>% get_viz(.outcome = "Overall", .title = "Changes per City in Overall Social Capital")
g2 = marginal %>% get_viz(.outcome = "Bonding", .title = "Changes per City in Bonding Social Capital")
g3 = marginal %>% get_viz(.outcome = "Bridging", .title = "Changes per City in Bridging Social Capital")
g4 = marginal %>% get_viz(.outcome = "Linking", .title = "Changes per City in Linking Social Capital")

ggsave(g1, filename = "viz/city_marginal_overall.png", dpi = 500, width = 10, height = 7)
ggsave(g2, filename = "viz/city_marginal_bonding.png", dpi = 500, width = 10, height = 7)
ggsave(g3, filename = "viz/city_marginal_bridging.png", dpi = 500, width = 10, height = 7)
ggsave(g4, filename = "viz/city_marginal_linking.png", dpi = 500, width = 10, height = 7)
browseURL("viz/city_marginal_overall.png")
browseURL("viz/city_marginal_bonding.png")
browseURL("viz/city_marginal_bridging.png")

#### OTHER VISUALS #########################

# Show me effect of total on each type
subset = marginal %>%  filter(treatment == "Total", outcome == "Overall Social Capital")

# In what cities does adding more social infrastructure have the largest impact?
ggplot() +
  geom_col(
    data = marginal %>%
      filter(treatment == "Total", outcome == "Overall Social Capital"),
    mapping = aes(x = reorder(name, estimate), y = estimate), alpha = 0.5, fill = "blue") +
  geom_col(
    data = marginal %>%
      filter(
        treatment == "Community Spaces",
        outcome == "Overall Social Capital"),
    mapping = aes(x = reorder(name, estimate), y = estimate), alpha = 0.5, fill = "red") +

  geom_col(
    data = marginal %>%
      filter(
        treatment == "Parks",
        outcome == "Overall Social Capital"),
    mapping = aes(x = reorder(name, estimate), y = estimate), alpha = 0.5,
    fill = "green") +
  coord_flip()


# These random effects are not work reporting.


ggplot() +
  geom_col(
    data = marginal %>%
      filter(outcome == "Overall Social Capital"),
    mapping = aes(x = reorder(name, estimate), y = estimate),
    alpha = 0.5, fill = "blue") +
  geom_linerange(
    marginal %>%
      filter(outcome == "Overall Social Capital"),
    mapping = aes(x = reorder(name, estimate), ymin = lower, ymax = upper)
  ) +
  facet_wrap(~treatment) +
  coord_flip()

# These random effects are not work reporting.

# Nah.
ggplot() +
  geom_linerange(
    data = marginal %>%
      filter(outcome == "Overall Social Capital",
             treatment == "Total"),
    mapping = aes(x = reorder(name, estimate),
                  ymin = estimate0, ymax = estimate1)
  ) +
  coord_flip()


ggplot() +
  geom_tile(
    data = marginal,
    mapping = aes(x = name, y = outcome, fill = estimate, color = sig)
  ) +
  facet_wrap(~treatment) +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "white", midpoint = 0, high = "blue") +
  scale_color_manual(values = c("white", "black"))


ggplot() +
  geom_tile(
    data = marginal %>% filter(outcome == "Bridging Social Capital"),
    mapping = aes(x = name, y = treatment, fill = estimate, color = sig)
  )  +
  shadowtext::geom_shadowtext(
    data = marginal %>% filter(outcome == "Bridging Social Capital"),
    mapping = aes(x = name, y = treatment, fill = estimate, color = sig, label = label)
  ) +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "white", midpoint = 0, high = "blue") +
  scale_color_manual(values = c("white", "black")) +
  labs(title = "Expected Effect on Bridging Social Capital\nof Adding 1 Social Infrastructure Site per person per sq.km.")

# Show the story of bridging social capital for community spaces and total
ggplot() +
  geom_col(data = marginal %>%
             filter(outcome == "Bridging Social Capital",
                    treatment == "Total"),
           mapping = aes(x = reorder(name, estimate), y = estimate,
                         fill = "Total", alpha = sig)) +
  geom_col(data = marginal %>%
             filter(outcome == "Bridging Social Capital",
                    treatment == "Community Spaces"),
           mapping = aes(x = reorder(name, estimate), y = estimate,
                         fill = "Community Spaces", alpha = sig)) +
  scale_alpha_discrete(range = c(0.25, 0.9)) +
  coord_flip() +
  theme(legend.position = "bottom")



### TABLE ##############################################




#read_csv("simulations/marginal_per_city.csv")$name %>% unique()

effect = read_csv("simulations/marginal_per_city.csv") %>%
  # Join in model metadata
  left_join(by = "groupid", y = read_csv("simulations/metadata.csv")) %>%
  # Join in City names and total population for ordering
  left_join(by = c("name"),
            y = read_csv("viz/cities.csv") %>%
              select(name, name_label, pop) ) %>%
  # Recode variable names
  mutate(treatment = treatment %>% dplyr::recode_factor(
    "total" = "SI Overall",
    "community_space" = "Community Spaces",
    "place_of_worship" = "Places of Worship",
    "social_business" = "Social Businesses",
    "park" = "Parks",
  ),
  outcome = outcome %>% dplyr::recode_factor(
    "social_capital" = "Overall",
    "bonding" = "Bonding",
    "bridging" = "Bridging",
    "linking" = "Linking",
  )) %>%
  mutate(stars = gtools::stars.pval(prob2)) %>%
  mutate(
    estimate0 = scales::number(estimate0, accuracy = 0.001),
    estimate1 = scales::number(estimate1, accuracy = 0.001),
    estimate = scales::number(estimate, accuracy = 0.001, style_positive = "plus"),
    lower = scales::number(lower, accuracy = 0.001),
    upper = scales::number(upper, accuracy = 0.001),
    prob2 = scales::number(prob2, accuracy = 0.001),
    label = paste0(estimate, stars),
    ci = paste0("[", lower, " to ", upper, "]")
  ) %>%
  select(outcome, treatment, name_label, estimate0, estimate1, label, ci, model, pop) %>%
  mutate(model = model %>% recode_factor(
    "total_social_capital" = 1,
    "subtypes_social_capital" = 2,
    "total_bonding" = 3,
    "subtypes_bonding" = 4,
    "total_bridging" = 5,
    "subtypes_bridging" = 6,
    "total_linking" = 7,
    "subtypes_linking" = 8
  )) %>%
  arrange(treatment, outcome, desc(pop)) %>%
  select(-pop) %>%
  select(`Type of Social Capital (SC)` = outcome,
         `Type of Social Infrastructure (SI)` = treatment,
         `City` = name_label,
         `Baseline SC` = estimate0,
         `Baseline + Effect` = estimate1,
         `Effect` = label,
         `95% CI` = ci,
         Model = model) %>%
  write_csv("viz/table_marginal_effects_per_city.csv")


# Let's get the model order right.



