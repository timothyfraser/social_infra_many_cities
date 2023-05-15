#' @name 11_osama.R
#' @description A script for visualizations!
#'
#'


###############################################
# 0. Prediction Setup
###############################################

# Read this! Follow along!
# https://timothyfraser.com/simulate/vignette_intro
library(dplyr)
library(readr)
library(broom)
library(tidyr)
library(ggplot2)

# Load models
m = read_rds("viz/models_main.rds")

m %>% str()
m %>% names()

m$total %>% names()
m$subtypes %>% names()
# View a model object
m$total$bridging

# Traits of an average city block in sample
avg_block = tibble(
  # Mean covariate traits
  pop_density_block = matrix(0),
  black_block = matrix(0),
  hisplat_block = matrix(0),
  asian_block = matrix(0),
  median_household_income_bg = matrix(0),
  income_0_60K_bg = matrix(0),
  some_college_bg = matrix(0),
  over_65_bg = matrix(0),
  unemployment_bg = matrix(0),
  # Modal city
  name = "houston"
)

# I've also writte a generate() function, which will get you predictions easily if you just supply it a model and data frame of new x values
generate = function(data, model){
  data %>%
    mutate(
      predict(model, newdata = ., se.fit = TRUE) %>%
        as_tibble() %>%
        select(yhat = fit, se = se.fit) ) %>%
    # Get confidence interval
    mutate(lower = yhat - se * 1.96,
           upper = yhat + se * 1.96)
}




###############################################
# 1. First Example
###############################################


# Now, we can use avgblock as a building block *pun intended*
# for making a bunch of predictor values we can feed our model

# Take average block
output_bridging = tibble(type = "Bridging",
  # Make a column you want to vary...
  total = c(0,1,2,3),
  # Then add a 1-row data.frame with with no overlapping names to fill in for as many cells as in `total`
  avg_block
) %>%
  # Then pipe in the model of choice
  generate(model = m$total$bridging)

# Let's check the output, by tilting it on its head for easier viewing
output_bridging %>% glimpse()

#Lets make some predictions for bonding social capital
output_bonding = tibble(type = 'Bonding',
  # Make a column you want to vary...
  total = c(0,1,2,3),
  # Then add a 1-row data.frame with with no overlapping names to fill in for as many cells as in `total`
  avg_block
) %>%
  # Predictions for linking
  generate(model = m$total$bonding)


#Lets make some predictions for linking social capital
output_linking = tibble(type = "Linking",
  # Make a column you want to vary...
  total = c(0,1,2,3),
  # Then add a 1-row data.frame with with no overlapping names to fill in for as many cells as in `total`
  avg_block
) %>%
  # Predictions for linking
  generate(model = m$total$linking)


#Lets make some predictions for total social capital
output_total = tibble(type = "Total Social Capital",
  # Make a column you want to vary...
  total = c(0,1,2,3),
  # Then add a 1-row data.frame with with no overlapping names to fill in for as many cells as in `total`
  avg_block
) %>%
  # Predictions for linking
  generate(model = m$total$social_capital)

#I'm still not sure what the total (x axis) represents
#I'm also not sure how to change the name of each type of social capital on the legend
ggplot() +
  geom_ribbon(data = output_bridging, mapping = aes(x = total, y = yhat, ymin = lower, ymax = upper, fill = "Bridging"), alpha = 0.5) +
  geom_ribbon(data = output_bonding, mapping = aes(x = total, y = yhat, ymin = lower, ymax = upper, fill = "Bonding"), alpha = 0.5) +
  geom_ribbon(data = output_linking, mapping = aes(x = total, y = yhat, ymin = lower, ymax = upper, fill = "Linking"), alpha = 0.5) +
  geom_ribbon(data = output_total, mapping = aes(x = total, y = yhat, ymin = lower, ymax = upper, fill = "Total"), alpha = 0.5) +
  labs(title = 'Social Infra vs. Social Capital',
       x = 'Total',
       y = 'Social Capital Index',
       fill = 'Type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "top")

#The visualization shows a very strong connection between bridging social capital and
#social infrastructure

###############################################
# 2. Complex Example
###############################################

# Suppose we try one with the subtypes:

tibble(type = "Community Spaces",
       community_space = c(0,1,2,3,4),
       place_of_worship = 0,
       social_business = 0,
       park = 0,
       avg_block) %>%
  generate(model = m$subtypes$bridging)

# Suppose we try a few...
output2 = bind_rows(
  # Get predictions when varying total SI
  tibble(type = "Total",
         total = c(0,1,2,3,4),
         avg_block) %>%
    generate(model = m$total$bridging) %>%
    select(type, x = total, yhat, se, lower, upper),
  # Get predictions when varying community spaces
  tibble(type = "Community Spaces",
         community_space = c(0,1,2,3,4),
         place_of_worship = 0,
         social_business = 0,
         park = 0,
         avg_block) %>%
    generate(model = m$subtypes$bridging)%>%
    select(type, x = community_space, yhat, se, lower, upper),
  # Get predictions when varying parks
  tibble(type = "Parks",
         community_space = 0,
         place_of_worship = 0,
         social_business = 0,
         park = c(0,1,2,3,4),
         avg_block) %>%
    generate(model = m$subtypes$bridging) %>%
    select(type, x = park, yhat, se, lower, upper),
  # Get predictions when varying places of worship
  tibble(type = "Places of Worship",
         community_space = 0,
         place_of_worship = c(0,1,2,3,4),
         social_business = 0,
         park = 0,
         avg_block) %>%
    generate(model = m$subtypes$bridging) %>%
    select(type, x = place_of_worship, yhat, se, lower, upper),
  # Get predictions when varying social businesses
  tibble(type = "Social Businesses",
         community_space = 0,
         place_of_worship = 0,
         social_business = c(0,1,2,3,4),
         park = 0,
         avg_block) %>%
    generate(model = m$subtypes$bridging) %>%
    select(type, x = social_business, yhat, se, lower, upper)
)

# A real snazzy - but still incomplete - visual
ggplot() +
  geom_ribbon(
    data = output2,
    mapping = aes(x = x, y = yhat, ymin = lower, ymax = upper, fill = type),
    # outline color
    color = "black",
    # transparency
    alpha = 0.5
  ) +
  # Change colors!
  scale_fill_manual(
    # Change order of labels
    breaks = c("Total",  "Community Spaces", "Places of Worship", "Parks", "Social Businesses"),
    # Change labels (eg. add linebreak with \n )
    labels = c("Total", "Community\nSpaces", "Places of\nWorship", "Parks", "Social\nBusinesses"),
    # Change colors assigned using hexadecimal codes
    # play around with this website.
    # https://davidmathlogic.com/colorblind/#%23648FFF-%23785EF0-%23DC267F-%23FE6100-%23FFB000
    values = c("#648FFF", "#785EF0", "#DC267F", "#FE6100", "#FFB000")) +
  # Change theme!
  theme_classic(base_size = 14) +
  # Change labels!
  labs(x = "x", y  = "y", fill = "fill", title = "Stuff", caption = "More stuff", subtitle = "So cool!") +
  theme(
    # Get rid of ticks
    axis.ticks = element_blank(),
    # add a border
    panel.border = element_rect(color = "black", fill = NA),
    # Justify the caption (0 = left, 0.5 = center, 1 = right)
    plot.caption = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
    # Change legend position
    legend.position = "bottom"
  )

