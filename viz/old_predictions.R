#' @name 09_figure_6.R
#' @author Tim & Osama

# 0. Prediction Setup ############################

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




# 1. Prediction Grid #############################


# Now, we can use avgblock as a building block *pun intended*
# for making a bunch of predictor values we can feed our model

# Now, we can use avgblock as a building block *pun intended*
# for making a bunch of predictor values we can feed our model

# Take average block
output_bridging = tibble(
  # Make a column you want to vary...
  total = c(0,1,2,3),
  # Then add a 1-row data.frame with with no overlapping names to fill in for as many cells as in `total`
  avg_block
) %>%
  # Then pipe in the model of choice
  generate(model = m$total$bridging) %>%
  mutate(label = round(yhat, 3))

# Let's check the output, by tilting it on its head for easier viewing
output_bridging %>% glimpse()

#Lets make some predictions for bonding social capital
output_bonding = tibble(
  # Make a column you want to vary...
  total = c(0,1,2,3),
  # Then add a 1-row data.frame with with no overlapping names to fill in for as many cells as in `total`
  avg_block
) %>%
  # Predictions for linking
  generate(model = m$total$bonding) %>%
  mutate(label = round(yhat, 3))


#Lets make some predictions for linking social capital
output_linking = tibble(
  # Make a column you want to vary...
  total = c(0,1,2,3),
  # Then add a 1-row data.frame with with no overlapping names to fill in for as many cells as in `total`
  avg_block
) %>%
  # Predictions for linking
  generate(model = m$total$linking) %>%
  mutate(label = round(yhat, 3))


#Lets make some predictions for total social capital
output_total = tibble(
  # Make a column you want to vary...
  total = c(0,1,2,3),
  # Then add a 1-row data.frame with with no overlapping names to fill in for as many cells as in `total`
  avg_block
) %>%
  # Predictions for linking
  generate(model = m$total$social_capital) %>%
  mutate(label = round(yhat, 3))

# 2. Visualization ########################################

gg = ggplot() +
  geom_ribbon(data = output_bridging,
              mapping = aes(x = total, y = yhat, ymin = lower, ymax = upper, fill = "Bridging", color = "Bridging"),
              alpha = 0.75, linewidth = 1, color = "#373737") +
  geom_ribbon(data = output_bonding,
              mapping = aes(x = total, y = yhat, ymin = lower, ymax = upper, fill = "Bonding", color = "Bonding"),
              alpha = 0.5) +
  geom_ribbon(data = output_linking,
              mapping = aes(x = total, y = yhat, ymin = lower, ymax = upper, fill = "Linking", color = 'Linking'), alpha = 0.5) +
  geom_ribbon(data = output_total,
              mapping = aes(x = total, y = yhat, ymin = lower, ymax = upper, fill = "Overall", color = 'Overall'), alpha = 0.5) +
  shadowtext::geom_shadowtext(
    data = output_bridging,
    mapping = aes(x = total, y = yhat, label = label),
    color = "#373737", bg.color = "white", bg.r = 0.2) +
  shadowtext::geom_shadowtext(
    data = output_bonding,
    mapping = aes(x = total, y = yhat, label = label),
    color = "darkgrey", bg.color = "white", bg.r = 0.2) +
  shadowtext::geom_shadowtext(
    data = output_linking,
    mapping = aes(x = total, y = yhat, label = label),
    color = "darkgrey", bg.color = "white", bg.r = 0.2) +
  shadowtext::geom_shadowtext(
    data = output_total,
    mapping = aes(x = total, y = yhat, label = label),
    color = "darkgrey", bg.color = "white", bg.r = 0.2) +

  scale_fill_manual(
    values = c("#785EF0", "#DC267F", "#FE6100", "#FFB000"),
    breaks = c("Overall", "Bonding", "Bridging", "Linking"),
    name = "Type of Social Capital") +
  scale_color_manual(
    values = c("#785EF0", "#DC267F", "#FE6100", "#FFB000"),
    breaks = c("Overall", "Bonding", "Bridging", "Linking"),
    guide = "none") +

  labs(title = 'Projected Change in Social Capital \n given Investment in Social Infrastructure',
       x = 'Rate of Total Social Infrastructure (per 1000 residents per 1 sq.km.)',
       y = 'Predicted Social Capital Index (0 - 1)\nwith 95% Confidence Interval',
       fill = 'Type') +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_rect(fill = NA, color = "#373737"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "bottom")

ggsave(gg, filename = "viz/figure_6_predictions.png", dpi = 500, width = 6.5, height= 6)
