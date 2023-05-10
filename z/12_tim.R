#' @name 12_tim.R
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
output = tibble(
  # Make a column you want to vary...
  total = c(0,1,2,3),
  # Then add a 1-row data.frame with with no overlapping names to fill in for as many cells as in `total`
  avg_block
) %>%
  # Then pipe in the model of choice
  generate(model = m$total$bridging)

# Let's check the output, by tilting it on its head for easier viewing
output %>% glimpse()


ggplot() +
  geom_ribbon(data = output, mapping = aes(x = total, y = yhat, ymin = lower, ymax = upper))


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


###############################################
# 3. Bonkers Example
###############################################

output3 = output2 %>%
  # Give each prediction a unique ID
  mutate(id = 1:n()) %>%
  # For each prediction,
  group_by(id, type, x) %>%
  # Let's get 1000 random draws from that
  # a normal distribution centered on the original prediction,
  # with standard deviation matching that standard error!
  summarize(
    ysim = rnorm(1000, mean = yhat, sd = se)
  )


# With jittered points!
output3 %>%
  filter(type == "Total") %>%
  ggplot(mapping = aes(x = x, y = ysim,  color = x)) +
  geom_jitter()

# With violins!
output3 %>%
  filter(type == "Total") %>%
  ggplot(mapping = aes(x = x, y = ysim, group = x, fill = x)) +
  geom_violin()

# Or mix and match!
ggplot() +
  geom_jitter(data = output3 %>% filter(type == "Total"),
              mapping = aes(x = x, y = ysim, color = x)) +
  geom_violin(data = output3 %>% filter(type == "Total"),
              mapping = aes(x =x, y= ysim, group = x, color = x),
              fill = "white", alpha = 0.5, linewidth = 1.5)

# Go wild!



##############################################
# Z. Other Previous Examples of Bar Charts
##############################################
# packages
# .rds
# tidyverse!

# Necessary package!

library(dplyr) # data wrangling
library(ggplot2) # visualization
library(readr) # reading in most data

stat = read_rds("viz/popstat.rds")

# One-sample t-tests for cities
# is the rate of 'community spaces' in Austin, TX significantly different from the mean across all cities?
# estimate = what's the mean in Austin?
# conf.low = what's the confidence interval around that statistic?
stat %>%
  ggplot(mapping = aes(x = name, y = estimate,
                       ymin = conf.low, ymax = conf.high)) +
  geom_col() +
  geom_errorbar() +
  facet_wrap(~type, scales = "free_x", ncol = 5) +
  coord_flip()




stat$type

round(mean(stat$estimate), 2)

# THE PIPELINE!!!!!!
# dplyr::`%>%`()

# THESE ARE ALL THE SAME!
round(mean(stat$estimate), 2)

stat$estimate %>% mean() %>% round(2)

stat %>%
  with(estimate) %>%
  mean() %>%
  round(2)


tally = read_rds("viz/poptally.rds")
tally$rate_park
tally$rate_social_business
tally$rate_place_of_worship
tally$rate_community_space
tally$rate_total
# rate of sites per person per square kilometer

tally$group = 1
tally$group

# Map data to aesthetics of plot
ggplot(
  data = tally,
  mapping = aes(x = pop_density, y = total,
                color = name, size = area_land, group = group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


ggplot(
  data = tally,
  mapping = aes(x = name_label, y = pop, fill = pop)
) + # not a pipeline! only needed for ggplot (it's like add layer on top of)
  # Add ontop a column layer
  geom_col() +
  # flip axes
  coord_flip() +
  # get rid of legend
  guides(fill = "none") +
  # Change theme
  theme_bw() +
  # add labels!
  labs(x = "Stuff", y= NULL, fill = "NAH")

