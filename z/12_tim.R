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


#I'm still not sure what the total (x axis) represents
#I'm also not sure how to change the name of each type of social capital on the legend
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

ggsave(gg, filename = "viz/figure_5_predictions.png", dpi = 500, width = 6.5, height= 6)


# After wrapping up this paper. would you want to...

# 1. Work on an Evacuation related project this summer?

# 2. Work on an Evacuation related project this fall?

# 3. Work on an Emissions related project (same)

# 4. Work on your own research project with some guidance

# 5. Go forth and be merry and do your own thing?


library(dplyr)
library(ggplot2)
library(viridis)

# 1. Make Grid ############################

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

# First Try ############################################

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


# 3. Simulated Visual ############################

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








df3 = bind_rows(
  m$subtypes$bonding %>% tidy() %>% mutate(group = "Bonding"),
  m$subtypes$bridging %>% tidy() %>% mutate(group = "Bridging")
) %>%
  # Filter to just variables containing these terms
  filter(stringr::str_detect(term, "community_space|place_of_worship|social_business|park")) %>%
  # Recode term
  mutate(term = term %>% dplyr::recode_factor(
    "log(community_space + 1)" = "Community Spaces",
    "log(place_of_worship + 1)" = "Places of Worship",
    "log(social_business + 1)" = "Social Businesses",
    "log(park + 1)" = "Parks")) %>%
  # Get significance
  mutate(significance = gtools::stars.pval(p.value)) %>%
  mutate(label = paste0(round(estimate, 3), significance))

ggplot() +
  geom_bar(
    data = df3 %>% filter(group == "Bonding"),
    mapping = aes(x = term, y = estimate, fill = group),
    stat = "identity") +
  geom_bar(
    data = df3 %>% filter(group == "Bridging"),
    mapping = aes(x = term, y = estimate, fill = group),
    stat = "identity") +
  shadowtext::geom_shadowtext(
    data = df3 %>% filter(group == "Bonding"),
    mapping = aes(x = term, y = estimate, fill = group, label = label),
    stat = "identity") +
  shadowtext::geom_shadowtext(
    data = df3 %>% filter(group == "Bridging"),
    mapping = aes(x = term, y = estimate, fill = group, label = label),
    stat = "identity") +
  labs(title = 'Bonding and Bridging Effects',
       x = 'Types',
       y = 'Association with (Logged) Rate of Social Infrastructure',
       fill = 'Effect Type') +
  theme_minimal()+
  coord_flip()+
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#DC267F", "#648FFF"),
                    breaks = c("Bonding", "Bridging"),
                    labels = c("Bonding\nSocial Capital", "Bridging\nSocial Capital")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1)


#I want to change the placement of the axis to make it apparent that some effects
#are negative



# Z. Other Previous Examples of Bar Charts #########################
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


