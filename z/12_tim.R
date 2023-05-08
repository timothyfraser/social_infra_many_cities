#' @name 12_tim.R
#' @description A script for visualizations!
#'
#'

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







