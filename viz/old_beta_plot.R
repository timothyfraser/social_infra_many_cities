#' @name 09_figure_7.R
#' @author Tim & Osama

# Figure 6 ##################################
## Beta Coefficients ##################################

library(dplyr)
library(readr)
library(broom)
library(tidyr)
library(ggplot2)
library(viridis)

# Load models
m = read_rds("viz/models_main.rds")

## Version 1 ######################################################


#Creating a dataframe holding information pertaining to hypothesis 4.
df2 <- data.frame(types = c('Community Spaces', 'Places of Worship', 'Social Businesses', 'Parks'),
                  bonding_effects = m$subtypes$bonding$coefficients[2:5],
                  bridging_effects = m$subtypes$bridging$coefficients[2:5]) %>%
  mutate(bonding_labels = round(bonding_effects, digits = 3),
         bridging_labels = round(bridging_effects, digits = 3))

gg = ggplot(df2, aes(x = types)) +
  geom_bar(mapping = aes(y = bonding_effects, fill = "Bonding"), stat = 'identity') +
  geom_bar(mapping = aes(y = bridging_effects, fill = "Bridging"), stat = 'identity') +
  geom_text(mapping = aes(y = bonding_effects, label = bonding_labels), hjust = 1) +
  geom_text(mapping = aes(y = bridging_effects, label = bridging_labels), hjust = 0) +

  labs(title = 'Associations of Bonding and Bridging Social Capital with Social Infrastructure',
       x = 'Types',
       y = 'Association (Beta) with (Logged) Rate of Social Infrastructure',
       fill = 'Type of Social Capital') +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(fill = NA, color = "#373737")) +
  scale_fill_manual(values = c("#DC267F", "#648FFF"), breaks = c("Bonding", "Bridging"),
                    labels = c("Bonding\nSocial Capital", "Bridging\nSocial Capital")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  scale_y_continuous(expand = expansion(add = c(0.003, 0.003))) +
  coord_flip()

remove(gg)

## Version 2 ########################################


df3 = bind_rows(
  m$subtypes$bonding %>% tidy() %>% mutate(group = "Bonding"),
  m$subtypes$bridging %>% tidy() %>% mutate(group = "Bridging")
) %>%
  # Filter to just variables containing these terms
  filter(stringr::str_detect(term, "community_space|place_of_worship|social_business|park")) %>%
  # Recode term
  mutate(term = term %>% dplyr::recode_factor(
    "log(community_space + 1)" = "Community Spaces",
    "log(park + 1)" = "Parks",
    "log(place_of_worship + 1)" = "Places of Worship",
    "log(social_business + 1)" = "Social Businesses"
  )) %>%
  # Get significance
  mutate(significance = gtools::stars.pval(p.value)) %>%
  mutate(label = paste0(round(estimate, 3), significance))


gg = ggplot() +
  # Bars
  geom_bar(
    data = df3 %>% filter(group == "Bonding"),
    mapping = aes(x = term, y = estimate, fill = group),
    stat = "identity") +
  geom_bar(
    data = df3 %>% filter(group == "Bridging"),
    mapping = aes(x = term, y = estimate, fill = group),
    stat = "identity") +
  # Text
  shadowtext::geom_shadowtext(
    data = df3 %>% filter(group == "Bonding"),
    mapping = aes(x = term, y = estimate, group = group, label = label),
    stat = "identity", hjust = 1,  bg.r = 0.1, bg.color = "white", color = "black") +
  shadowtext::geom_shadowtext(
    data = df3 %>% filter(group == "Bridging"),
    mapping = aes(x = term, y = estimate, group = group, label = label),
    stat = "identity", hjust = 0, bg.r = 0.1, bg.color = "white", color = "black") +
  # Labels and Settings
  labs(title = 'Associations of Bonding and Bridging Social Capital with Social Infrastructure',
       x = 'Types',
       y = 'Association (Beta) with (Logged) Rate of Social Infrastructure',
       fill = 'Type of Social Capital') +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(fill = NA, color = "#373737")) +
  theme_minimal()+
  coord_flip()+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(fill = NA, color = "#373737")) +
  scale_fill_manual(values = c("#DC267F", "#648FFF"),
                    breaks = c("Bonding", "Bridging"),
                    labels = c("Bonding\nSocial Capital", "Bridging\nSocial Capital")) +
  scale_y_continuous(expand = expansion(add = c(0.005, 0.005))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1)

ggsave(plot = gg, filename = "viz/figure_7_beta_plot.png", dpi = 500, width = 8, height = 6)
rstudioapi::viewer("viz/figure_7_beta_plot.png")

