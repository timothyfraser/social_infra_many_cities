#' @name `09_figure_2.R`
#' @title external validity comparisons
#' @description Tests of external validity for Boston case.
#'


data("connect")
data("wkb_as_sf")
data("aea")
data("meta")

# Is Boston representative of other cities in our sample?
source("keys.R")
geo = connect()

geo %>% dbListTables()

v = c("pop_density_block", "white_block", "black_block", "asian_block", "hisplat_block",
      "median_household_income_bg", "income_0_60K_bg",
      "some_college_bg", "over_65_bg", "unemployment_bg") %>%
  tibble(var = .)


# Get just block groups from populated block groups in our sample
raw = geo %>%
  tbl("data_grid") %>%
  # From populated blocks....
  filter(pop_density_block > 0) %>%
  # In our sample of top 25 cities
  filter(name %in% !!meta$top) %>%
  collect() %>%
  # Get just these key demographic variables
  select(cell, name, all_of(v$var))

transform_it = function(var, value, .f = "log"){
  if(.f == "log"){ .f = log; k = 1; j = 0
  }else if(.f == "exp"){ .f = exp; k = 0; j = 1 }
  # Now log transform key variables which are very right skewed
  case_when(
    # For population, add 1 person (so it's not 0)
    var == "pop_density_block" ~ .f(value + k*1) - j*1,
    # For percentages and rates, add just 1% (so it's not 0)
    var == "hisplat_block" ~ .f(value + k*0.01) - j*0.01,
    var == "black_block" ~ .f(value + k*0.01) - j*0.01,
    var == "asian_block" ~ .f(value + k*0.01) - j*0.01,
    var == "unemployment_bg" ~ .f(value + k*0.01) - j*0.01,
    TRUE ~ value)
}

data = raw %>%
  # Pivot longer into a tally data.frame
  pivot_longer(cols = all_of(v$var), names_to = "var", values_to = "x") %>%
  mutate(xtrans = transform_it(var = var, value = x, .f = "log")) %>%
  group_by(var) %>%
  mutate(value = scale(xtrans)) %>%
  ungroup() %>%
  mutate(type = if_else(name == "boston", "boston", "other"))

library(tidyr)
library(broom)
library(purrr)
library(ggplot2)

# Get statistics
stat = data %>%
  group_by(var) %>%
  summarize(
    # Get t.test results
    t.test(value ~ type, paired = FALSE) %>% broom::tidy(),
    # Get a BUNCH of descriptive statistics too
    p005 = quantile(value, prob = 0.005, na.rm = TRUE),
    p025 = quantile(value, prob = 0.025, na.rm = TRUE),
    p250 = quantile(value, prob = 0.25, na.rm = TRUE),
    p500 = quantile(value, prob = 0.50, na.rm = TRUE),
    p750 = quantile(value, prob = 0.75, na.rm = TRUE),
    p975 = quantile(value, prob = 0.975, na.rm = TRUE),
    p995 = quantile(value, prob = 0.995, na.rm = TRUE),

    # Get original values
    x005 = quantile(x, prob = 0.005, na.rm = TRUE),
    x025 = quantile(x, prob = 0.025, na.rm = TRUE),
    x250 = quantile(x, prob = 0.25, na.rm = TRUE),
    x500 = quantile(x, prob = 0.50, na.rm = TRUE),
    x750 = quantile(x, prob = 0.75, na.rm = TRUE),
    x975 = quantile(x, prob = 0.975, na.rm = TRUE),
    x995 = quantile(x, prob = 0.995, na.rm = TRUE),
    xmin = quantile(x, prob = 0, na.rm = TRUE),
    xmax = quantile(x, prob = 1, na.rm = TRUE),
    x1 = mean(xtrans[type == "boston"], na.rm = TRUE),
    x2 = mean(xtrans[type != "boston"], na.rm = TRUE),
  ) %>%
  ungroup() %>%
  # Back transform these means and log means into their original units
  mutate(x1 = transform_it(var = var, value = x1, .f = "exp"),
         x2 = transform_it(var = var, value = x2, .f = "exp"),
         # Difference of means, put back into original units.
         xdiff = x1 - x2,
         # Percent of 99% Range
         xrange = abs(xdiff) / (xmax - xmin)
  ) %>%
  # Format text!
  mutate(direction = case_when(
    x1 > x2 ~ "plus",
    x2 < x1 ~ "minus",
    x1 == x2 ~ "neutral")) %>%
  mutate(
    across(.cols = c(x1, x2, xdiff),
           .fns = ~case_when(
             var %in% c("white_block", "hisplat_block", "black_block", "asian_block",
                        "over_65_bg", "income_0_60K_bg", "some_college_bg") ~ scales::percent(.x, accuracy = 1, suffix = "%", style_positive = "plus", style_negative = "minus"),
             var == "median_household_income_bg" ~ scales::number(.x, accuracy = 1, prefix = "$", decimal.mark = ",", style_positive = "plus", style_negative = "minus"),
             TRUE ~ scales::number(.x, accuracy = 1, style_positive = "plus", style_negative = "minus")
           ))) %>%
  mutate(xrange = scales::percent(xrange, accuracy = 1, suffix = "%")) %>%
  mutate(sig = gtools::stars.pval(p.value)) %>%
  # Add in a blank row
  bind_rows(tibble(var = " ")) %>%
  mutate(var = var %>% dplyr::recode_factor(
    "unemployment_bg" = "Unemployment\nRate (log)",
    "income_0_60K_bg" = "$0-60K \nIncome Bracket (%)",
    "median_household_income_bg" = "Median Household\n Income ($)",
    "some_college_bg" = "% Some College",
    "white_block" = "% White",
    "asian_block" = "% Asian (log)",
    "hisplat_block" = "% Hispanic/\nLatino (log)",
    "black_block" = "% Black (log)",
    "over_65_bg" = "% Over Age 65",
    "pop_density_block" = "Pop. Density (log)",

    " " = " "))

# Show me the 95% range of these
colors = list(blue = "#648FFF")
colors$b1 = colors$blue
colors$b2 = colorspace::adjust_transparency(blue, alpha = 0.5)
colors$b3 = colorspace::adjust_transparency(blue, alpha = 0.25)

theme_set(
  theme_classic(base_size = 14) +
    theme(panel.border = element_rect(color = NA, fill = NA),
          axis.line.y = element_blank())
)

# Metadata for chart
n_cells = nrow(raw) %>% scales::number()
ylab = paste0("Value Range (n = ", n_cells, " grid cells)",
              "\n", "(standard deviations, where 0 = mean)")
title = paste0("Difference in Mean Demographic Traits \nfor Cells in Boston vs. Other Top 24 Cities")
k = 3.5 # placement of Difference stats on y axis
vjust = -2.5 # placement of header labels
benchmark = "Pop. Density (log)" # benchmark for header labels
caption = paste(
  "Significance [p-value]: *** p < 0.001; ** p < 0.01; * p < 0.05; . p < 0.10",
  "Meaningfulness [% Range]: shows size of difference, as a percentage of the range.",
  sep = "\n")


gg = ggplot() +
  geom_crossbar(
    data = stat, mapping = aes(x = var, y = p500, ymin = p005, ymax = p995, fill = "99"), color = NA) +
  geom_crossbar(
    data = stat, mapping = aes(x = var, y = p500, ymin = p025, ymax = p975, fill = "95"),  color = NA) +
  geom_crossbar(
    data = stat, mapping = aes(x = var, y = p500, ymin = p250, ymax = p750, fill = "50"), color = "white") +
  scale_fill_manual(
    name = "Most Common Range (%)",
    breaks = c("50", "95", "99"),
    labels = c("25-75%", "2.5-97.5%", "0.5-99.5%"),
    values = c(colors$b1, colors$b2, colors$b3)) +
  ggnewscale::new_scale_fill() +
  geom_linerange(
    data = stat,
    mapping = aes(x = var, ymin = estimate1, ymax = estimate2), linewidth = 1) +
  geom_point(
    data = stat,
    mapping = aes(x = var, y = estimate1, fill = "boston"), shape = 21, size = 4, stroke = 1, color = "white") +
  geom_point(
    data = stat,
    mapping = aes(x = var, y = estimate2, fill = "other"), shape = 21, size = 4, stroke = 1, color = "black") +
  scale_fill_manual(
    name = "Mean Value by Sample",
    breaks = c("boston", "other"),
    labels = c("Boston", "Other 24 Cities"),
    values = c("black", "white")) +
  # Labels
  shadowtext::geom_shadowtext(
    mapping = aes(x = benchmark, y = 0, label = "Group Means"),
    vjust = vjust, hjust = 0.5, color = "black", bg.color = "white", bg.r = 0.3, fontface = "bold"
  ) +
  shadowtext::geom_shadowtext(
    mapping = aes(x = benchmark, y = -2.5, label = "Distribution"),
    vjust = vjust, hjust = 0, color = colors$b1, bg.color = "white", bg.r = 0.3, fontface = "bold"
  ) +
  geom_errorbar(
    mapping = aes(x = " ", ymin = -2.75, ymax = 2.25),
    color = colors$b1, width = 0.25
  ) +
  geom_point(
    mapping = aes(y = c(0.5, 1), x = c(" ", " "),
                  fill = c("boston", "other")),
    alpha = 0.5, shape = 21, size = 2, stroke = 1) +
  # Difference of Means and p-values
  shadowtext::geom_shadowtext(
    mapping = aes(x = benchmark, y = k, label = "Difference"),
    vjust = vjust, hjust = 1, color = "darkgrey", bg.color = "white", bg.r = 0.3, fontface = "bold"
  ) +
  shadowtext::geom_shadowtext(
    mapping = aes(x = benchmark, y = k + 0.1, label = "p"),
    vjust = vjust, hjust = 0, color = "darkgrey", bg.color = "white", bg.r = 0.3, fontface = "bold"
  ) +
  shadowtext::geom_shadowtext(
    data = stat, mapping = aes(x = var, y = k, label = xdiff, color = direction), hjust = 1, bg.color = "white", bg.r = 0.3) +
  shadowtext::geom_shadowtext(
    data = stat, mapping = aes(x = var, y = k + 0.1, label = sig,color = direction),
    hjust = 0, bg.color = "white", bg.r = 0.3) +
  # % of range
  shadowtext::geom_shadowtext(
    mapping = aes(x = benchmark, y = k + 0.6, label = "% Range"),
    vjust = vjust, hjust = 0, color = "darkgrey", bg.color = "white", bg.r = 0.3, fontface = "bold"
  ) +
  shadowtext::geom_shadowtext(
    data = stat, mapping = aes(x = var, y = k + 0.6, label = xrange, color = direction),
    hjust = 0, bg.color = "white", bg.r = 0.3) +
  scale_y_continuous(expand = expansion(add = c(0, 1))) +
  scale_color_manual(breaks = c("plus", "neutral", "minus"), guide = "none",
                     values = c(colors$b1, "black", "red")) +
  labs(y = ylab, x = NULL, title = title, caption = caption) +
  theme(legend.position = "bottom", legend.box = "vertical",
        axis.ticks = element_blank(),
        legend.margin = margin(0,0,0,0, "cm"),
        legend.box.margin = margin(0,0,0,0, "cm"),
        legend.justification = "left",
        legend.box.just = "left",
        legend.spacing = unit(0, "cm"),
        panel.spacing = unit(0, "cm"),
        plot.margin = margin(0.1,0.1,0.1,0.1, "cm"),
        legend.title = element_text(size = 11, color = "#373737", hjust = 0.5),
        legend.text = element_text(size = 10, color = "#373737", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0, color = "#373737")
  ) +
  coord_flip()

ggsave(plot = gg, filename = "viz/crossbars_boston_vs_range.png", dpi = 500, width = 7, height = 8)

dbDisconnect(geo)
rm(list = ls()); gc()
