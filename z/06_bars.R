## Bars

library(dplyr)
library(ggplot2)
library(shadowtext)
library(readr)
library(ggtext)

d = read_rds("viz/tally_stat.rds") %>%
  group_by(type) %>%
  mutate(highlight = case_when(
    rate == max(rate, na.rm = TRUE) ~ "Highest",
    rate == min(rate, na.rm = TRUE) ~ "Lowest",
    TRUE ~ "Other")) %>%
  ungroup()

g1 <- d %>%
  ggplot(mapping = aes(x = reorder(name_label, -order), y = rate + add, label = label)) +
  geom_col(mapping = aes(y = rate + add*20), color = NA, fill = NA) +
  geom_col(mapping = aes(y = rate, fill = highlight)) +
  ggtext::geom_richtext(mapping = aes(y = rate + add),
                        hjust = 0,  color = "#373737",
                        fill = NA, label.color = NA) +
  facet_grid(cols = vars(type), scales = "free_x") +
  theme_bw(base_size =14) +
  theme(strip.background = element_blank(),
        strip.text.x = ggtext::element_markdown(hjust = 0.5),
        axis.text.y = ggtext::element_markdown(hjust = 0, size = 12),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = ggtext::element_markdown(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        panel.spacing.y = unit(0, "cm"),
        panel.spacing.x = unit(1, "cm")) +
  coord_flip() +
  scale_fill_manual(breaks = c("Highest", "Lowest", "Other"),
                    values = c("#648FFF", "#DC267F", "darkgrey"), guide = 'none') +
  scale_y_continuous(expand = expansion(add = c(0, 2)),na.value = 0.01,
                     limits = c(0.01, max(d$rate)),
                     trans = "log",
                     breaks = c(1,  10, 1000)) +
  labs(x = NULL, y = "Mean Sites per 10,000 residents, per 1 km<sup>2</sup>",
       subtitle = "Average Rates of Social Infrastructure by City") +
  theme(panel.spacing = unit(0, "cm"), plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))


ggsave(g1, filename = "viz/cities_avg_all.png", dpi = 500, width = 10, height = 5.5)


