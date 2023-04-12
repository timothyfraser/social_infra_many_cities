## Bars

library(tidyverse)
library(sf)

# First, get population for each place
read_rds("search/census2020/bg_data.rds") %>%
  mutate(tract = str_sub(geoid, 1, 11)) %>%
  left_join(by = c("tract" = "geoid"), y = read_csv("census2020/sample_tracts.csv")) %>%
  group_by(name) %>%
  summarize(pop = sum(pop, na.rm = TRUE)) %>%
  mutate(name_label = name %>% str_split(pattern = "_") %>%
           map(~paste(str_sub(., 1,1) %>% toupper(),
                      str_sub(., 2,-1), sep = "")) %>%
           map(~paste(., collapse = " ")) %>% unlist(),
         name_label = if_else(name_label %in% c("Nyc", "La", "Dc"), toupper(name_label), name_label),
         name_label = if_else(name_label == "Oklahoma", "Oklahoma City", name_label)) %>%
  mutate(pop_label = round(pop / 1e6, 2)) %>%
  saveRDS("viz/poptally.rds")

tally <- read_sf("search/all/tally1kmbg.geojson") %>%
  as_tibble() %>%
  select(name, pop_density, community_space:park) %>%
  pivot_longer(cols = c(community_space:park), names_to = "type", values_to = "rate") %>%
  group_by(name,
           type = type %>% recode_factor(
             "community_space" = "**Community Spaces**",
             "place_of_worship" = "**Places of Worship**",
             "social_business" = "**Social Businesses**",
             "park" = "**Parks**")) %>%
  summarize(mean = mean(rate*10, na.rm = TRUE)) %>%
  mutate(label = round(mean, 1)) %>%
  # Join in population
  left_join(by = "name",
            y = read_rds("viz/poptally.rds"))

tally_stat <- tally %>%
  arrange(desc(pop), type) %>%
  group_by(type) %>%
  mutate(order = 1:n()) %>%
  group_by(type) %>%
  mutate(add = mean / 75,
         fill = if_else(str_detect(name_label, "Boston"), "Case", "Other")) %>%
  ungroup() %>%
  filter(!name %in% c("worcester", "ithaca") )

saveRDS(tally_stat, file = "viz/tally_stat.rds")

g1 <- tally_stat %>%
  ggplot(mapping = aes(x = reorder(name_label, -order), y = mean + add, label = label)) +
  geom_col(mapping = aes(y = mean + add*20), color = NA, fill = NA) +
  geom_col(mapping = aes(y = mean, fill = fill)) +
  ggtext::geom_richtext(mapping = aes(y = mean + add),
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
  scale_fill_manual(values = c("#00338e", "darkgrey"), guide = 'none') +
  scale_y_continuous(expand = expansion(add = c(0, 2)), trans = "log",
                     breaks = c(1,  10, 100, 1000)) +
  labs(x = NULL, y = "Mean Sites per 10,000 residents, per 1 km<sup>2</sup>",
       subtitle = "Average Rates of Social Infrastructure by City")

ggsave(g1, filename = "viz/cities_avg_all.png", dpi = 500, width = 10, height = 5.5)


d = read_rds("viz/tally_stat.rds") %>%
  filter(type == "**Community Spaces**")
