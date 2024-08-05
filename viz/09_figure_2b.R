# FIGURE 2: MAP OF RATES #########################

#' @name `09_figure_2.R`
#' @title MAP RATES FOR ALL CITIES

## Data #########################################

library(sf)
library(dplyr)
library(DBI)
library(readr)
library(purrr)
library(tidyr)
library(broom)

source("keys.R")
data("meta")
data("connect")
geo = connect()
geo %>% dbListTables()

# Get stats per city
tab = read_rds("viz/poptally.rds") %>%
  mutate(pop = scales::number(pop, scale = 1/1e6, accuracy = 0.01, decimal.mark = ".", big.mark = ",", suffix = "M")) %>%
  mutate(label = paste0(total, " sites in ", cells, " cells", "\n", "pop. ", pop)) %>%
  #mutate(name_label = paste0(name_label, " (", "pop. ", pop, ")")) %>%
  select(name, name_label, label)


# Get rates per cell
original =  geo %>%
  tbl("data_grid") %>%
  filter(name %in% !!meta$top) %>%
  collect() %>%
  select(name, cell, social_capital, bonding:linking,
         total, community_space, place_of_worship, social_business, park,
         pop_density_block) %>%
  select(name, cell, total, pop_density_block) %>%
  # Join in names
  left_join(by = "name", y = tab)

dbDisconnect(geo); remove(geo)

# Load shapes
grid = read_sf("search/all/tally1km.geojson") %>%
  select(name, cell, geometry) %>%
  inner_join(
    by = c("name", "cell"),
    y = original)


bounds = read_sf("search/all/bounds.geojson")

# Fix the cropping on these shapes, to make clearer visuals.

# San Francisco Crop
cropbox = c("xmin" = -122.55, "xmax" = -122.3276, "ymin" = 37.69274, "ymax" = 37.85) %>% st_bbox()

bounds = bind_rows(
  bounds %>% filter(name != "san_francisco"),
  bounds %>% filter(name == "san_francisco") %>% st_crop(cropbox)
)

# Test it
ggplot() +
  geom_sf(data = bounds2 %>% filter(name == "san_francisco"))

# Load Packages
library(sf)
library(ggplot2)


get_viz = function(.name = "la"){

  mybounds = bounds %>% filter(name == .name)

  # Get bounds
  mygrid = grid %>% filter(name == .name)
  # Get stats
  mytab = tab %>% filter(name == .name)

  # Visualize the study areas and their missing data
  gg = ggplot() +
    geom_sf(data = mygrid, mapping = aes(fill = total),
            linewidth = 0.01, color = "#373737") +
    geom_sf(data= mybounds, fill = NA, linewidth = 1.5, color = "black") +
    geom_sf(data= mybounds, fill = NA, linewidth = 0.5, color = "white") +
    scale_fill_viridis(
      breaks = c(0.01, 0.03, 1, 3, 10, 30, 100, 300, 1000, 30000),
      labels = scales::label_number(
        scale_cut = scales::cut_si("")),
      guide = guide_colorsteps(show.limits = TRUE,
        barwidth = 20,
        barheight = 2,
        frame.color = "#373737",
        ticks = TRUE),
      name = "<b>Rate of Social Infrastructure</b><br><i>sites per 1000 residents per 1 sq.km.</i>",
      option = "plasma", trans = "log", na.value = "black") +
    theme_void(base_size = 14) +
    #guides(fill = "none") +
    theme(plot.subtitle = element_text(hjust = 0, size = 10),
          plot.title = element_text(hjust = 0),
          plot.margin = margin(0,0,0,0,"cm"),
          legend.title = ggtext::element_markdown(size = 14),
          legend.position = "bottom") +
    labs(subtitle = mytab$label,
         title = mytab$name_label)

  return(gg)
}

#meta$top[22] %>% get_viz()
# Generate ggplots for every city
meta$top %>% map(~get_viz(.name = .)) %>% saveRDS("viz/gg_rates.rds")

gg = read_rds("viz/gg_rates.rds")
#gg[[2]]

library(ggpubr)
ggall =  ggpubr::ggarrange(
  plotlist = gg,
  ncol = 5, nrow = 5,
  legend = "bottom",
  common.legend = TRUE)

ggsave(plot = ggall, filename = "viz/figure_A2_rates_map.png", dpi = 200, width = 9, height = 12)

rstudioapi::viewer("viz/figure_A2_rates_map.png")
rm(list = ls())

