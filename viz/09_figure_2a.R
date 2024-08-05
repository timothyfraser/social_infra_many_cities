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
# tab = read_rds("viz/poptally.rds") %>%
#   mutate(pop = scales::number(pop, scale = 1/1e6, accuracy = 0.01, decimal.mark = ".", big.mark = ",", suffix = "M")) %>%
#   mutate(label = paste0(total, " sites in ", cells, " cells", "\n", "pop. ", pop)) %>%
#   #mutate(name_label = paste0(name_label, " (", "pop. ", pop, ")")) %>%
#   select(name, name_label, label)

# Get rates per tract
original = geo %>%
  tbl("data_tract") %>%
  filter(name %in% !!meta$top) %>%
  select(name, geoid, #social_capital, bonding:linking,
         total, #community_space, place_of_worship, social_business, park,
         pop_density, pop, area) %>%
  # Count up total sites per census tract
  mutate(sites = total * pop_density / 1000) %>%
  collect()

dbDisconnect(geo); remove(geo)

# Calculate statistics per city.
cities = original %>%
  group_by(name) %>%
  summarize(sites = sum(sites, na.rm = TRUE),
            pop = sum(pop, na.rm = TRUE),
            area = sum(area, na.rm = TRUE),
            tracts = sum(!is.na(geoid))) %>%
  mutate(label = paste0(sites, " sites in ", tracts, " tracts")) %>%
  left_join(by = "name", y= read_csv("viz/cities.csv") %>% select(name, name_label, pop_label)) %>%
  mutate(pop_label = paste0("pop. ", pop_label))

bounds = read_sf("search/all/bounds.geojson") %>%
  inner_join(by = c("name"), y = cities %>% select(name))

# Fix the cropping on these shapes, to make clearer visuals.

# San Francisco Crop
cropbox = c("xmin" = -122.55, "xmax" = -122.3276, "ymin" = 37.69274, "ymax" = 37.85) %>% st_bbox()

bounds = bind_rows(
  bounds %>% filter(name != "san_francisco"),
  bounds %>% filter(name == "san_francisco") %>% st_crop(cropbox)
)


# Load shapes
grid = read_sf("search/all/tracts.geojson") %>%
  select(name, geoid, geometry) %>%
  inner_join(
    by = c("name", "geoid"),
    y = original)

grid2 = meta$top %>%
  map(~st_intersection(x = grid %>% filter(name == .x),
                  y = bounds %>% filter(name == .x))) %>%
  bind_rows()

# Black out any tracts with no sites.
grid2 = grid2 %>%
  mutate(total = if_else(sites == 0, true = NA, false = total))

# grid %>%
#   st_join(y = bounds, join = st_crop, left = FALSE)
#
# # st_covered_by(x = grid %>% filter(name == "san_jose"), y = bounds, sparse = FALSE)
#
# grid %>%
#   filter(name == "san_jose") %>%
#   st_join(y = bounds %>% select(), join = st_covered_by, left = FALSE) %>%
#   ggplot(mapping = aes(geometry = geometry, fill = total, label = geoid)) +
#   geom_sf()
#   geom_sf_label()

# grid %>%
#   head(10) %>%
#   st_join(y = bounds %>% select(), join = st_intersects, left = FALSE)



# Test it
ggplot() +
  geom_sf(data = bounds %>% filter(name == "san_francisco"))



# Load Packages
library(sf)
library(ggplot2)


get_viz = function(.name = "la"){

  mybounds = bounds %>% filter(name == .name)

  # Get bounds
  mygrid = grid2 %>% filter(name == .name)
  # Get stats
  mytab = cities %>% filter(name == .name)

  # Visualize the study areas and their missing data
  gg = ggplot() +
    #geom_sf(data= mybounds, fill = "black", linewidth = 1.5, color = "black") +
    geom_sf(data = mygrid, mapping = aes(fill = total), color = NA) +
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
#
# grid %>%
#   filter(name == "el_paso") %>%
#   st_intersection(y = bounds) %>%
#   select(total) %>%
#   plot()
#
# grid %>%
#   filter(name == "el_paso") %>%
#   mutate(geometry = st_intersection(geometry, y = bounds %>% filter(name == "el_paso"))) %>%
#   select(total) %>%
#   plot()
# st_crop()
# st_crop(x = grid %>% filter(name == "el_paso"),
#         y = bounds %>% filter(name == "el_paso")) %>%
#   select(total) %>%
#   plot()

meta$top[23] %>% get_viz()
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

ggsave(plot = ggall, filename = "viz/figure_A2a_rates_map.png", dpi = 200, width = 9, height = 12)

rstudioapi::viewer("viz/figure_A2a_rates_map.png")
rm(list = ls())

