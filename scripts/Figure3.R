pacman::p_load(
  ggplot2, 
  dplyr, 
  sf,
  rnaturalearth, 
  rnaturalearthhires,   # cartes et régions détaillées
  ggspatial, 
  tidygeocoder,
  ggrepel, ggpubr, tigris
)

source("scripts/map_functions.R")

### World map ###
df_geo <- df_networks %>%
  add_count(network, name = "nb_articles") %>%
  count(nb_articles, continent, scale_grp, country, name_location, network) %>%
  geocode(address = name_location, method = "osm", lat = lat, long = lon)

## Min-max médiane nb articles par réseau ##
# summary_stats <- c(
#   min = min(df_geo$nb_articles, na.rm = TRUE),
#   median = median(df_geo$nb_articles, na.rm = TRUE),
#   mean = mean(df_geo$nb_articles, na.rm=TRUE),
#   max = max(df_geo$nb_articles, na.rm = TRUE)
# )
# summary_stats


# 2) Géocode spécifique mais on conserve chaque « region » comme ligne à part
special <- tribble(
  ~name_location,                                                               ~region,
  "Saxony and Thuringia",                                                        "Saxony",
  "Saxony and Thuringia",                                                        "Thuringia",
  "New South Wales and Western Australia and Queensland and South Australia",            "New South Wales",
  "New South Wales and Western Australia and Queensland and South Australia",            "Western Australia",
  "New South Wales and Western Australia and Queensland and South Australia",            "Queensland",
  "New South Wales and Western Australia and Queensland and South Australia",            "South Australia"
) %>%
  geocode(address = region, method = "osm", lat = "lat_spec", long = "lon_spec")

data_world <- df_geo %>%
  left_join(special, by = "name_location") %>%
  # si on a un lat_spec, on l’utilise, sinon on reste sur lat
  mutate(
    lat  = if_else(!is.na(lat_spec),  lat_spec,  lat),
    lon  = if_else(!is.na(lon_spec),  lon_spec,  lon)
  ) %>%
  select(-lat_spec, -lon_spec)

plot_final_world <- gen_world_map(data_world)
plot_final_world

ggsave(plot_final_world, filename = "figures/maps/map_world.png", width = 30, height = 18, dpi = 600, units = "cm")


### Zoom Europe ###

data_europe = data_world %>%
  filter(continent == "Europe")

plot_final_europe = gen_region_map(data_europe, "Europe", long_limits=c(-23, 32), lat_limits = c(33, 70))
plot_final_europe
ggsave(plot_final_europe, filename = "figures/maps/map_europe.png", width = 15, height = 15, dpi = 600, units = "cm")


### Zoom Amérique du Nord ###

data_am = data_world %>% filter(continent == "North America")
plot_final_am = gen_region_map(data_am, "North America", long_limits=c(-162,-50), lat_limits = c(20, 84))
plot_final_am
ggsave(plot_final_am, filename = "figures/maps/map_am.png", width = 15, height = 15, dpi = 600, units = "cm")


### Echelle du réseau

data_plot = df_networks %>% 
  distinct(network, .keep_all = TRUE) %>%   # Garde une seule ligne par valeur unique de 'network'
  mutate(scale_grp = factor(scale_grp, levels = c("Hospital or group of hospitals", 
                                                  "Subnational", 
                                                  "National"))) %>%
  count(scale_grp)

plot_scale <- ggplot(data_plot, aes(y = scale_grp, x = n, fill = scale_grp)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of networks", y = "", fill ="") +
  scale_fill_manual(values = c("Hospital or group of hospitals" = color_hospital, 
                               "Subnational" = color_subnational, 
                               "National" = color_national)) +
  scale_x_continuous(limits = c(0, 35), breaks = seq(0, 35, 5), expand = expansion(mult = c(0, 0.05))) +  # Ensures bars start exactly at 0
  theme_classic(base_size = 14) +  # More professional theme with larger base font
  theme(
    legend.position = "none",  # Remove legend
    axis.text = element_text(size = 12, color = "black"),  # Professional font
    axis.title = element_text(size = 14),  # Bold axis labels
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )
# sauvegarde plot tout seul
ggsave(plot_scale, filename = "figures/maps/plot_scale.png", width = 30, height = 10, dpi = 600, units = "cm")  # High-resolution 600 dpi


### Ggarrange ###

# plot_final_arrange = ggarrange(
#   plot_scale,  # Ajout du plot scale en haut
#   plot_final_world, 
#   ggarrange(plot_final_europe, plot_final_am, ncol = 2, labels = c("C", "D"), align = "hv"), 
#   ncol = 1, nrow = 3, labels = c("A", "B", ""), 
#   heights = c(0.5, 1, 1)  # Réduction de la hauteur de A (plot_scale)
# )

plot_final_arrange = ggarrange(
  plot_scale,  # Plot A en haut
  ggarrange(
    plot_final_europe, plot_final_am,
    ncol = 2, labels = c("B", "C"), align = "hv"
  ),
  ncol = 1, nrow = 2, labels = c("A", ""), 
  heights = c(0.5, 2)  # Réduction de la hauteur du plot_scale
)

ggsave(plot_final_arrange, filename = "figures/maps/maps_world_eu_am.png", width = 25, height = 25, dpi = 600, units = "cm")

library(officer)
library(rvg)

ppt <- read_pptx()
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")

ppt <- ph_with(
  ppt,
  dml(ggobj = plot_final_arrange),
  location = ph_location(
    left = 0, top = 0,
    width = 25/2.54,
    height = 25/2.54
  )
)

print(ppt, target = "figures/maps/maps_eu_am_editable.pptx")





# ppt
# ppt <- read_pptx(); ppt <- add_slide(ppt, "Blank", "Office Theme")
# dml_dev <- rvg::dml_pptx(pptx = ppt, left = 0, top = 0, width = 30/2.54, height = 30/2.54, bg = "transparent")
# print(plot_final_arrange)
# dev.off()
# print(ppt, target = "figures/maps/maps_world_eu_am_editable.pptx")
# 



