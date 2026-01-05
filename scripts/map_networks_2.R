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

#### DATAFRAME AVEC LOCALISATIONS ####

df_summary <- df_networks %>%
  count(continent, scale_grp, country, name_location, network,
        name = "nb_articles")

df_geo = df_summary %>%
  geocode(address = name_location, method = "osm", lat = lat, long = lon)


# Ajout des localisations manquantes

## Australian regions, Saxony and thuringia
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

# Atlanta metropolitan area
ga_counties <- st_read("tl_2019_13_cousub/tl_2019_13_cousub.shp")
atlanta_metro <- ga_counties %>%
  filter(NAME %in% c("Fulton", "DeKalb", "Cobb", "Gwinnett", "Clayton", "Cherokee", "Douglas", "Fayette", "Henry", "Bartow")) %>%
  st_union() %>%
  st_transform(4326) %>%          # WGS84
  st_point_on_surface() %>%       # point à l'intérieur du polygone
  st_coordinates() %>%
  as_tibble()

## Rassemblement dans df_geo

data_world <- df_geo %>%
  left_join(special, by = "name_location") %>%
  # si on a un lat_spec, on l’utilise, sinon on reste sur lat
  mutate(
    lat  = if_else(!is.na(lat_spec),  lat_spec,  lat),
    lon  = if_else(!is.na(lon_spec),  lon_spec,  lon)
  ) %>%
  select(-lat_spec, -lon_spec)

data_world <- data_world %>%
  mutate(lon = if_else(network == "Atlanta Metropolitan Area", atlanta_metro$X, lon),
         lat = if_else(network == "Atlanta Metropolitan Area", atlanta_metro$Y, lat))

# modification du point de stockholm (il était faux jusqu'ici)
data_world <- data_world %>%
  mutate(lon = if_else(network == "Stockholm", 17.882, lon),
         lat = if_else(network == "Stockholm", 59.421, lat))

data_world <- data_world %>%
  mutate(lon = ifelse(network == "Central Florida", -81.7195, lon),
         lat = ifelse(network == "Central Florida", 28.9239, lat))

#### CARTE EUROPE ####

data_europe = data_world %>%
  filter(continent == "Europe")
region = "Europe"
long_limits = c(-9, 32)
lat_limits = c(37, 70) 

hospitals <- filter(data_europe, scale_grp == "Hospital or group of hospitals")
subnational <- filter(data_europe, scale_grp == "Subnational")
national <- filter(data_europe, scale_grp == "National")

countries_map <- ne_countries(scale = "medium", continent = region, returnclass = "sf") %>%
  filter(sovereignt != "Antarctica", admin != "Greenland", admin != "Iceland") %>%
  mutate(fill_color = ifelse(admin %in% national$country, color_national, "white"))

regions_map <- ne_states(country = unique(subnational$country), returnclass = "sf") %>%
  filter(region %in% subnational$network | 
           name %in% subnational$network |
           region %in% subnational$region[!is.na(subnational$region)] |
           name %in% subnational$region[!is.na(subnational$region)]
  )


# ajout regions qui ne sont pas dans regions_map
uk_regions <- ne_states(country = "United Kingdom", returnclass = "sf") %>%
  filter(geonunit %in% c("England", "Scotland"))

saxonythuringia <- ne_states(country = "Germany", returnclass = "sf") %>%
  filter(name_alt %in% c("Saxony", "Thuringia"))

westerngreece <- ne_states(country="Greece", returnclass="sf") %>%
  filter(name_alt == "Greece West")

lombardy <- ne_states(country = "Italy", returnclass="sf") %>% 
  filter(region== "Lombardia")

bavaria <- ne_states(country = "Germany", returnclass="sf") %>%
  filter(name_alt == "Bavaria")

lowersaxony <- ne_states(country = "Germany", returnclass="sf") %>%
  filter(name_alt == "Lower Saxony")

regions_map <- bind_rows(regions_map,uk_regions, saxonythuringia, westerngreece, lombardy, bavaria, lowersaxony)

# Labels enrichis
national_unique <- national %>%
  group_by(network, lon, lat) %>%
  summarise(nb_articles_total = sum(nb_articles, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = paste0(network, " (", nb_articles_total, ")"), network)

subnational_unique <- subnational %>%
  group_by(network, lon, lat) %>%
  summarise(nb_articles_total = sum(nb_articles, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = paste0(network, " (", nb_articles_total, ")"), network) %>%
  distinct(network, .keep_all = TRUE)

hospitals_unique <- hospitals %>%
  group_by(network, lon, lat) %>%
  summarise(nb_articles_total = sum(nb_articles, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = paste0(network, " (", nb_articles_total, ")"),network)

labels_all <- bind_rows(
  national_unique   %>% mutate(type = "national",   col = "grey30", face = "italic"),
  subnational_unique %>% mutate(type = "subnational", col = "black",  face = "bold"),
  hospitals_unique   %>% mutate(type = "hospital",  col = "black",  face = "plain")
)

plot_final_europe = ggplot() +
  geom_sf(data = countries_map, aes(fill = fill_color), color = "grey") +
  geom_sf(data = regions_map,   fill = color_subnational, color = color_subnational) +
  geom_point(data = hospitals, aes(x = lon, y = lat, size = n),
             color = color_hospital, size = 1.5) +
  scale_fill_identity() +
  scale_x_continuous(limits = long_limits) +
  scale_y_continuous(limits = lat_limits) +
  
  geom_text_repel(
    data = labels_all,
    aes(x = lon, y = lat, label = label,
        color = col, fontface = face),
    size          = 4,
    seed          = 42,
    force         = 1,
    force_pull    = 30,
    box.padding   = 0.5,
    point.padding = 0.5,
    segment.color = "black",
    segment.size  = 0.5,
    max.overlaps  = Inf
  ) +
  scale_color_identity() +
  theme_minimal() +
  theme(
    panel.grid   = element_blank(),
    legend.position = "none",
    axis.title   = element_blank(),
    axis.text    = element_blank(),
    axis.ticks   = element_blank(),
    plot.margin  = margin(0, 0, 0, 2, "mm")
  )

plot_final_europe


#### CARTE AMERIQUE DU NORD ####

data_am = data_world %>%
  filter(continent == "North America")
region = "North America"
long_limits = c(-162,-50)
lat_limits = c(26, 80.5)

hospitals <- filter(data_am, scale_grp == "Hospital or group of hospitals")
subnational <- filter(data_am, scale_grp == "Subnational")
national <- filter(data_am, scale_grp == "National")

countries_map <- ne_countries(scale = "medium", continent = region, returnclass = "sf") %>%
  filter(sovereignt != "Antarctica", admin != "Greenland") %>%
  mutate(fill_color = ifelse(admin %in% national$country, color_national, "white"))

regions_map <- ne_states(country = unique(subnational$country), returnclass = "sf") %>%
  filter(region %in% subnational$network | 
           name %in% subnational$network |
           region %in% subnational$region[!is.na(subnational$region)] |
           name %in% subnational$region[!is.na(subnational$region)]
  )

# Ajout des régions qui ne sont pas dans regions_map

# Californie
california_state <- rnaturalearth::ne_states(
  country = "United States of America", returnclass = "sf"
) %>%
  dplyr::filter(name == "California") %>%
  sf::st_transform(sf::st_crs(regions_map))

regions_map <- dplyr::bind_rows(
  regions_map %>% dplyr::mutate(network = name),   # garantir colonne
  california_state
) %>% sf::st_as_sf()

# Autres counties
flo_counties <- st_read("tl_2023_12_cousub/tl_2023_12_cousub.shp")
calif_counties <- st_read("ca_counties/CA_Counties.shp")

geom_oc_calif <- calif_counties %>% filter(NAME == "Orange") %>%
  st_transform(st_crs(regions_map)) %>% pull(geometry)

geom_oc_flo <- flo_counties %>% filter(NAME == "Orange") %>%
  st_transform(st_crs(regions_map)) %>% pull(geometry)

central_florida <- flo_counties %>%
  filter(NAME %in% c("Orange", "Seminole", "Osceola", "Polk", "Lake", "Brevard", "Volusia")) %>%
  st_transform(st_crs(regions_map)) %>%
  st_union()

ga_counties <- st_read("tl_2019_13_cousub/tl_2019_13_cousub.shp")
atlanta_metro <- ga_counties %>%
  filter(NAME %in% c("Fulton", "DeKalb", "Cobb", "Gwinnett", "Clayton", "Cherokee", "Douglas", "Fayette", "Henry", "Bartow")) %>%
  st_union() %>%
  st_transform(st_crs(regions_map))

regions_map <- bind_rows(
  regions_map,
  tibble(name = "Orange County, California", geometry = geom_oc_calif),
  tibble(name = "Orange County, Florida", geometry = geom_oc_flo),
  tibble(name = "Central Florida", geometry = central_florida),
  tibble(name = "Atlanta Metropolitan Area", geometry = atlanta_metro)
)

# Labels enrichis
national_unique <- national %>%
  group_by(network, lon, lat) %>%
  summarise(nb_articles_total = sum(nb_articles, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = paste0(network, " (", nb_articles_total, ")"), network)

subnational_unique <- subnational %>%
  group_by(network, lon, lat) %>%
  summarise(nb_articles_total = sum(nb_articles, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = paste0(network, " (", nb_articles_total, ")"), network) %>%
  distinct(network, .keep_all = TRUE)

hospitals_unique <- hospitals %>%
  group_by(network, lon, lat) %>%
  summarise(nb_articles_total = sum(nb_articles, na.rm = TRUE), .groups = "drop") %>%
  mutate(label = paste0(network, " (", nb_articles_total, ")"),network)



labels_all <- bind_rows(
  national_unique   %>% mutate(type = "national",   col = "grey30", face = "italic"),
  subnational_unique %>% mutate(type = "subnational", col = "black",  face = "bold"),
  hospitals_unique   %>% mutate(type = "hospital",  col = "black",  face = "plain")
)

plot_final_am = ggplot() +
  geom_sf(data = countries_map, aes(fill = fill_color), color = "grey") +
  geom_sf(data = regions_map,   fill = color_subnational, color = color_subnational) +
  geom_point(data = hospitals, aes(x = lon, y = lat, size = n),
             color = color_hospital, size = 1.5) +
  scale_fill_identity() +
  scale_x_continuous(limits = long_limits) +
  scale_y_continuous(limits = lat_limits) +
  
  geom_text_repel(
    data = labels_all,
    aes(x = lon, y = lat, label = label,
        color = col, fontface = face),
    size          = 4,
    seed          = 42,
    force         = 1,
    force_pull    = 30,
    box.padding   = 0.5,
    point.padding = 0.5,
    segment.color = "black",
    segment.size  = 0.5,
    max.overlaps  = Inf
  ) +
  scale_color_identity() +
  theme_minimal() +
  theme(
    panel.grid   = element_blank(),
    legend.position = "none",
    axis.title   = element_blank(),
    axis.text    = element_blank(),
    axis.ticks   = element_blank(),
    plot.margin  = margin(0, 0, 0, 2, "mm")
  )

plot_final_am


#### PLOT SCALE ####
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


# en horiz
plot_final_arrange = ggarrange(
  plot_scale,  # Plot A en haut
  ggarrange(
    plot_final_europe, plot_final_am,
    ncol = 2, labels = c("B", "C"), align = "hv"
  ),
  ncol = 1, nrow = 2, labels = c("A", ""),
  heights = c(0.5, 2)  # Réduction de la hauteur du plot_scale
)

ggsave(plot_final_arrange, filename = "figures/maps/maps_scale_eu_am_horiz.png", width = 25, height = 25, dpi = 600, units = "cm")

# en verti
plot_final_arrange <- ggarrange(
  plot_scale,
  plot_final_europe,
  plot_final_am,
  ncol = 1, nrow = 3,
  labels = c("A", "B", "C"),
  align = "v",
  heights = c(0.3, 1, 1)
)
ggsave(plot_final_arrange, filename = "figures/maps/maps_scale_eu_am.png", width = 25, height = 45, dpi = 600, units = "cm")

