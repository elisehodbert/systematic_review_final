gen_world_map <- function(data_region, long_limits = NA, lat_limits = NA){
  # Séparer catégories
  hospitals <- filter(data_region, scale_grp == "Hospital or group of hospitals")
  subnational <- filter(data_region, scale_grp == "Subnational")
  national <- filter(data_region, scale_grp == "National")
  
  # Cartes de base
  countries_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(sovereignt != "Antarctica") %>%
    mutate(fill_color = ifelse(admin %in% national$country, color_national, "white"))
  
  regions_map <- ne_states(country = unique(subnational$country), returnclass = "sf") %>%
    filter(region %in% subnational$network | 
             name %in% subnational$network |
             region %in% subnational$region[!is.na(subnational$region)] |
             name %in% subnational$region[!is.na(subnational$region)]
           )
  
  # Construction de la carte
  ggplot() +
    geom_sf(data = countries_map, aes(fill = fill_color), color = "grey") +
    scale_fill_identity() +
    geom_sf(data = regions_map, fill = color_subnational, color = "grey") +
    geom_point(data = hospitals, aes(x = lon, y = lat, size = n), 
               color = color_hospital, size = 1.5) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "cm")
    )
}

gen_region_map <- function(data_region, region, long_limits = NA, lat_limits = NA) {
  hospitals <- filter(data_region, scale_grp == "Hospital or group of hospitals")
  subnational <- filter(data_region, scale_grp == "Subnational")
  national <- filter(data_region, scale_grp == "National")
  
  countries_map <- ne_countries(scale = "medium", continent = region, returnclass = "sf") %>%
    filter(sovereignt != "Antarctica", admin != "Greenland") %>%
    mutate(fill_color = ifelse(admin %in% national$country, color_national, "white"))
  
  regions_map <- ne_states(country = unique(subnational$country), returnclass = "sf") %>%
    filter(region %in% subnational$network | 
             name %in% subnational$network |
             region %in% subnational$region[!is.na(subnational$region)] |
             name %in% subnational$region[!is.na(subnational$region)]
    )
  
  regions_map <- ne_states(country = unique(subnational$country), returnclass = "sf") %>%
    filter(region %in% subnational$network | 
             name %in% subnational$network
    )
  
  if (region == "North America") { # ajout des counties qui ne sont pas directement dans regions_map
# ajout californie
    california_state <- rnaturalearth::ne_states(
      country = "United States of America", returnclass = "sf"
    ) %>%
      dplyr::filter(name == "California") %>%
      sf::st_transform(sf::st_crs(regions_map))
    
    regions_map <- dplyr::bind_rows(
      regions_map %>% dplyr::mutate(network = name),   # garantir colonne
      california_state
    ) %>% sf::st_as_sf()
    
    # ajout counties
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
  }
  
  if(region=="Europe"){ # ajout des régions qui ne sont pas dans regions_map
    uk_regions <- ne_states(country = "United Kingdom", returnclass = "sf") %>%
      filter(geonunit %in% c("England", "Scotland"))
    # ajout saxony and thuringia
    saxonythuringia <- ne_states(country = "Germany", returnclass = "sf") %>%
      filter(name_alt %in% c("Saxony", "Thuringia"))
    westerngreece <- ne_states(country="Greece", returnclass="sf") %>%
      filter(name_alt == "Greece West")
    regions_map <- bind_rows(regions_map,uk_regions, saxonythuringia, westerngreece)
  }
  
  # Labels enrichis
  national_unique <- national %>%
    group_by(network, lon, lat) %>%
    summarise(nb_articles_total = sum(nb_articles, na.rm = TRUE), .groups = "drop") %>%
    mutate(label = paste0(network, " (", nb_articles_total, ")"), network)
  
  subnational_unique <- subnational %>%
    group_by(network, lon, lat) %>%
    summarise(nb_articles_total = sum(nb_articles, na.rm = TRUE), .groups = "drop") %>%
    mutate(# ajout coordonnées atlanta car ne fonctionne pas
      lon = ifelse(network == "Atlanta Metropolitan Area", -84.3880, lon),
      lat = ifelse(network == "Atlanta Metropolitan Area", 33.7490, lat),
      label = paste0(network, " (", nb_articles_total, ")")
    ) %>% 
    mutate(label = paste0(network, " (", nb_articles_total, ")"), network)
  
  hospitals_unique <- hospitals %>%
    group_by(network, lon, lat) %>%
    summarise(nb_articles_total = sum(nb_articles, na.rm = TRUE), .groups = "drop") %>%
    mutate(label = paste0(network, " (", nb_articles_total, ")"),network)
  
  ggplot() +
    # Cartes
    geom_sf(data = countries_map, aes(fill = fill_color), color = "grey") +
    geom_sf(data = regions_map, fill = color_subnational, color = color_subnational) +
    geom_point(data = hospitals, aes(x = lon, y = lat, size = n), 
               color = color_hospital, size = 1.5) +
    scale_fill_identity() +
    
    # on remplace scale_x/_y par coord_sf(expand=TRUE, clip="off")
    #coord_sf(expand = TRUE, clip = "off") +
    
    # Coupe aux limites
    scale_x_continuous(limits = long_limits) +
    scale_y_continuous(limits = lat_limits) +
    # Textes
    geom_text_repel(data = national_unique, 
                    aes(x = lon, y = lat, label = label),
                    color = "grey30", 
                    fontface = "italic", 
                    size = 3, 
                    seed = 42,
                    force = 2, 
                    box.padding = 0.5, 
                    point.padding = 0.5,
                    segment.color = "black", 
                    segment.size = 0.5, 
                    max.overlaps = Inf) +
    geom_text_repel(data = subnational_unique, 
                    aes(x = lon, y = lat, label = label),
                    color = "black", 
                    fontface = "bold", 
                    size = 3,
                    seed = 42,
                    force = 30, 
                    box.padding = 0.5, 
                    point.padding = 0.5,
                    segment.color = "black", 
                    segment.size = 0.5, 
                    max.overlaps = Inf) +
    geom_text_repel(data          = hospitals_unique,
                    aes(lon, lat, label = label),
                    color         = "black",
                    size          = 3,
                    seed          = 42,
                    force         = 0,
                    vjust=1.5,
                    segment.color = "grey50",
                    segment.size  = 0.5,
                    max.overlaps  = Inf) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      legend.position = "none",
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.margin    = margin(0, 0, 0, 2, "mm")
    )
}
