# Import packages
pacman::p_load(
  openxlsx, 
  writexl,
  tidyverse, 
  ggpubr, 
  colorspace, 
  scales,
  sf,
  rnaturalearth, 
  rnaturalearthhires,   # cartes et régions détaillées
  ggspatial, 
  tidygeocoder,
  ggrepel, 
  ggpubr, 
  tigris,
  ggforce,
  ggnewscale,
  officer,
  rvg,
  patchwork,
  grid,
  scales,
  colorspace,
  RColorBrewer,
  treemapify,
  packcircles
)

# Creation of df_articles (one line per article)
df_articles <- read.xlsx("grille_extract_test.xlsx") %>%
  filter(!str_detect(author_year, "_sub_")) %>%
  mutate(
    transfer_type = factor(transfer_type, levels = c("Direct", "Indirect")),
    across(c(nb_transfers, nb_facilities, nb_units, time_range_database_years), as.numeric),
    prop_model_pathogen = recode(
      prop_model_pathogen,
      "MRSA"                                       = "S. aureus",
      "ESBL K. pneumoniae"                         = "K. pneumoniae",
      "Carbapenem-Resistant Enterobacterales"      = "Enterobacterales",
      "MDR Enterobacterales"                       = "Enterobacterales",
      "Vancomycin-Resistant Enterococci"           = "Enterococci",
      "Multiple (CRE, MRSA, MDR P. aeruginosa, C. difficile)" = "Multiple",
      .default = prop_model_pathogen
    ),
    static_or_temporal = case_when(
      if_temporal_tempscale == "Yearly" ~ "Static",
      TRUE ~ as.character(static_or_temporal)
    ),
    if_static_agr_or_avg = if_else(
      if_temporal_tempscale == "Yearly",
      "Aggregated",
      if_static_agr_or_avg
    ),
    if_temporal_tempscale = if_else(
      if_temporal_tempscale == "Yearly",
      NA_character_,
      if_temporal_tempscale
    ),
    static_or_temporal = factor(static_or_temporal, levels = c("Static", "Temporal")),
    nb_transfers_year = nb_transfers / time_range_database_years
  )


# Creation of df_networks (one line per network presented)
df_networks <- read.xlsx(
  "grille_extract_test.xlsx") %>%
  filter(network != "Not a network") %>%
  mutate(
    across(c(nb_facilities, nb_units, nb_transfers, time_range_database_years), as.numeric),
    nb_transfers_year = nb_transfers / time_range_database_years,
    transfer_type     = factor(transfer_type, levels = c("Direct", "Indirect")),
    scale_grp         = factor(scale_grp),
    start_year_database = as.numeric(start_year_database)
  )

# Definition of parameters: colors and shapes
color_national = "#A8C5DA"
color_subnational = "#B8D5B8"
color_hospital = "#F47C57"

continent_shapes <- c(
  Europe          = 16,
  Asia            = 17,
  `North America` = 15,
  Oceania         = 18
)

continent_palettes <- c(
  Asia            = "Set 2",
  Oceania         = "Pastel 1"
)

# Colors of European networks
v1 = c("black","grey50", "grey80",
       "aquamarine", "aquamarine4",
       "blue", "blue4","cornflowerblue",
       "burlywood1","burlywood4",
       "tomato", "brown4",
       #"cyan1",
       "mediumorchid1", "mediumorchid4",
       "lightsalmon", 
       "red",
       "palevioletred", "deeppink",
       "darkgreen", "olivedrab",
       "palegreen1","mediumseagreen",
       "magenta", "plum", 
       "darkorange", "gold")

barplot(
  rep(1, length(v1)),
  col    = v1,
  border = NA,
  main   = paste0("Europe (", length(v1), " colors)"),
  axes   = FALSE
)

# North American colors
v2 <- c("tomato",       
        "dodgerblue",   
        "blue",
        "mediumseagreen", 
        "magenta",       
        "gold",         
        "darkorange",  
        "turquoise",    
        "grey",    
        "slateblue",   
        "olivedrab",   
        "indianred",   
        "deepskyblue",  
        "mediumvioletred", 
        "burlywood",   
        "black",
        "green",
        "red",
        "darkred"
        ) 

barplot(
  rep(1, length(v2)),
  col    = v2,
  border = NA,
  main   = paste0("North America(", length(v2), " colors)"),
  axes   = FALSE
)

network_info <- df_networks %>%
  distinct(network, continent) %>%
  arrange(continent, network) %>%
  group_by(continent) %>%
  mutate(
    shape = continent_shapes[continent],
    color = if (continent[1]=="Europe") {
      set.seed(260312)
      sample(v1,length(v1))
} else if (continent[1]=="North America") {
  set.seed(260312)
  sample(v2,length(v2))
  } else {
      qualitative_hcl(n(), palette = continent_palettes[[continent[1]]])
    }
  ) %>%
  ungroup()

save(network_info, file = "network_info.RData")
#load("network_info.RData")

network_shapes <- setNames(network_info$shape, network_info$network)
network_colors <- setNames(network_info$color, network_info$network)

### Preprocessing the datasets

# Changing character columns to factors
df_articles <- df_articles %>%
  mutate(indirect_transfer_max_delay = as.factor(indirect_transfer_max_delay))

df_networks <- df_networks %>%
  mutate(
    indirect_transfer_max_delay = as.factor(indirect_transfer_max_delay),
    autotransfers_included      = as.factor(autotransfers_included)
  )

# Creating transfer_type_arrangé
df_articles <- df_articles %>%
  mutate(
    transfer_type_arrangé = case_when(
      transfer_type == "Direct"                ~ "Direct",
      autotransfers_included == "Excluded"     ~ "Indirect, no autotransfers",
      autotransfers_included == "Included"     ~ "Indirect, with autotransfers",
      TRUE                                     ~ NA_character_
    )
  )

df_networks <- df_networks %>%
  mutate(
    transfer_type_arrangé = case_when(
      transfer_type == "Direct"                ~ "Direct",
      autotransfers_included == "Excluded"     ~ "Indirect, no autotransfers",
      autotransfers_included == "Included"     ~ "Indirect, with autotransfers",
      TRUE                                     ~ NA_character_
    )
  )

summary(as.factor(df_networks$transfer_type_arrangé))

# Creating max_delay_arrangé
df_networks <- df_networks %>%
  mutate(
    max_delay_arrangé = factor(case_when(
      transfer_type == "Direct"                                                       ~ "0",
      indirect_transfer_max_delay == "Any time within the period covered by the data" ~ as.character(time_range_database_years),
      TRUE                                                                             ~ as.character(indirect_transfer_max_delay)
    ))
  ) 

df_networks %>% count(max_delay_arrangé) # 90 days and 0.25 years are the same value

# Creation of df_networks_unique (one value per network)
df_clean <- df_networks %>%
  mutate(
    max_delay_arrangé = trimws(max_delay_arrangé),
    max_delay_arrangé = na_if(max_delay_arrangé, ""),
    .tmp_lower = str_to_lower(max_delay_arrangé),
    max_delay_arrangé = ifelse(
      .tmp_lower %in% c("non spécifié","non specifie","not specified"), 
      NA, 
      max_delay_arrangé
    )
  ) %>%
  select(-.tmp_lower)

df_networks_unique <- df_clean %>%
  mutate(
    delay_years = case_when(
      is.na(max_delay_arrangé) ~ NA_real_,
      str_detect(max_delay_arrangé, regex("day",  ignore_case = TRUE)) ~
        as.numeric(str_extract(max_delay_arrangé, "\\d+\\.?\\d*")) / 365,
      str_detect(max_delay_arrangé, regex("year", ignore_case = TRUE)) ~
        as.numeric(str_extract(max_delay_arrangé, "\\d+\\.?\\d*")),
      str_detect(max_delay_arrangé, "^[0-9]+\\.?[0-9]*$") ~
        as.numeric(max_delay_arrangé),
      TRUE ~ NA_real_
    )
  ) %>%
  group_by(network) %>%
  arrange(desc(!is.na(delay_years)), desc(delay_years), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-delay_years)

df_networks_unique %>% count(max_delay_arrangé) 

df_articles <- df_articles %>%
  mutate(
    max_delay_arrangé = factor(case_when(
      transfer_type == "Direct"                                                       ~ "0",
      indirect_transfer_max_delay == "Any time within the period covered by the data" ~ as.character(time_range_database_years),
      TRUE                                                                             ~ as.character(indirect_transfer_max_delay)
    ))
  ) 

df_articles %>% count(max_delay_arrangé)
