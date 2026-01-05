# Import packages
pacman::p_load(
  openxlsx, tidyverse, ggpubr, colorspace, scales
)

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

# 
# # 1. Fonctions de recodage
# recode_pathogen <- function(x) {
#   recode(
#     x,
#     "MRSA"                                       = "S. aureus",
#     "ESBL K. pneumoniae"                         = "K. pneumoniae",
#     "Carbapenem-Resistant Enterobacterales"      = "Enterobacterales",
#     "MDR Enterobacterales"                       = "Enterobacterales",
#     "Vancomycin-Resistant Enterococci"           = "Enterococci",
#     "Multiple (CRE, MRSA, MDR P. aeruginosa, C. difficile)" = "Multiple",
#     .default = x
#   )
# }
# 
# # 2. Lecture et pré‐traitement des articles
# df_articles <- read.xlsx(
#   "grille_extract_test.xlsx"
# ) %>%
#   filter(!str_detect(author_year, "_sub_")) %>%
#   mutate(
#     transfer_type        = factor(transfer_type, levels = c("Direct", "Indirect")),
#     across(c(nb_transfers, nb_facilities, nb_units, time_range_database_years), as.numeric),
#     prop_model_pathogen  = recode_pathogen(prop_model_pathogen)
#   ) %>%
#   mutate(
#     static_or_temporal = case_when(
#       if_temporal_tempscale == "Yearly" ~ "Static",
#       TRUE                              ~ as.character(static_or_temporal)),
#       if_static_agr_or_avg = if_else(if_temporal_tempscale == "Yearly", "Aggregated", if_static_agr_or_avg),
#     if_temporal_tempscale = if_else(if_temporal_tempscale == "Yearly", NA_character_, if_temporal_tempscale)
#   ) %>%
#   mutate(static_or_temporal = factor(static_or_temporal, levels = c("Static", "Temporal"))) %>%
#   mutate(nb_transfers_year = nb_transfers / time_range_database_years)


# 3. Lecture et pré‐traitement des réseaux
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

# 4. Définition des formes et palettes

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

# Fonction pour générer les couleurs
# generate_distinct_colors <- function(n = 25, min_dist = 30, seed = 456) {
#   if (!is.null(seed)) set.seed(seed)
#   h_vals <- c(); c_vals <- c(); l_vals <- c()
#   cols    <- character(0)
#   
#   while (length(cols) < n) {
#     h0 <- runif(1, 0, 360)
#     c0 <- runif(1, 20, 80)
#     l0 <- runif(1, 30, 90)
#     
#     if (length(cols) > 0) {
#       dh <- abs(h0 - h_vals)
#       dh <- pmin(dh, 360 - dh)  
#       dc <- c0 - c_vals
#       dl <- l0 - l_vals
#       dists <- sqrt(dh^2 + dc^2 + dl^2)
#       if (min(dists) < min_dist) next
#     }
#     h_vals <- c(h_vals, h0)
#     c_vals <- c(c_vals, c0)
#     l_vals <- c(l_vals, l0)
#     cols    <- c(cols, hcl(h = h0, c = c0, l = l0, fixup = TRUE))
#   }
#   cols
# }

# Exemple et visualisation
# palette <- generate_distinct_colors(n = 25, min_dist = 40, seed = 123)
# barplot(
#   rep(1, length(palette)),
#   col    = palette,
#   border = NA,
#   space  = 0,
#   axes   = FALSE,
#   main   = "25 couleurs éloignées les unes des autres"
# )



# 5. Construction de network_info avec couleurs et formes
# 5. Construction de network_info avec couleurs et formes

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
  main   = paste0("Europe (", length(v1), " couleurs équidistantes via rainbow_hcl)"),
  axes   = FALSE
)

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
  main   = paste0("NA(", length(v2), " couleurs équidistantes via rainbow_hcl)"),
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


# 6. Sauvegarde pour réutilisation
save(network_info, file = "network_info.RData")
#load("network_info.RData")

# 7. Vecteurs nommés pour ggplot2
network_shapes <- setNames(network_info$shape, network_info$network)
network_colors <- setNames(network_info$color, network_info$network)

# 9. Aperçu visuel pour l’Europe

eu_colors <- network_info %>%
  filter(continent == "Europe") %>%
  pull(color)
stopifnot(
  length(eu_colors) == n_distinct(
    df_networks$network[df_networks$continent == "Europe"]
  )
)

n_eu <- length(eu_colors)
barplot(
  rep(1, n_eu),
  col    = eu_colors,
  border = NA,
  main   = paste0("Europe (", n_eu, " couleurs)"),
  axes   = FALSE
)

# Amérique du Nord
na_colors <- network_info %>% filter(continent == "North America") %>% pull(color)
barplot(
  rep(1, length(na_colors)),
  col    = na_colors,
  border = NA,
  main   = paste0("Amérique du Nord (", length(na_colors), " couleurs)"),
  axes   = FALSE
)




### Arrangement des transfer_type et max_delay

summary(df_networks$transfer_type)

df_articles$indirect_transfer_max_delay <- as.factor(df_articles$indirect_transfer_max_delay)
summary(df_articles$indirect_transfer_max_delay)

df_networks$indirect_transfer_max_delay <- as.factor(df_networks$indirect_transfer_max_delay)
summary(df_networks$indirect_transfer_max_delay)

df_networks$autotransfers_included = as.factor(df_networks$autotransfers_included)
summary(df_networks$autotransfers_included)

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

df_networks <- df_networks %>%
  mutate(
    max_delay_arrangé = factor(case_when(
      transfer_type == "Direct"                                                       ~ "0",
      indirect_transfer_max_delay == "Any time within the period covered by the data" ~ as.character(time_range_database_years),
      TRUE                                                                             ~ as.character(indirect_transfer_max_delay)
    ))
  ) 

df_networks %>%
  count(max_delay_arrangé)
# 90 days et 0.25 years: même chose



# pour une seule valeur par network

df_clean <- df_networks %>%
  mutate(
    max_delay_arrangé = trimws(max_delay_arrangé),
    max_delay_arrangé = na_if(max_delay_arrangé, ""),
    # passer "Non spécifié"/"Not specified" (avec/ss accents) à NA
    .tmp_lower = str_to_lower(max_delay_arrangé),
    max_delay_arrangé = ifelse(.tmp_lower %in% c("non spécifié","non specifie","not specified"), NA, max_delay_arrangé)
  ) %>%
  select(-.tmp_lower)

# 2) Fonction de conversion -> années (numérique)
to_years <- function(x) {
  if (is.na(x)) return(NA_real_)
  x_trim <- str_trim(x)
  
  if (str_detect(x_trim, regex("year", ignore_case = TRUE))) {
    # "1 year", "1.5 years"
    as.numeric(str_extract(x_trim, "\\d+\\.?\\d*"))
  } else if (str_detect(x_trim, regex("day", ignore_case = TRUE))) {
    # "90 days" -> 90/365
    as.numeric(str_extract(x_trim, "\\d+\\.?\\d*")) / 365
  } else if (str_detect(x_trim, "^[0-9]+\\.?[0-9]*$")) {
    # nombre nu => années
    as.numeric(x_trim)
  } else {
    NA_real_
  }
}

# 3) Calculer la valeur numérique et sélectionner la meilleure ligne par réseau
df_networks_unique <- df_clean %>%
  mutate(delay_years = vapply(max_delay_arrangé, to_years, numeric(1))) %>%
  group_by(network) %>%
  arrange(desc(!is.na(delay_years)), desc(delay_years), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-delay_years)

df_networks_unique %>% 
  count(max_delay_arrangé) 

df_articles <- df_articles %>%
  mutate(
    max_delay_arrangé = factor(case_when(
      transfer_type == "Direct"                                                       ~ "0",
      indirect_transfer_max_delay == "Any time within the period covered by the data" ~ as.character(time_range_database_years),
      TRUE                                                                             ~ as.character(indirect_transfer_max_delay)
    ))
  ) 

df_articles %>%
  count(max_delay_arrangé)


#
summary(as.factor(df_networks$autotransfers_included))
table(df_networks$network, df_networks$autotransfers_included)
