fill_bar = "#5e3c99"
fill_hist = "#4c72b0"

### Barplot types of databases combinées (x: articles (87))

grille_extract_modif <- df_networks %>%
  mutate(
    type_database_grp = str_replace_all(type_database_grp, "databases", "database"),
    type_database_grp = str_replace_all(type_database_grp, ",\\s*", " and "),
    type_database_grp = str_replace(
      type_database_grp,
      "Health insurance database and Survey database",
      "Health insurance database\nand Survey database"
    )
  )
  

plot_databases_type_87 = ggplot(grille_extract_modif, aes(x = fct_rev(fct_infreq(type_database_grp)))) +
  geom_bar(fill = "#1F3A93", width = 0.6) +
  scale_y_continuous(
    breaks = seq(0, 80, by = 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Number of articles"
  ) +
  theme_minimal() +
  theme(
    axis.text.x       = element_text(size = 18, color = "black"),
    axis.text.y       = element_text(size = 20, color = "black"),
    axis.title.x      = element_text(size = 20, color = "black"),
    legend.position   = "none",
    panel.grid        = element_blank(),
    axis.line         = element_line(linewidth = 1, color = "black"),
    axis.ticks.x      = element_line(size = 1, color = "black"),
    axis.ticks.length = unit(5, "pt")
  )
plot_databases_type_87

### Barplot types of databases combinées (x: articles (73))

grille_extract_modif <- df_articles %>%
  mutate(
    type_database_grp = str_replace_all(type_database_grp, "databases", "database"),
    type_database_grp = str_replace_all(type_database_grp, ",\\s*", " and "),
    type_database_grp = str_replace(
      type_database_grp,
      "Health insurance database and Survey database",
      "Health insurance database\nand Survey database"
    )
  )

plot_databases_type_73 = ggplot(grille_extract_modif, aes(x = fct_rev(fct_infreq(type_database_grp)))) +
  geom_bar( fill = fill_bar,
            color = "black",
           width = 0.6) +
  scale_y_continuous(
    breaks = seq(0, 80, by = 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Number of articles"
  ) +
  theme_minimal() +
  theme(
    axis.text.x       = element_text(size = 18, color = "black"),
    axis.text.y       = element_text(size = 20, color = "black"),
    axis.title.x      = element_text(size = 20, color = "black"),
    legend.position   = "none",
    panel.grid        = element_blank(),
    axis.line         = element_line(linewidth = 1, color = "black"),
    axis.ticks.x      = element_line(size = 1, color = "black"),
    axis.ticks.length = unit(5, "pt")
  )
plot_databases_type_73


### Barplot types of databases (x: réseau, article avec le plus de types de databases)

nouveau_df <- grille_extract_modif %>%
  arrange(network, desc(nchar(type_database_grp))) %>%
  distinct(network, .keep_all = TRUE)

plot_databases_type_by_network = ggplot(nouveau_df, aes(x = fct_rev(fct_infreq(type_database_grp)))) +
  geom_bar( fill =fill_bar,
            color = "black",
           width = 0.6) +
  scale_y_continuous(
    breaks = seq(0, 80, by = 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Number of networks"
  ) +
  theme_minimal() +
  theme(
    axis.text.x       = element_text(size = 18, color = "black"),
    axis.text.y       = element_text(size = 20, color = "black"),
    axis.title.x      = element_text(size = 20, color = "black"),
    legend.position   = "none",
    panel.grid        = element_blank(),
    axis.line         = element_line(linewidth = 1, color = "black"),
    axis.ticks.x      = element_line(size = 1, color = "black"),
    axis.ticks.length = unit(5, "pt")
  )
plot_databases_type_by_network


### Databases duration (histogram)

df_articles$time_range_database_years <- as.numeric(df_articles$time_range_database_years)
summary(df_articles$time_range_database_years)

hist_time_range_73 <- ggplot(df_articles, aes(x = time_range_database_years)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,         # démarre les bins à 0 → [0,1], (1,2], (2,3], ...
    closed = "right",     # inclut la borne supérieure
    fill = fill_hist,
    color = "black",
  ) +
  labs(
    x = "Database duration (years)",
    y = "Number of articles"
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 1),
    limits = c(0, 10),
    expand = expansion(mult = c(0, 0))   # colle à l’axe Y
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)) # colle à l’axe X, avec un mini espace en haut (2%)
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 20, color = "black"),
    panel.grid = element_blank(),
    axis.line = element_line(size = 1, color = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black")
  )

hist_time_range_73

### Même chose pour les entworks, point = durée max

df_networks$time_range_database_years = as.numeric(df_networks$time_range_database_years)
summary(df_networks$time_range_database_years)

# df_duree_max_par_network <- df_networks %>%
#   filter(
#     !is.na(start_year_database), # car un NA pour Ohst, le papier sur stockholm
#     !is.na(time_range_database_years),
#   ) %>%
#   mutate(end_year = start_year_database + floor(time_range_database_years)) %>% # calcul end_year entier
#   group_by(network) %>%
#   summarise(
#     min_start = min(start_year_database, na.rm = TRUE),
#     max_end   = max(end_year,            na.rm = TRUE),
#     max_dur_years = (max_end - min_start) + 1
#   ) %>%
#   ungroup()
# summary(df_duree_max_par_network$max_dur_years)
# 
df_duree_max_simple <- df_networks %>%
  filter(
    !is.na(time_range_database_years)
  ) %>%
  group_by(network) %>%
  summarise(
    max_dur_years = max(time_range_database_years, na.rm = TRUE)
  ) %>%
  ungroup()
summary(df_duree_max_simple$max_dur_years)


hist_time_range_max_network <- ggplot(df_duree_max_simple, aes(x = max_dur_years)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,         # démarre les bins à 0 → [0,1], (1,2], (2,3], ...
    closed = "right",     # inclut la borne supérieure
    fill = fill_hist,
    color = "black",
  ) +
  labs(
    x = "Database duration (years)",
    y = "Number of articles"
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 1),
    limits = c(0, 10),
    expand = expansion(mult = c(0, 0))   # colle à l’axe Y
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02)) # colle à l’axe X, avec un mini espace en haut (2%)
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 20, color = "black"),
    panel.grid = element_blank(),
    axis.line = element_line(size = 1, color = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black")
  )

hist_time_range_max_network

### Old : boxplot of study duration ###

### Databases duration (x: number of articles 73)

# df_articles$time_range_database_years = as.numeric(df_articles$time_range_database_years)
# summary(df_articles$time_range_database_years)
# 
# bxplt_time_range_73 <- ggplot(df_articles, aes(x = "", y = time_range_database_years)) +  
#   geom_boxplot(fill =  "#7B92C6", color = "black", outlier.shape = NA, width = 0.3) +  # Boxplot plus fin et sans outliers
#   geom_jitter(aes(x = ""), color = "darkred", size = 2, width = 0.1, alpha = 0.6) +  # Ajout des points avec jitter
#   labs(
#     y = "Database duration (years)",
#     x = ""
#   ) +
#   scale_y_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +  # Axe Y de 0 à 10 avec pas de 1
#   theme_minimal() +
#   theme(
#     text = element_text(size = 20, color = "black"),  # Texte en noir
#     panel.grid = element_blank(),  # Supprime la grille
#     axis.line = element_line(size = 1, color = "black"),  # Ajoute des axes X et Y
#     axis.text = element_text(color = "black"),  # Chiffres des axes en noir
#     axis.ticks.y = element_line(color = "black")  # Ticks en noir
#   )
# bxplt_time_range_73
# 
# 
# ### Databases duration (x: number of articles 87)
# 
# df_networks$time_range_database_years = as.numeric(df_networks$time_range_database_years)
# summary(df_networks$time_range_database_years)
# 
# bxplt_time_range_87 <- ggplot(df_networks, aes(x = "", y = time_range_database_years)) +  
#   geom_boxplot(fill =  "#7B92C6", color = "black", outlier.shape = NA, width = 0.3) +  # Boxplot plus fin et sans outliers
#   geom_jitter(aes(x = ""), color = "darkred", size = 2, width = 0.1, alpha = 0.6) +  # Ajout des points avec jitter
#   labs(
#     y = "Database duration (years)",
#     x = ""
#   ) +
#   scale_y_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +  # Axe Y de 0 à 10 avec pas de 1
#   theme_minimal() +
#   theme(
#     text = element_text(size = 20, color = "black"),  # Texte en noir
#     panel.grid = element_blank(),  # Supprime la grille
#     axis.line = element_line(size = 1, color = "black"),  # Ajoute des axes X et Y
#     axis.text = element_text(color = "black"),  # Chiffres des axes en noir
#     axis.ticks.y = element_line(color = "black")  # Ticks en noir
#   )
# bxplt_time_range_87
### Databases duration (maximum time range by network)
# 
# df_duree_max_par_network <- df_networks %>%
#   filter(
#     !is.na(start_year_database), # car un NA pour Ohst, le papier sur stockholm
#     !is.na(time_range_database_years),
#   ) %>%
#   mutate(end_year = start_year_database + floor(time_range_database_years)) %>% # calcul end_year entier
#   group_by(network) %>%
#   summarise(
#     min_start = min(start_year_database, na.rm = TRUE),
#     max_end   = max(end_year,            na.rm = TRUE),
#     max_dur_years = (max_end - min_start) + 1
#   ) %>%
#   ungroup()
# 
# bxplt_time_range_max <- ggplot(df_duree_max_par_network, aes(x = "", y = max_dur_years)) +  
#   geom_boxplot(fill =  "#7B92C6", color = "black", outlier.shape = NA, width = 0.3) +  # Boxplot plus fin et sans outliers
#   geom_jitter(aes(x = ""), color = "darkred", size = 2, width = 0.1, alpha = 0.6) +  # Ajout des points avec jitter
#   labs(
#     y = "Database duration (years)",
#     x = ""
#   ) +
#   scale_y_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +  # Axe Y de 0 à 10 avec pas de 1
#   theme_minimal() +
#   theme(
#     text = element_text(size = 20, color = "black"),  # Texte en noir
#     panel.grid = element_blank(),  # Supprime la grille
#     axis.line = element_line(size = 1, color = "black"),  # Ajoute des axes X et Y
#     axis.text = element_text(color = "black"),  # Chiffres des axes en noir
#     axis.ticks.y = element_line(color = "black")  # Ticks en noir
#   )
# bxplt_time_range_max

# Figure arrangée par combinaison article/réseau
# database_type_time_range = ggarrange(plot_databases_type_73, bxplt_time_range_73, nrow = 1, ncol = 2, labels = c("A", "B"), widths = c(3,1))
# ggsave(database_type_time_range, filename = "figures/database_type_duration/database_type_duration_by_article_73.png", width = 40, height = 15, dpi=600, units="cm")
# 
# database_type_time_range = ggarrange(plot_databases_type_87, bxplt_time_range_87, nrow = 1, ncol = 2, labels = c("A", "B"), widths = c(3,1))
# ggsave(database_type_time_range, filename = "figures/database_type_duration/database_type_duration_by_article_87.png", width = 40, height = 15, dpi=600, units="cm")
# 
# database_type_time_range_by_network = ggarrange(plot_databases_type_by_network, bxplt_time_range_max, nrow = 1, ncol = 2, labels = c("A", "B"), widths = c(3,1))
# ggsave(database_type_time_range_by_network, filename = "figures/database_type_duration/database_type_duration_by_network.png", width = 40, height = 15, dpi=600, units="cm")



# Arrangement avec histo

empty_space <- ggplot() + theme_void()
database_type_time_range_by_network <- ggarrange(
  plot_databases_type_by_network,
  hist_time_range_max_network,
  empty_space,                           # espace vide
  nrow = 1, ncol = 3,
  widths = c(1, 1, 0.02),                # 0.02 = ~2% de marge à droite
  labels = c("A", "B", "")
)

ggsave(database_type_time_range_by_network, filename = "figures/database_type_duration/database_type_duration_by_network.png", width = 40, height = 15, dpi=600, units="cm")


database_type_time_range_by_network = ggarrange(plot_databases_type_73, 
                                                hist_time_range_73, 
                                                empty_space,                           # espace vide
                                                nrow = 1, ncol = 3,
                                                labels = c("A", "B"), 
                                                widths = c(1,1,0.02))
ggsave(database_type_time_range_by_network, filename = "figures/database_type_duration/database_type_duration_by_article.png", width = 40, height = 15, dpi=600, units="cm")









##### Par article ####

### Barplot types of databases séparées (x: articles)
# 
# type_database_vector <- unlist(strsplit(as.character(grille_extract$type_database_grp), split = ", "))
# 
# type_database_counts_grouped <- as.data.frame(table(type_database_vector))
# colnames(type_database_counts_grouped) <- c("type_database_grp", "count")
# 
# plot_databases_type = ggplot(type_database_counts_grouped, aes(y = reorder(type_database_grp, count), x = count)) +
#   geom_bar(stat = "identity", fill = "#1ABC9C", width = 0.6) +
#   theme_minimal() +
#   labs(x = "Number of articles", y = "") +
#   scale_x_continuous(breaks = seq(0, 70, by = 5), expand = expansion(mult = c(0, 0.05))) +  # Commence à 0
#   theme(
#     axis.text.x = element_text(size = 18, color = "black"),  
#     axis.text.y = element_text(size = 20, color = "black"),
#     axis.title.x = element_text(size = 20, color = "black"),
#     legend.position = "none",
#     panel.grid = element_blank(),  # Supprime les grilles
#     axis.line = element_line(size = 1, color = "black"),  # Ajoute des axes X et Y
#     axis.ticks.x = element_line(size = 1, color = "black"),  # Ajoute des ticks sur l'axe X
#     axis.ticks.length = unit(5, "pt")  # Définit la longueur des ticks (ici 5 points)
#   )
# plot_databases_type