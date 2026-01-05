### Analyse des métriques pour décrire les réseaux ###

dico_metrics <- read.xlsx("dico_metrics.xlsx")

# Nombre de lignes avec des NA
na_count <- sum(is.na(df_articles$descriptive.metrics_grp))
cat("Nombre de lignes avec NA:", na_count, "\n")

metrics_list <- df_articles$descriptive.metrics_grp

# 1. Split each string into individual metrics and trim whitespace
split_metrics_raw <- strsplit(metrics_list, ",\\s*")
split_metrics_raw <- lapply(split_metrics_raw, trimws)

# On compte les articles qui ont utilisé une méthode de détection de communauté
n_community <- sum(sapply(split_metrics_raw, function(x) sum(x %in% c("community detection", "clustering", "community-node sizes"), na.rm = TRUE)))
n_community

# On enlève les community detection methods

split_metrics <- lapply(split_metrics_raw, function(x) {
  # Filtrer les termes exacts
  x <- x[!x %in% c("community detection", "clustering", "community-node sizes")]
  
  # Remplacements exacts
  x[x == "betweenness centrality"] <- "betweenness"
  x[x == "number of ties"] <- "number of edges"
  x[x == "transfer volume"] <- "weighted degree"
  
  x[x == "shortest path (Floyd-Warshall algorithm)"] <- "geodesic distance"
  x[x == "shortest path"] <- "geodesic distance"
  x[x == "shortest connecting path"] <- "geodesic distance"
  x[x == "shortest path distance"] <- "geodesic distance"
  x[x == "geodesic distance distance"] <- "geodesic distance"
  
  x[x == "average path length"] <- "average geodesic distance"
  x[x == "average shortest path length"] <- "average geodesic distance"
  
  x[x == "transitivity"] <- "local clustering coefficient"
  x[x == "global transitivity"] <- "global clustering coefficient"
  x[x == "average betweenness centrality"] <- "average betweenness"
  x[x == "PageRank"] <- "PageRank centrality"
  
  x[x == "weighted degree"] <- "node strength"
  x[x == "out-degree centrality"] <- "out-degree"
  x[x == "in-degree centrality"] <- "in-degree"
  x[x == "average in-degree centrality"] <- "average in-degree"
  x[x == "average out-degree centrality"] <- "average out-degree"
  
  x[x == "in-intensity"] <- "weighted in-degree"
  x[x == "out-intensity"] <- "weighted out-degree"
  x[x == "average degree centrality"] <- "average degree"
  
  x[x == "weighted edge density"] <- "weighted density"
  x[x == "unweighted betweenness"] <- "betweenness"
  x[x == "transitive ties"] <- "cyclic closure"

  x[x == "weighted strength"] <- "node strength"
  x[x == "strength"] <- "node strength"
  x[x == "average network node strength"] <- "average network strength"
  x[x == "in-node strength"] <- "in-strength"
  x[x == "out-node strength"] <- "out-strength"
  
  x[x == "recent receiving"] <- "receiving balance"
  x[x == "recent sending"] <- "sending balance"
  x[x == "closeness centrality"] <- "average closeness"
  x[x == "average closeness centrality"] <- "closeness"
  x[x == "connectivity"] <- "node connectivity"
  x[x == "multiple node connectivity"] <- "node connectivity"
  x[x == "network modularity"] <- "modularity"
  
  return(x)
})


# 2. Calculate the count statistics for metrics per group
# metrics_count <- sapply(split_metrics, length)
# mean_metrics <- mean(metrics_count)
# median_metrics <- median(metrics_count)
# sd_metrics <- sd(metrics_count)  # Standard deviation
# 
# cat("Mean number of metrics per group:", mean_metrics, "\n")
# cat("Median number of metrics per group:", median_metrics, "\n")
# cat("Standard deviation of metrics per group:", sd_metrics, "\n")

# 3. Flatten the list to get all metrics and calculate frequency
all_metrics <- tolower(unlist(split_metrics))  # Convert to lowercase for consistency
metric_frequency <- sort(table(all_metrics), decreasing = TRUE)

metric_df <- as.data.frame(metric_frequency)

metric_df <- metric_df %>%
  mutate(variation_lower = tolower(all_metrics))

dico_metrics <- dico_metrics %>%
  mutate(variation_lower = tolower(variation))

# Mettre tout en minuscules pour comparaison
metric_df <- metric_df %>%
  mutate(variation_lower = tolower(all_metrics))

dico_metrics <- dico_metrics %>%
  mutate(variation_lower = tolower(variation))

dico_joined <- left_join(dico_metrics, metric_df, by = "variation_lower") %>%
  rename(frequency = Freq) %>%
  mutate(précisions = if_else(!is.na(précisions), "yes", NA_character_))

tab_freq <- bind_rows(dico_joined %>% select(level, measure, variation, frequency, précisions))
tab_freq <- tab_freq %>% 
  filter(!is.na(frequency)) %>%
  mutate(level = ifelse(level == "Aggregate network-level measures", "Network-level", level))

write.xlsx(tab_freq,"freq_metrics.xlsx")

# nombre de métriques distinctes dans chaque catégorie
# barplot_distinct_categ = ggplot(
#   tab_freq %>%
#     group_by(level) %>%
#     summarise(n_metrics = n_distinct(variation)) %>%
#     ungroup(),
#   aes(x = reorder(level, n_metrics), y = n_metrics)
# ) +
#   geom_bar(stat = "identity", fill = "#4575b4") +
#   labs(x = "", y = "Number of distinct metrics") +
#   coord_flip() +
#   scale_y_continuous(limits= c(0, 35), breaks = seq(0, 35, by = 5), expand = expansion(mult = c(0, 0.05))) +
# theme_minimal()+
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
# barplot_distinct_categ
# ggsave(barplot_distinct_categ, filename = "figures/barplot_distinct_categ.pdf", width = 30, height = 15, dpi = 600, units = "cm")  # High-resolution 600 dpi


# fréquence d'utilisation de chaque grande catégorie de métrique
# barplot_freq_categ = ggplot(
#   tab_freq %>%
#     group_by(level) %>%
#     summarise(freq_total = sum(frequency, na.rm = TRUE)) %>%
#     ungroup(),
#   aes(x = reorder(level, freq_total), y = freq_total)
# ) +
#   geom_bar(stat = "identity", fill = "#4575b4") +
#   labs(x = "", y = "Number of uses of a metric that belongs to this category") +
#   coord_flip() +
#   scale_y_continuous(limits= c(0, 140), breaks = seq(0, 140, by = 20), expand = expansion(mult = c(0, 0.05))) +
#   theme_minimal()+
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
# 
# barplot_freq_categ
# ggsave(barplot_freq_categ, filename = "figures/barplot_freq_categ.pdf", width = 30, height = 15, dpi = 600, units = "cm")  # High-resolution 600 dpi


# Graphe
top10 <- tab_freq %>%
  arrange(desc(frequency)) %>%
  slice_head(n = 69) %>%
  mutate(variation = str_to_title(variation))  # Chaque mot commence par une majuscule

# Treemap
library(treemap)
treemap(
  top10,
  index = c("level", "variation"),  # hiérarchie : 1er = parent, 2e = sous-catégorie
  vSize = "frequency",              # taille des rectangles = fréquence
  vColor = "frequency",             # couleur selon la même variable (optionnel)
  type = "value",                   # coloration selon la valeur numérique
  palette = "Blues",                # palette de couleurs
  title = "Treemap des variations par niveau"
)

library(treemapify)
library(RColorBrewer)

palette_levels <- brewer.pal(n = 4, name = "Set2")

ggplot(top10, aes(area = frequency, 
               fill = factor(level),  # couleur par catégorie principale
               label = variation, 
               subgroup = level)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 1.5) +
  geom_treemap_text(colour = "white", place = "centre", reflow = TRUE) +
  scale_fill_manual(values = palette_levels, name = "Level") +
  theme_minimal() +
  theme(legend.position = "bottom")




# version avec une palette de couleurs par catégorie
palette_levels <- brewer.pal(n = 4, name = "Set2")

base_colors <- c(
  "Node-level" = palette_levels[1],  # bleu
  "Network-level" = palette_levels[2],  # vert
  "Dyad-level" = palette_levels[3],  # rouge
  "Triad-level" = palette_levels[4]   # violet
)

# on crée les variations de couleurs 
library(colorspace)  # pour lighten()

top10 <- top10 %>%
  group_by(level) %>%
  mutate(
    freq_scaled = rescale(frequency),  # normalisation 0–1
    fill_color = rgb(
      colorRamp(
        c(
          lighten(base_colors[as.character(unique(level))], amount = 0.9),  # version claire
          base_colors[as.character(unique(level))]                          # couleur normale
        )
      )(freq_scaled),
      maxColorValue = 255
    )
  ) %>%
  ungroup()

# on trace le graphe

ggplot(top10, aes(
  area = frequency,
  fill = fill_color,
  label = variation,
  subgroup = level
)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "black", size = 2) +
  geom_treemap_text(colour = "black", place = "centre", reflow = TRUE) +
  scale_fill_identity() +  # on utilise directement les couleurs calculées
  theme_minimal() +
  theme(legend.position = "none")

# on trace la légende


# # Barplot avec ggplot
# ggplot(top10, aes(x = reorder(variation, frequency), 
#                   y = frequency, 
#                   fill = level)) +
#   geom_col() +
#   coord_flip() +
#   labs(
#     x = "Metric",
#     y = "Number of occurrences",
#     fill = "Metric Level"
#   ) +
#   # Axe borné à 0–30 et sans espace inutile
#   scale_y_continuous(limits = c(0,30), expand = c(0,0)) +
#   # Couleurs personnalisées
#   scale_fill_manual(values = c(
#     "Node-level"     = "#1f77b4",  # bleu
#     "Dyad-level"     = "#d95f02",  # orange
#     "Triad-level"    = "#2ca02c",  # vert
#     "Network-level"  = "#9467bd"   # violet
#   )) +
#   theme_classic(base_size = 14) +
#   theme(
#     axis.text = element_text(color = "black")
#   )
# 
# ggsave("figures/barplot_top15_metrics.png", 
#        dpi = 600,        # très haute définition
#        width = 7,        # largeur en pouces
#        height = 8,       # hauteur en pouces
#        units = "in")     # unités
# 
# 
