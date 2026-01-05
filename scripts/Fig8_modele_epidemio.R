pacman::p_load(openxlsx,
               tidyverse,
               packcircles,
               ggplot2,
               ggforce,
               dplyr,
               officer,
               rvg
               )

# à reréfléchir si on fait par article ou par network.
summary(as.factor(df_articles$prop_model_yes_no))

### Au moins un modèle pour le network? ###
résumé <- df_networks %>%
  group_by(network) %>%
  summarize(au_moins_un_yes = any(prop_model_yes_no == "yes"), .groups = "drop")
summary(résumé$au_moins_un_yes)

### Pathogènes ###
summary(as.factor(df_articles$prop_model_pathogen))

## Epidemio data? ###
summary(as.factor(df_articles$epidemio_data_yes_no))

### Types de modèles ###
summary(as.factor(df_articles$prop_model_type_binary))



# #### Bubble plot brouillon ####
# 
# 
# # Trier les cercles du plus grand au plus petit
# data_plot <- df_articles %>%
#   filter(prop_model_pathogen != "Generic pathogen") %>%
#   count(prop_model_pathogen) %>%
#   na.omit() %>%
#   mutate(id = row_number()) %>% # Ajouter un ID unique
#   arrange(desc(n))
# 
# # Nombre de cercles autour du centre
# n_circles <- nrow(data_plot) - 1
# angles <- seq(0, 2*pi, length.out = n_circles + 1)[-1]  
# 
# # Rayon du cercle central (le plus grand)
# radius_center <- sqrt(max(data_plot$n)) * 0.55  
# 
# # Réduction de la distance pour resserrer les cercles
# distance_from_center <- radius_center * 1
# 
# # Calcul des coordonnées des cercles
# data_plot <- data_plot %>%
#   mutate(
#     x = c(0, cos(angles) * distance_from_center),  
#     y = c(0, sin(angles) * distance_from_center), 
#     r = sqrt(n) * 0.45  # Taille ajustée pour éviter les chevauchements
#   )
# 
# # Création du graphique
# bubble_plot <- ggplot() +
#   geom_circle(data = data_plot, aes(x0 = x, y0 = y, r = r, fill = prop_model_pathogen), 
#               color = "black", alpha = 0.8) +
#   geom_text(data = data_plot, 
#             aes(x = x, y = y, label = paste0(prop_model_pathogen, "\n(", n, "/28)")), 
#             color = "black", size = 5, fontface = "bold", vjust = 0.5, hjust = 0.5) +  
#   scale_fill_manual(values = hcl.colors(7, palette = "Dark 3")) +
#   coord_fixed() +  
#   theme_void() +
#   theme(legend.position = "none")
# 
# ### Diapo
# ppt <- read_pptx()
# ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
#   ph_with(value = dml(ggobj = bubble_plot), location = ph_location_fullsize())
# print(ppt, target = "figures/bubbleplot_raw.pptx")
# 
# 
# 
# # Pour considérer les modèles séparément, dans l'article qui étudie multiples pathogènes
# vecteur <- df_articles$prop_model_pathogen[df_articles$prop_model_pathogen != "Multiple"]
# vecteur <- c(vecteur, "Enterobacterales", "S. aureus", "P. aeruginosa", "C. difficile")
# 
# data_plot <- tibble(prop_model_pathogen = vecteur) %>%
#   filter(!is.na(prop_model_pathogen),                # on conserve les NA hors comptage
#          prop_model_pathogen != "Generic pathogen") %>%  # on supprime "Generic pathogen"
#   count(prop_model_pathogen, name = "n") %>%
#   arrange(desc(n)) %>%
#   mutate(id = row_number())
# 
# # Nombre total (pour l’affichage des fractions)
# total_n <- sum(data_plot$n)
# 
# # ---- Disposition des bulles ----
# n_circles <- nrow(data_plot) - 1
# angles <- if (n_circles > 0) {
#   seq(0, 2*pi, length.out = n_circles + 1)[-1]
# } else {
#   numeric(0)
# }
# 
# # Taille / distances
# radius_center <- sqrt(max(data_plot$n)) * 0.55
# distance_from_center <- radius_center * 1
# 
# data_plot <- data_plot %>%
#   mutate(
#     x = c(0, if (n_circles > 0) cos(angles) * distance_from_center),
#     y = c(0, if (n_circles > 0) sin(angles) * distance_from_center),
#     r = sqrt(n) * 0.45
#   )
# 
# # ---- Palette dynamique ----
# pal <- hcl.colors(n = n_distinct(data_plot$prop_model_pathogen), palette = "Dark 3")
# names(pal) <- unique(data_plot$prop_model_pathogen)
# 
# # ---- Graphique ----
# bubble_plot <- ggplot() +
#   geom_circle(data = data_plot,
#               aes(x0 = x, y0 = y, r = r, fill = prop_model_pathogen),
#               color = "black", alpha = 0.8) +
#   geom_text(data = data_plot,
#             aes(x = x, y = y,
#                 label = paste0(prop_model_pathogen, "\n(", n, "/", total_n, ")")),
#             color = "black", size = 5, fontface = "bold", vjust = 0.5, hjust = 0.5) +
#   scale_fill_manual(values = pal) +
#   coord_fixed() +
#   theme_void() +
#   theme(legend.position = "none")
# 
# # ---- Export PowerPoint ----
# ppt <- read_pptx()
# ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme") %>%
#   ph_with(value = dml(ggobj = bubble_plot), location = ph_location_fullsize())
# 
# print(ppt, target = "figures/bubbleplot_raw.pptx")
# 



### Type de modèle ###
summary(as.factor(df_articles$prop_model_yes_no))
summary(as.factor(df_articles$prop_model_details))



### Interventions ###
summary(as.factor(df_articles$prop_model_interv_yes_no))
summary(as.factor(df_articles$prop_model_interv_grp))
split_interv = unlist(strsplit(df_articles$prop_model_interv_grp, ", "))
split_interv = split_interv[!is.na(split_interv)]
split_interv
interv_grp = as.data.frame(table(split_interv))
write.xlsx(interv_grp, "figures/modele_epidemio/interv_grp_raw.xlsx")


### Barplot interventiosn 

interv_arrangé = read.xlsx("figures/modele_epidemio/interv_grp_arrangé.xlsx")

interv_arrangé$Intervention <- factor(
  interv_arrangé$Intervention,
  levels = interv_arrangé$Intervention[order(interv_arrangé$Frequency, decreasing = FALSE)]
)

interv_arrangé <- interv_arrangé %>%
  mutate(Intervention = str_wrap(Intervention, width = 44))

interv_plot <- ggplot(interv_arrangé, aes(x = reorder(Intervention, Frequency), y = Frequency)) +
  geom_col(fill = "black", width=0.7) +
  coord_flip() +
  theme_classic() +
  labs(x = NULL, y = "Frequency") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.title.x = element_text(size = 14, color = "black")
  )
ggsave(interv_plot, file="figures/interv_plot.png", dpi = 600, width = 26, height = 13, unit="cm")

ppt <- read_pptx()

# Slide blanche (plus propre pour une figure)
ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")

# --- Insertion du graphique en vectoriel (éditable) ---
ppt <- ph_with(
  ppt,
  value = dml(ggobj = interv_plot),
  location = ph_location(
    left   = 0.5,
    top    = 0.5,
    width  = 10,    # largeur en pouces
    height = 5      # hauteur en pouces (ajuste selon ton poster)
  )
)

# --- Export ---
print(ppt, target = "figures/modele_epidemiointerv_plot.pptx")



# types de modèles par pathogène
table(as.factor(df_articles$prop_model_type_binary),
      as.factor(df_articles$prop_model_pathogen))

# stacked barplot
df_plot <- as.data.frame(table(df_articles$prop_model_pathogen,
                               df_articles$prop_model_type_binary))
colnames(df_plot) <- c("Pathogen", "Model_type", "Count")

ggplot(df_plot, aes(x = Pathogen, y = Count, fill = Model_type)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Model Type by Pathogen",
       x = "Pathogen",
       y = "Number of Articles",
       fill = "Model Type")
