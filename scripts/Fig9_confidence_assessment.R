grille_quality = read.xlsx("quality_assessment.xlsx")

grille_quality <- grille_quality %>%
  mutate(across(6:18, ~ as.factor(substr(as.character(.), 1, 1)))) %>%
  select(
    - starts_with("5"),
    - starts_with("8"),
    - starts_with("brouillon")
  )

colnames(grille_quality)[6:ncol(grille_quality)] = c(
  "1 - Conceptual basis",
  "2 - Aims",
  "3 - Research setting and\ntarget population description",
  "4 - Research setting adequacy",
  "5 - Data choice rationale",
  "6 - Data adequacy",
  "7 - Recruitement data",
  "8 - Analytic method rationale",
  "9 - Analytic method adequacy",
  "10 - Stakeholder input",
  "11 - Strength and limits"
)

### Figure ###

df_long <- grille_quality %>%
  select(6:ncol(grille_quality)) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "critere",
    values_to = "note"
  ) %>%
  mutate(
    # transformer en facteur 0–3
    note = factor(note, levels = 0:3),
    # garder l'ordre original (inversé pour ggplot)
    critere = factor(critere, levels = rev(names(grille_quality)[6:18]))
  )

df_count <- df_long %>%
  count(critere, note)

conf = ggplot(df_count, aes(x = n, y = critere, fill = note)) +
  geom_col() +
  scale_fill_manual(
    name   = "Note",
    values = c(
      "0"  = "#B2182B",  # medium-red
      "1"  = "#EF8A62",  # salmon-rose
      "2"  = "#67A9CF",  # light-steel-blue
      "3"  = "#2166AC",  # medium-blue
      "NA" = "grey75"    # neutre pour les manquants
    )
  ) +
  labs(
    x = "Number of articles",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "bottom",
    axis.text.y        = element_text(size = 10, color="black"),
    axis.text.x        = element_text(size = 10, color="black"),
    axis.title         = element_text(size = 10),
    panel.grid.major.y = element_blank()
  )
conf

ggsave(conf, file="figures/confidence_assessment.png", dpi=600, width = 20, height = 12, unit="cm", bg       = "white")





# Table S5
# 
# library(dplyr)
# library(stringr)
# library(writexl)
# 
# # 1. Garder uniquement le premier caractère des colonnes 6 à 19
# grille_quality_mod <- grille_quality %>%
#   mutate(across(6:19, ~ str_sub(., 1, 1)))
# 
# # 2. Créer le tableau agrégé avec retours à la ligne
# grille_quality_agg <- grille_quality_mod %>%
#   rowwise() %>%
#   mutate(
#     agg = paste(
#       sprintf("0: %d", sum(c_across(6:19) == "0", na.rm = TRUE)),
#       sprintf("1: %d", sum(c_across(6:19) == "1", na.rm = TRUE)),
#       sprintf("2: %d", sum(c_across(6:19) == "2", na.rm = TRUE)),
#       sprintf("3: %d", sum(c_across(6:19) == "3", na.rm = TRUE)),
#       sep = "\n"
#     )
#   ) %>%
#   ungroup() %>%
#   select(1, agg)  # Garde la colonne 1 + colonne agrégée
# 
# # 3. Exporter en Excel
# write_xlsx(grille_quality_agg, "grille_quality_agg.xlsx")



# Tableau avec 5 colonnes (ocunt de 0 etc.)
# library(dplyr)
# library(stringr)
# 
# # 1. Garder uniquement le premier caractère des colonnes 6 à 19
# grille_quality_mod <- grille_quality %>%
#   mutate(across(6:19, ~ str_sub(., 1, 1)))
# 
# # 2. Créer un tableau avec 1 + 4 colonnes
# grille_quality_counts <- grille_quality_mod %>%
#   rowwise() %>%
#   mutate(
#     count_0 = sum(c_across(6:19) == "0", na.rm = TRUE),
#     count_1 = sum(c_across(6:19) == "1", na.rm = TRUE),
#     count_2 = sum(c_across(6:19) == "2", na.rm = TRUE),
#     count_3 = sum(c_across(6:19) == "3", na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   select(1, count_0:count_3)  # Garde la colonne 1 + colonnes de comptage
# 
# # Afficher le résultat
# write_xlsx(grille_quality_counts, "grille_quality_counts.xlsx")




### stats sur les résultats

summary(as.factor(grille_quality$`5 - Data choice rationale`))
summary(as.factor(grille_quality$`8 - Analytic method rationale`))
summary(as.factor(grille_quality$`10 - Stakeholder input`))
