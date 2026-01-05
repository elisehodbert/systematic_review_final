# ============================================================
# Bubble plot "camembert" : bulles = pathogènes, parts = type de modèle
# Variable de parts : prop_model_type_binary
# Taille bulle : nombre d'articles du pathogène
# ============================================================

pacman::p_load(
  tidyverse, packcircles, ggplot2, ggforce, dplyr,
  officer, rvg
)

# ----------------------------
# 1) Préparation des données
# ----------------------------
# df_articles doit contenir au moins :
# - prop_model_pathogen
# - prop_model_type_binary

df_clean <- df_articles %>%
  filter(
    !is.na(prop_model_pathogen),
    prop_model_pathogen != "Generic pathogen"
  )

# Comptage par pathogène x type de modèle
df_counts <- df_clean %>%
  filter(!is.na(prop_model_type_binary)) %>%
  count(prop_model_pathogen, prop_model_type_binary, name = "n_type")

# Total par pathogène (sert pour rayon + label)
df_total <- df_counts %>%
  group_by(prop_model_pathogen) %>%
  summarise(n_total = sum(n_type), .groups = "drop")

# ----------------------------
# 2) Disposition des bulles (packing)
# ----------------------------
# Rayon ~ sqrt(n_total) (classique pour aire ~ n_total)
bubble_layout <- df_total %>%
  mutate(r = sqrt(n_total)) %>%
  arrange(desc(n_total))

layout <- packcircles::circleProgressiveLayout(bubble_layout$r, sizetype = "area")

bubble_layout <- bubble_layout %>%
  bind_cols(as_tibble(layout)) %>%     # ajoute x, y
  mutate(r_plot = r * 0.35)            # facteur d'échelle visuel (à ajuster si besoin)

# ----------------------------
# 3) Construire les "parts" du camembert pour ggforce::geom_arc_bar
# ----------------------------
# On calcule pour chaque pathogène les angles cumulés (start/end) des parts
df_pie <- df_counts %>%
  left_join(bubble_layout %>% select(prop_model_pathogen, x, y, r_plot, n_total),
            by = "prop_model_pathogen") %>%
  group_by(prop_model_pathogen) %>%
  arrange(prop_model_type_binary, .by_group = TRUE) %>%
  mutate(
    frac  = n_type / sum(n_type),
    end   = 2*pi*cumsum(frac),
    start = lag(end, default = 0),
    r0    = 0
  ) %>%
  ungroup()

# Palette (dynamique) pour les types de modèles
pal_type <- hcl.colors(
  n = n_distinct(df_pie$prop_model_type_binary),
  palette = "Dark 3"
)
names(pal_type) <- sort(unique(df_pie$prop_model_type_binary))

# ----------------------------
# 4) Plot
# ----------------------------
bubble_pie_plot <- ggplot() +
  
  # Camemberts (parts)
  ggforce::geom_arc_bar(
    data = df_pie,
    aes(
      x0 = x, y0 = y,
      r0 = r0, r = r_plot,
      start = start, end = end,
      fill = prop_model_type_binary
    ),
    color = "black",
    linewidth = 0.25,
    alpha = 0.95
  ) +
  
  # Contour cercle (optionnel mais propre)
  ggforce::geom_circle(
    data = bubble_layout,
    aes(x0 = x, y0 = y, r = r_plot),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.35,
    fill = NA
  ) +
  
  # Labels (pathogène + total)
  geom_text(
    data = bubble_layout,
    aes(x = x, y = y, label = paste0(prop_model_pathogen, "\n(n=", n_total, ")")),
    size = 4,
    fontface = "bold",
    lineheight = 0.95
  ) +
  
  scale_fill_manual(values = pal_type) +
  coord_fixed() +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank()
  )

bubble_pie_plot

# ----------------------------
# 5) Export PowerPoint (vectoriel, éditable)
# ----------------------------
ppt <- read_pptx() %>%
  add_slide(layout = "Blank", master = "Office Theme") %>%
  ph_with(
    value = dml(ggobj = bubble_pie_plot),
    location = ph_location_fullsize()
  )

print(ppt, target = "figures/bubbleplot_camembert_prop_model_type_binary.pptx")
