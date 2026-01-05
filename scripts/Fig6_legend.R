# --- LÉGENDE ALIGNÉE : colonnes plus espacées, lignes resserrées ---
library(tidyverse)
library(ggplot2)

# Paramètres d'espacement
col_spacing <- 1  # ↑ augmente pour écarter les colonnes
row_spacing <- 0.2   # ↓ diminue pour rapprocher les lignes
max_rows <- 10

legend_df <- network_info %>%
  mutate(
    continent = factor(continent,
                       levels = c("Europe", "North America", "Asia", "Oceania")),
    shape = as.numeric(shape)
  ) %>%
  arrange(continent, network) %>%
  group_by(continent) %>%
  mutate(
    idx     = row_number() - 1,
    col     = idx %/% max_rows,                  # nombre de lignes par colonne
    row     = idx %% max_rows,
    x       = col * col_spacing,           # espacement horizontal
    y       = -row * row_spacing           # espacement vertical
  ) %>%
  ungroup()

# Plot
p_legend <- ggplot(legend_df, aes(x, y)) +
  geom_point(aes(color = color, shape = shape), size = 4.2) +
  geom_text(aes(label = network), hjust = 0, nudge_x = 0.06, size = 5.6) +
  scale_color_identity() +
  scale_shape_identity() +
  facet_grid(. ~ continent, scales = "free_x", space = "free_x") +
  coord_cartesian(clip = "off") +
  theme_void(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 13, margin = margin(b = 3)),
    panel.spacing.x = unit(9, "lines"),     # espace entre les continents
    plot.margin = margin(10, 15, 10, 10)
  )

p_legend



ppt <- read_pptx()
ppt <- add_slide(ppt, layout = "Title and Content", master = "Office Theme")
ppt <- ph_with(ppt, dml(ggobj = p_legend), location = ph_location_fullsize())
print(ppt, target = "legend_networks_compact.pptx")
