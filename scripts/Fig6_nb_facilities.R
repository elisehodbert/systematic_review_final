##### Par article #####

# 1. Chargement des packages
pacman::p_load(ggplot2, 
               patchwork, 
               dplyr, 
               scales, 
               grid,
               officer,
               rvg)

# 3. Échelles de couleur/forme pour pA/pB (sans légende) et pour legend_plot
scales_noleg <- list(
  scale_color_manual(values = network_colors, guide = FALSE),
  scale_shape_manual(values = network_shapes, guide = FALSE)
)
scales_legend <- list(
  scale_color_manual(
    name  = "Network",
    values = network_colors,
    guide = guide_legend(override.aes = list(size = 4, alpha = 1))
  ),
  scale_shape_manual(
    name  = "Network",
    values = network_shapes,
    guide = guide_legend(override.aes = list(size = 4, alpha = 1))
  )
)

# 4. Thème commun (sans légende)
common_theme <- theme_minimal(base_size = 16) +  # taille de base à 16 pt (au lieu de 11)
  theme(
    legend.position   = "none",
    panel.grid        = element_blank(),
    axis.text.x       = element_blank(),
    axis.ticks.x      = element_blank(),
    axis.line.y       = element_line(color = "black"),
    axis.text.y       = element_text(color = "black", size = 14),    # texte des graduations Y
    axis.title.y      = element_text(size = 16),     # titre de l’axe Y
    strip.text        = element_text(size = 16),     # texte des facettes
    plot.title        = element_text(size = 18, hjust = 0.5)
  )

# 5. pA : nb_facilities (Subnational vs National) avec boxplots gris
pA <- df_networks %>%
  filter(scale_grp %in% c("Subnational", "National")) %>%
  ggplot(aes(x = 1, y = nb_facilities)) +
  geom_boxplot(
    fill          = "white",
    alpha         = 0.9,
    outlier.shape = NA       # plus de points noirs
  ) +
  geom_jitter(aes(color = network, shape = network),
              width = 0.2, size = 4) +
  scales_noleg +
  scale_y_log10(limits = c(0.99, 1e4)) +
  facet_wrap(~ scale_grp) +
  labs(x = NULL, y = "Number of facilities (log)") +
  common_theme

# 6. pB : nb_units (Hospital or group of hospitals) avec boxplots gris
pB <- df_networks %>%
  filter(scale_grp == "Hospital or group of hospitals") %>%
  ggplot(aes(x = 1, y = nb_units)) +
  geom_boxplot(
    fill          = "white",
    alpha         = 0.9,
    outlier.shape = NA       # plus de points noirs
  ) +
  geom_jitter(aes(color = network, shape = network),
              width = 0.1, size = 4) +
  scales_noleg +
  scale_y_log10(limits = c(10, 1e3)) +
  facet_wrap(~ scale_grp) +
  labs(x = NULL, y = "Number of units (log)") +
  common_theme

# 7. Plot dédié à la légende
legend_plot <- ggplot(df_networks,
                      aes(x = network, y = nb_facilities,
                          color = network, shape = network)) +
  geom_point(alpha = 0) +  # invisibles, servent à générer la légende
  scales_legend +
  theme_void() +
  theme(
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.box        = "horizontal"
  )

# 8. Assemblage final
fig2 <- pA + pB + legend_plot +
  plot_layout(
    ncol    = 2,
    widths  = c(2, 1),
    heights = c(1, 0.15),
    guides  = "collect"
  ) &
  theme(
    legend.title    = element_blank(),
    legend.position = "bottom",
    legend.text       = element_text(size = 14),
    legend.key.size   = unit(1.2, "lines")
  )

# Affichage
print(fig2)

ggsave("figures/nb_facilities_nb_mvts/nb_facilities_units.pdf", fig2,
       width = 45, height = 40, units = "cm", dpi = 600)


# en ppt
library(officer)
library(rvg)

pptx <- read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(
    value = dml(ggobj = fig2),                         
    location = ph_location(
      left   = 0, 
      top    = 0, 
      width  = 35/2.54, 
      height = 30/2.54
    )
  )


print(pptx, target = "figures/nb_facilities_nb_mvts/nb_facilities_units_articles.pptx")

###### Par réseau #####

df_networks_unique <- df_networks %>%
  arrange(network, desc(nb_facilities)) %>%  # trier par réseau puis nb_facilities décroissant
  distinct(network, .keep_all = TRUE)        # ne garder qu’une ligne par réseau

# 3. Échelles de couleur/forme pour pA/pB (sans légende) et pour legend_plot
scales_noleg <- list(
  scale_color_manual(values = network_colors, guide = FALSE),
  scale_shape_manual(values = network_shapes, guide = FALSE)
)
scales_legend <- list(
  scale_color_manual(
    name  = "Network",
    values = network_colors,
    guide = guide_legend(override.aes = list(size = 4, alpha = 1))
  ),
  scale_shape_manual(
    name  = "Network",
    values = network_shapes,
    guide = guide_legend(override.aes = list(size = 4, alpha = 1))
  )
)

# 4. Thème commun (sans légende)
common_theme <- theme_minimal(base_size = 16) +  # taille de base à 16 pt (au lieu de 11)
  theme(
    legend.position   = "none",
    panel.grid        = element_blank(),
    axis.text.x       = element_blank(),
    axis.ticks.x      = element_blank(),
    axis.line.y       = element_line(color = "black"),
    axis.text.y       = element_text(color = "black", size = 14),    # texte des graduations Y
    axis.title.y      = element_text(size = 16),     # titre de l’axe Y
    strip.text        = element_text(size = 16),     # texte des facettes
    plot.title        = element_text(size = 18, hjust = 0.5)
  )

# 5. pA : nb_facilities (Subnational vs National) avec boxplots gris
pA <- df_networks_unique %>%
  filter(scale_grp %in% c("Subnational", "National")) %>%
  ggplot(aes(x = 1, y = nb_facilities)) +
  geom_boxplot(
    fill          = "white",
    alpha         = 0.9,
    outlier.shape = NA       # plus de points noirs
  ) +
  geom_jitter(aes(color = network, shape = network),
              width = 0.2, size = 4) +
  scales_noleg +
  scale_y_log10(limits = c(0.99, 1e4)) +
  facet_wrap(~ scale_grp) +
  labs(x = NULL, y = "Number of facilities (log)") +
  common_theme

# 6. pB : nb_units (Hospital or group of hospitals) avec boxplots gris
pB <- df_networks_unique %>%
  filter(scale_grp == "Hospital or group of hospitals") %>%
  ggplot(aes(x = 1, y = nb_units)) +
  geom_boxplot(
    fill          = "white",
    alpha         = 0.9,
    outlier.shape = NA       # plus de points noirs
  ) +
  geom_jitter(aes(color = network, shape = network),
              width = 0.1, size = 4) +
  scales_noleg +
  scale_y_log10(limits = c(10, 1e3)) +
  facet_wrap(~ scale_grp) +
  labs(x = NULL, y = "Number of units (log)") +
  common_theme

# 7. Plot dédié à la légende
legend_plot <- ggplot(df_networks_unique,
                      aes(x = network, y = nb_facilities,
                          color = network, shape = network)) +
  geom_point(alpha = 0) +  # invisibles, servent à générer la légende
  scales_legend +
  theme_void() +
  theme(
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.box        = "horizontal"
  )

# 8. Assemblage final
fig2 <- pA + pB + legend_plot +
  plot_layout(
    ncol    = 2,
    widths  = c(2, 1),
    heights = c(1, 0.15),
    guides  = "collect"
  ) &
  theme(
    legend.title    = element_blank(),
    legend.position = "bottom",
    legend.text       = element_text(size = 14),
    legend.key.size   = unit(1.2, "lines")
  )

pptx <- read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(
    value = dml(ggobj = fig2),                         
    location = ph_location(
      left   = 0, 
      top    = 0, 
      width  = 35/2.54, 
      height = 30/2.54
    )
  )


print(pptx, target = "figures/nb_facilities_nb_mvts/nb_facilities_units_networks.pptx")







