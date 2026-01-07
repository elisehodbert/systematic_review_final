#### Creation of df_networks_unique ####
df_networks_unique <- df_networks %>%
  arrange(network, desc(nb_facilities)) %>% 
  distinct(network, .keep_all = TRUE) 

#### Definition of parameters ####
violin_colors <- c(
  "National"                    = "#A8C5DA",
  "Subnational"                 = "#B8D5B8",
  "Hospital or group of hospitals" = "#F47C57"
)

# Scales of shapes and colors
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

# Common theme
common_theme <- theme_minimal(base_size = 16) + 
  theme(
    legend.position   = "none",
    panel.grid        = element_blank(),
    axis.text.x       = element_blank(),
    axis.ticks.x      = element_blank(),
    axis.line.y       = element_line(color = "black"),
    axis.text.y       = element_text(color = "black", size = 14),  
    axis.title.y      = element_text(size = 16),    
    strip.text        = element_text(size = 16),  
    plot.title        = element_text(size = 18, hjust = 0.5)
  )

#### Number of faciltiies (x= articles) ####

# pA = number of facilities
pA <- df_networks %>%
  filter(scale_grp %in% c("Subnational", "National")) %>%
  ggplot(aes(x = 1, y = nb_facilities, group = 1, fill = scale_grp)) +
  geom_violin(
    alpha          = 0.9,
    color          = "black",
    width          = 0.6,
    trim           = FALSE,
    scale          = "width",
    draw_quantiles = 0.5   # ligne de médiane
  ) +
  geom_jitter(aes(color = network, shape = network),
              width = 0.2, size = 4) +
  scale_fill_manual(values = violin_colors, guide = FALSE) +  # couleurs de fond
  scales_noleg +
  scale_y_log10(limits = c(0.99, 1e4)) +
  facet_wrap(~ scale_grp) +
  labs(x = NULL, y = "Number of facilities (log)") +
  common_theme

# pb = number of units
pB <- df_networks %>%
  filter(scale_grp == "Hospital or group of hospitals") %>%
  ggplot(aes(x = 1, y = nb_units, group = 1, fill = scale_grp)) +
  geom_violin(
    alpha          = 0.9,
    color          = "black",
    width          = 0.6,
    trim           = FALSE,
    scale          = "width",
    draw_quantiles = 0.5
  ) +
  geom_jitter(aes(color = network, shape = network),
              width = 0.1, size = 4) +
  scale_fill_manual(values = violin_colors, guide = FALSE) +
  scales_noleg +
  scale_y_log10(limits = c(10, 1e3)) +
  facet_wrap(~ scale_grp) +
  labs(x = NULL, y = "Number of units (log)") +
  common_theme

fig2 <- pA + pB + 
  plot_layout(
    ncol    = 2,
    widths  = c(2, 1),
    guides  = "collect"
  ) &
  theme(
    legend.title    = element_blank(),
    legend.position = "bottom",
    legend.text       = element_text(size = 14),
    legend.key.size   = unit(1.2, "lines")
  )
fig2

# Exporting the plot
ggsave(fig2, file="figures/nb_facilities_nb_mvts/violin_nb_facilities_article.png", width = 30, height = 20, dpi = 600, units = "cm")

pptx <- read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(
    value = dml(ggobj = fig2),                         
    location = ph_location(
      left   = 0, 
      top    = 0, 
      width  = 30/2.54, 
      height = 20/2.54
    )
  )

print(pptx, target = "figures/nb_facilities_nb_mvts/nb_facilities_units_article.pptx")




#### Number of facilities (x = networks)
violin_colors <- c(
  "National"                    = "#A8C5DA",
  "Subnational"                 = "#B8D5B8",
  "Hospital or group of hospitals" = "#F47C57"
)

# pA = number of facilities
pA <- df_networks_unique %>%
  filter(scale_grp %in% c("Subnational", "National")) %>%
  ggplot(aes(x = 1, y = nb_facilities, group = 1, fill = scale_grp)) +
  geom_violin(
    alpha          = 0.9,
    color          = "black",
    width          = 0.6,
    trim           = FALSE,
    scale          = "width",
    draw_quantiles = 0.5   # ligne de médiane
  ) +
  geom_jitter(aes(color = network, shape = network),
              width = 0.2, size = 4) +
  scale_fill_manual(values = violin_colors, guide = FALSE) +  # couleurs de fond
  scales_noleg +
  scale_y_log10(limits = c(0.99, 1e4)) +
  facet_wrap(~ scale_grp) +
  labs(x = NULL, y = "Number of facilities (log)") +
  common_theme

# pB = number of units
pB <- df_networks_unique %>%
  filter(scale_grp == "Hospital or group of hospitals") %>%
  ggplot(aes(x = 1, y = nb_units, group = 1, fill = scale_grp)) +
  geom_violin(
    alpha          = 0.9,
    color          = "black",
    width          = 0.6,
    trim           = FALSE,
    scale          = "width",
    draw_quantiles = 0.5
  ) +
  geom_jitter(aes(color = network, shape = network),
              width = 0.1, size = 4) +
  scale_fill_manual(values = violin_colors, guide = FALSE) +
  scales_noleg +
  scale_y_log10(limits = c(10, 1e3)) +
  facet_wrap(~ scale_grp) +
  labs(x = NULL, y = "Number of units (log)") +
  common_theme

# Assembling the plots
fig2 <- pA + pB + 
  plot_layout(
    ncol    = 2,
    widths  = c(2, 1),
    guides  = "collect"
  ) &
  theme(
    legend.title    = element_blank(),
    legend.position = "bottom",
    legend.text       = element_text(size = 14),
    legend.key.size   = unit(1.2, "lines")
  )
fig2

# Exporting the assembled plot
ggsave(fig2, file="figures/nb_facilities_nb_mvts/violin_nb_facilities_reseau.png", width = 30, height = 20, dpi = 600, units = "cm")

pptx <- read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(
    value = dml(ggobj = fig2),                         
    location = ph_location(
      left   = 0, 
      top    = 0, 
      width  = 30/2.54, 
      height = 20/2.54
    )
  )
print(pptx, target = "figures/nb_facilities_nb_mvts/nb_facilities_units_reseau.pptx")


#### Median calculus (articles) ####
df_networks %>%
  filter(scale_grp == "National") %>%
  summarise(
    q2.5   = quantile(nb_facilities, probs = 0.025, na.rm = TRUE),
    median = median(nb_facilities,      na.rm = TRUE),
    q97.5  = quantile(nb_facilities, probs = 0.975, na.rm = TRUE))

df_networks %>%
  filter(scale_grp == "Subnational") %>%
  summarise(
    q2.5   = quantile(nb_facilities, probs = 0.025, na.rm = TRUE),
    median = median(nb_facilities,      na.rm = TRUE),
    q97.5  = quantile(nb_facilities, probs = 0.975, na.rm = TRUE))

df_networks %>%
  filter(scale_grp == "Hospital or group of hospitals") %>%
  summarise(
    q2.5   = quantile(nb_units, probs = 0.025, na.rm = TRUE),
    median = median(nb_units,      na.rm = TRUE),
    q97.5  = quantile(nb_units, probs = 0.975, na.rm = TRUE))



#### Median calculus (networks) ####
df_networks %>%
  filter(scale_grp == "National") %>%
  group_by(network) %>%
  slice_max(nb_facilities, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  summarise(
    q2.5   = quantile(nb_facilities, probs = 0.025, na.rm = TRUE),
    median = median(nb_facilities,      na.rm = TRUE),
    q97.5  = quantile(nb_facilities, probs = 0.975, na.rm = TRUE)
  )

df_networks %>%
  filter(scale_grp == "Subnational") %>%
  group_by(network) %>%
  slice_max(nb_facilities, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  summarise(
    q2.5   = quantile(nb_facilities, probs = 0.025, na.rm = TRUE),
    median = median(nb_facilities,      na.rm = TRUE),
    q97.5  = quantile(nb_facilities, probs = 0.975, na.rm = TRUE))

df_networks %>%
  filter(scale_grp == "Hospital or group of hospitals") %>%
  group_by(network) %>%
  slice_max(nb_facilities, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  summarise(
    q2.5   = quantile(nb_units, probs = 0.025, na.rm = TRUE),
    median = median(nb_units,      na.rm = TRUE),
    q97.5  = quantile(nb_units, probs = 0.975, na.rm = TRUE))