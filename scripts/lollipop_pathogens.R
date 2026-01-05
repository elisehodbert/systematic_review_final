library(dplyr)
library(ggplot2)
library(forcats)

# --- Données ---
vecteur <- df_articles$prop_model_pathogen[df_articles$prop_model_pathogen != "Multiple"]
vecteur <- c(vecteur, "Enterobacterales", "S. aureus", "P. aeruginosa", "C. difficile")

data_plot <- tibble(prop_model_pathogen = vecteur) %>%
  filter(!is.na(prop_model_pathogen),
         prop_model_pathogen != "Generic pathogen") %>%
  count(prop_model_pathogen, name = "n") %>%
  arrange(desc(n))

# --- Palette : une couleur par pathogène ---
pal <- hcl.colors(n = nrow(data_plot), palette = "Dark 3")
names(pal) <- data_plot$prop_model_pathogen

# --- Lollipop compact ---

max_n <- max(data_plot$n)

lollipop_pathogens <- data_plot %>%
  mutate(prop_model_pathogen = fct_reorder(prop_model_pathogen, n)) %>% 
  ggplot(aes(x = n, y = prop_model_pathogen, color = prop_model_pathogen)) +
  geom_segment(aes(x = 0, xend = n,
                   y = prop_model_pathogen, yend = prop_model_pathogen),
               linewidth = 1) +
  geom_point(size = 5) +
  scale_color_manual(values = pal) +
  scale_x_continuous(
    breaks = 0:max_n,          # ✅ uniquement des entiers
    limits = c(0, max_n),      # optionnel, mais propre
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = "Number of models", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )


# --- Export PPTX éditable ---
ppt <- read_pptx()
ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")

ppt <- ph_with(
  ppt,
  value = dml(ggobj = lollipop_pathogens),
  location = ph_location(
    left   = 0.5,   # position horizontale (en pouces)
    top    = 1.5,   # position verticale
    width  = 8,    # large
    height = 5      # ✅ peu de hauteur -> lollipops serrés
  )
)

print(ppt, target = "figures/lollipop_pathogens.pptx")

