#### Importation and preprecrocessing ####

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
    note = factor(note, levels = 0:3),
    critere = factor(critere, levels = rev(names(grille_quality)[6:18]))
  )

df_count <- df_long %>%
  count(critere, note)

conf = ggplot(df_count, aes(x = n, y = critere, fill = note)) +
  geom_col() +
  scale_fill_manual(
    name   = "Note",
    values = c(
      "0"  = "#B2182B",  
      "1"  = "#EF8A62",  
      "2"  = "#67A9CF",  
      "3"  = "#2166AC",  
      "NA" = "grey75"   
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



# Descriptive statistics

summary(as.factor(grille_quality$`5 - Data choice rationale`))
summary(as.factor(grille_quality$`8 - Analytic method rationale`))
summary(as.factor(grille_quality$`10 - Stakeholder input`))
