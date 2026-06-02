library(patchwork)

#### Importation and preprocessing ####

grille_quality <- read.xlsx("quality_assessment_v0.xlsx")

grille_quality <- grille_quality %>%
  mutate(across(6:18, ~ as.factor(substr(as.character(.), 1, 1)))) %>%
  select(
    - starts_with("5"),
    - starts_with("8"),
    - starts_with("brouillon")
  )

colnames(grille_quality)[6:ncol(grille_quality)] <- c(
  "1 - Conceptual basis",
  "2 - Aims",
  "3 - Research setting and\ntarget population description",
  "4 - Research setting adequacy",
  "5 - Data choice rationale",
  "6 - Data adequacy",
  "7 - Recruitment data",
  "8 - Analytic method rationale",
  "9 - Analytic method adequacy",
  "10 - Stakeholder input",
  "11 - Strength and limits"
)

#### Figure ####

critere_levels <- rev(names(grille_quality)[6:ncol(grille_quality)])

df_long <- grille_quality %>%
  mutate(
    period = case_when(
      year_publi >= 2010 & year_publi <= 2014 ~ "2010-2014",
      year_publi >= 2015 & year_publi <= 2019 ~ "2015-2019",
      year_publi >= 2020 & year_publi <= 2025 ~ "2020-2025",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  select(period, 6:ncol(grille_quality)) %>%
  pivot_longer(
    cols = -period,
    names_to = "critere",
    values_to = "note"
  ) %>%
  mutate(
    note = factor(note, levels = c("0", "1", "2", "3", NA)),
    critere = factor(critere, levels = critere_levels),
    period = factor(period, levels = c("2010-2014", "2015-2019", "2020-2025"))
  )

df_count <- df_long %>%
  count(period, critere, note)

make_plot <- function(data, period_name) {
  ggplot(filter(data, period == period_name), aes(x = n, y = critere, fill = note)) +
    geom_col() +
    scale_fill_manual(
      name = "Note",
      values = c(
        "0" = "#B2182B",
        "1" = "#EF8A62",
        "2" = "#67A9CF",
        "3" = "#2166AC",
        "NA" = "grey75"
      ),
      drop = FALSE
    ) +
    labs(
      title = period_name,
      x = "Number of articles",
      y = "",
      tag = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(size = 10, color = "black"),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.title = element_text(size = 10),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      plot.tag = element_text(face = "bold", size = 14),
      panel.grid.major.y = element_blank()
    )
}

p1 <- make_plot(df_count, "2010-2014")
p2 <- make_plot(df_count, "2015-2019")
p3 <- make_plot(df_count, "2020-2025")

conf_periods <- (p1 + p2 + p3) +
  plot_layout(nrow = 1, guides = "collect") +
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      legend.position = "bottom",
      plot.tag = element_text(face = "bold", size = 14)
    )
  )

conf_periods

ggsave(
  conf_periods,
  file = "figures/confidence_assessment_by_period.tiff",
  dpi = 600,
  width = 30,
  height = 12,
  unit = "cm",
  bg = "white",
  compression = "lzw"
)



# Score 7 moyen + SD

summary_critere7 <- grille_quality %>%
  mutate(
    period = case_when(
      year_publi >= 2010 & year_publi <= 2014 ~ "2010-2014",
      year_publi >= 2015 & year_publi <= 2019 ~ "2015-2019",
      year_publi >= 2020 & year_publi <= 2025 ~ "2020-2025",
      TRUE ~ NA_character_
    ),
    critere7 = as.numeric(as.character(`7 - Recruitment data`))
  ) %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarise(
    n = sum(!is.na(critere7)),
    mean_score = mean(critere7, na.rm = TRUE),
    sd_score = sd(critere7, na.rm = TRUE),
    median_score = median(critere7, na.rm = TRUE),
    q1 = quantile(critere7, 0.25, na.rm = TRUE),
    q3 = quantile(critere7, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

summary_critere7

#### Trend test for criterion 7 ####

trend_critere7 <- grille_quality %>%
  mutate(
    period = case_when(
      year_publi >= 2010 & year_publi <= 2014 ~ 1,
      year_publi >= 2015 & year_publi <= 2019 ~ 2,
      year_publi >= 2020 & year_publi <= 2025 ~ 3,
      TRUE ~ NA_real_
    ),
    critere7 = as.numeric(as.character(`7 - Recruitment data`))
  ) %>%
  filter(!is.na(period), !is.na(critere7))

model_trend_critere7 <- lm(critere7 ~ period, data = trend_critere7)

summary(model_trend_critere7)
confint(model_trend_critere7)

summary(model_trend_critere7)$coefficients["period", ]
