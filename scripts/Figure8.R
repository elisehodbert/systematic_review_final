# Number of articles that developed a model
summary(as.factor(df_articles$prop_model_yes_no))

# Number of networks that had a least a model developed on them
résumé <- df_networks %>%
  group_by(network) %>%
  summarize(au_moins_un_yes = any(prop_model_yes_no == "yes"), .groups = "drop")
summary(résumé$au_moins_un_yes)

# Summary of pathogens
summary(as.factor(df_articles$prop_model_pathogen))

# Epidemio data?
summary(as.factor(df_articles$epidemio_data_yes_no))

# Types of models
summary(as.factor(df_articles$prop_model_type_binary))

# Type de modèle
summary(as.factor(df_articles$prop_model_yes_no))
summary(as.factor(df_articles$prop_model_details))

# Types of models per pathogen
table(as.factor(df_articles$prop_model_type_binary),
      as.factor(df_articles$prop_model_pathogen))

# Interventions
summary(as.factor(df_articles$prop_model_interv_yes_no))
summary(as.factor(df_articles$prop_model_interv_grp))
split_interv = unlist(strsplit(df_articles$prop_model_interv_grp, ", "))
split_interv = split_interv[!is.na(split_interv)]
split_interv
interv_grp = as.data.frame(table(split_interv))
write.xlsx(interv_grp, "figures/modele_epidemio/interv_grp_raw.xlsx")

# Barplot interventions
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

# Export the plot
ggsave(interv_plot, file="figures/interv_plot.png", dpi = 600, width = 26, height = 13, unit="cm")

ppt <- read_pptx()
ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")
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
print(ppt, target = "figures/modele_epidemio/interv_plot.pptx")
