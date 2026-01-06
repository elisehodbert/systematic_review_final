### Barplot nb Publication years
nb_years = ggplot(df_articles, aes(x = factor(year_publi))) +
  geom_bar(fill = "black", color = "black", alpha = 0.8, stat = "count") +
  labs(x = "Publication Year",
       y = "Number of articles") +
  theme_minimal(base_size = 20) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(0, 13, by = 2), expand = c(0, 0)) +  # Fixer l'origine à 0 avec expand = c(0, 0)
  theme(axis.text.x = element_text(angle = 90, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"))  # Ajouter des axes noirs
nb_years

ggsave(nb_years, filename = "figures/descriptive_analyses/nb_years.png", width = 25, height = 25, dpi = 600, units = "cm") 


### Arranging the data for doing a pie plot in Excel
data_pie <- df_articles %>%
  count(subject_journal_grp) %>%
  mutate(percentage = n / sum(n) * 100)  # Calcul des pourcentages
write_xlsx(data_pie, "figures/descriptive_analyses/data_pie.xlsx")



### Number of publications per network
summary(as.factor(df_articles$name_location))
length(unique(df_articles$name_location))
View(df_articles %>% count(name_location))
