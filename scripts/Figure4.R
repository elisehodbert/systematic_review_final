fill_bar = "#5e3c99"
fill_hist = "#4c72b0"

### Barplot types of databases ###

### Barplot types of databases combinées (x= articles/networks (87))
grille_extract_modif <- df_networks %>%
  mutate(
    type_database_grp = str_replace_all(type_database_grp, "databases", "database"),
    type_database_grp = str_replace_all(type_database_grp, ",\\s*", " and "),
    type_database_grp = str_replace(
      type_database_grp,
      "Health insurance database and Survey database",
      "Health insurance database\nand Survey database"
    )
  )
  
plot_databases_type_87 = ggplot(grille_extract_modif, aes(x = fct_rev(fct_infreq(type_database_grp)))) +
  geom_bar(fill = "#1F3A93", width = 0.6) +
  scale_y_continuous(
    breaks = seq(0, 80, by = 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Number of articles"
  ) +
  theme_minimal() +
  theme(
    axis.text.x       = element_text(size = 18, color = "black"),
    axis.text.y       = element_text(size = 20, color = "black"),
    axis.title.x      = element_text(size = 20, color = "black"),
    legend.position   = "none",
    panel.grid        = element_blank(),
    axis.line         = element_line(linewidth = 1, color = "black"),
    axis.ticks.x      = element_line(size = 1, color = "black"),
    axis.ticks.length = unit(5, "pt")
  )
plot_databases_type_87

### Barplot types of databases (x: articles (73))

grille_extract_modif <- df_articles %>%
  mutate(
    type_database_grp = str_replace_all(type_database_grp, "databases", "database"),
    type_database_grp = str_replace_all(type_database_grp, ",\\s*", " and "),
    type_database_grp = str_replace(
      type_database_grp,
      "Health insurance database and Survey database",
      "Health insurance database\nand Survey database"
    )
  )

plot_databases_type_73 = ggplot(grille_extract_modif, aes(x = fct_rev(fct_infreq(type_database_grp)))) +
  geom_bar( fill = fill_bar,
            color = "black",
           width = 0.6) +
  scale_y_continuous(
    breaks = seq(0, 80, by = 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Number of articles"
  ) +
  theme_minimal() +
  theme(
    axis.text.x       = element_text(size = 18, color = "black"),
    axis.text.y       = element_text(size = 20, color = "black"),
    axis.title.x      = element_text(size = 20, color = "black"),
    legend.position   = "none",
    panel.grid        = element_blank(),
    axis.line         = element_line(linewidth = 1, color = "black"),
    axis.ticks.x      = element_line(size = 1, color = "black"),
    axis.ticks.length = unit(5, "pt")
  )
plot_databases_type_73

### Barplot types of databases (x=networks, maximum of database type)
new_df <- grille_extract_modif %>%
  arrange(network, desc(nchar(type_database_grp))) %>%
  distinct(network, .keep_all = TRUE)

plot_databases_type_by_network = ggplot(new_df, aes(x = fct_rev(fct_infreq(type_database_grp)))) +
  geom_bar( fill =fill_bar,
            color = "black",
           width = 0.6) +
  scale_y_continuous(
    breaks = seq(0, 80, by = 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Number of networks"
  ) +
  theme_minimal() +
  theme(
    axis.text.x       = element_text(size = 18, color = "black"),
    axis.text.y       = element_text(size = 20, color = "black"),
    axis.title.x      = element_text(size = 20, color = "black"),
    legend.position   = "none",
    panel.grid        = element_blank(),
    axis.line         = element_line(linewidth = 1, color = "black"),
    axis.ticks.x      = element_line(size = 1, color = "black"),
    axis.ticks.length = unit(5, "pt")
  )
plot_databases_type_by_network



### Databases duration (histogram) ###

# x = articles
df_articles$time_range_database_years <- as.numeric(df_articles$time_range_database_years)
summary(df_articles$time_range_database_years)

hist_time_range_73 <- ggplot(df_articles, aes(x = time_range_database_years)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,       
    closed = "right",
    fill = fill_hist,
    color = "black",
  ) +
  labs(
    x = "Database duration (years)",
    y = "Number of articles"
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 1),
    limits = c(0, 10),
    expand = expansion(mult = c(0, 0))   
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02))
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 20, color = "black"),
    panel.grid = element_blank(),
    axis.line = element_line(size = 1, color = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black")
  )

hist_time_range_73

### x = network (maximum duration)
df_networks$time_range_database_years = as.numeric(df_networks$time_range_database_years)
summary(df_networks$time_range_database_years)

df_duree_max_simple <- df_networks %>%
  filter(
    !is.na(time_range_database_years)
  ) %>%
  group_by(network) %>%
  summarise(
    max_dur_years = max(time_range_database_years, na.rm = TRUE)
  ) %>%
  ungroup()
summary(df_duree_max_simple$max_dur_years)

hist_time_range_max_network <- ggplot(df_duree_max_simple, aes(x = max_dur_years)) +
  geom_histogram(
    binwidth = 1,
    boundary = 0,       
    closed = "right",   
    fill = fill_hist,
    color = "black",
  ) +
  labs(
    x = "Database duration (years)",
    y = "Number of articles"
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 1),
    limits = c(0, 10),
    expand = expansion(mult = c(0, 0))   
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.02))
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 20, color = "black"),
    panel.grid = element_blank(),
    axis.line = element_line(size = 1, color = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black")
  )

hist_time_range_max_network

# Arrangement avec histo

empty_space <- ggplot() + theme_void()
database_type_time_range_by_network <- ggarrange(
  plot_databases_type_by_network,
  hist_time_range_max_network,
  empty_space,                        
  nrow = 1, ncol = 3,
  widths = c(1, 1, 0.02),           
  labels = c("A", "B", "")
)

ggsave(database_type_time_range_by_network, filename = "figures/database_type_duration/database_type_duration_by_network.png", width = 40, height = 15, dpi=600, units="cm")


database_type_time_range_by_network = ggarrange(plot_databases_type_73, 
                                                hist_time_range_73, 
                                                empty_space,                        
                                                nrow = 1, ncol = 3,
                                                labels = c("A", "B"), 
                                                widths = c(1,1,0.02))
ggsave(database_type_time_range_by_network, filename = "figures/database_type_duration/database_type_duration_by_article.png", width = 40, height = 15, dpi=600, units="cm")