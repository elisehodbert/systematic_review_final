# création de df_networks_unique
df_networks_unique <- df_networks %>%
  arrange(network, desc(nb_transfers_year)) %>%  # trier par réseau puis nb_facilities décroissant
  distinct(network, .keep_all = TRUE)        # ne garder qu’une ligne par réseau

is_num <- is.numeric(df_networks$nb_transfers_year)


# par article:

mvts_direct_indirect <- ggplot(df_articles, aes(x = transfer_type, y = nb_transfers_year, fill=transfer_type)) +
  geom_violin(
    alpha          = 0.9,
    color          = "black",
    width          = 0.6,
    trim           = FALSE,
    scale          = "width",
    draw_quantiles = 0.5   # ligne de médiane
  ) +
  geom_jitter(aes(color = network, shape = network),
              width = 0.2, size = 4, na.rm = TRUE) +
  scale_fill_manual(values = c("#b4c7e7", "#2f5597")) +
  scale_y_log10() +
  scales_noleg +
  labs(x = "", y = "Number of patient movements per year (log)") +
  common_theme +
  theme(
    axis.text.x  = element_text(color = "black", size = 24),
    axis.ticks.x = element_line(color = "black"),
    axis.title.x = element_text(size = 1)
  )
mvts_direct_indirect

ggsave(mvts_direct_indirect, file="figures/violin_nb_transfers_article.png", width = 30, height = 20, dpi = 600, units = "cm")

### save en ppt

library(officer)
library(rvg)

pptx <- read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(
    value = dml(ggobj = mvts_direct_indirect),                         
    location = ph_location(
      left   = 0, 
      top    = 0, 
      width  = 30/2.54, 
      height = 20/2.54
    )
  )


print(pptx, target = "figures/nb_facilities_nb_mvts/nb_mvts_articles.pptx")

# par réseau

mvts_direct_indirect <- ggplot(df_networks_unique, aes(x = transfer_type, y = nb_transfers_year, fill = transfer_type)) +
  geom_violin(
    alpha          = 0.9,
    color          = "black",
    width          = 0.6,
    trim           = FALSE,
    scale          = "width",
    draw_quantiles = 0.5   # ligne de médiane
  ) +
  geom_jitter(aes(color = network, shape = network),
              width = 0.2, size = 4, na.rm = TRUE) +
  scale_fill_manual(values = c("#b4c7e7", "#2f5597")) +
  scale_y_log10() +
  scales_noleg +
  labs(x = "", y = "Number of patient movements per year (log)") +
  common_theme +
  theme(
    axis.text.x  = element_text(color = "black", size = 24),
    axis.ticks.x = element_line(color = "black"),
    axis.title.x = element_text(size = 1)
  )
mvts_direct_indirect
ggsave(mvts_direct_indirect, file="figures/violin_nb_transfers_network.png", width = 30, height = 20, dpi = 600, units = "cm")

### save en ppt

library(officer)
library(rvg)

pptx <- read_pptx() %>% 
  add_slide(layout = "Title and Content", master = "Office Theme") %>% 
  ph_with(
    value = dml(ggobj = mvts_direct_indirect),                         
    location = ph_location(
      left   = 0, 
      top    = 0, 
      width  = 30/2.54, 
      height = 20/2.54
    )
  )

print(pptx, target = "figures/nb_facilities_nb_mvts/nb_mvts_networks.pptx")
