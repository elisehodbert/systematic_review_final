#### Definition of parameters ####
dico_metrics <- read.xlsx("dico_metrics.xlsx")

MAX_DARK <- 30
amount_lighten <- 0.9 
amount_darken  <- 0.25

palette_levels <- brewer.pal(n = 4, name = "Set2")
base_colors <- c(
  "Node-level"    = palette_levels[3],
  "Network-level" = palette_levels[4],
  "Dyad-level"    = palette_levels[1],
  "Triad-level"   = palette_levels[2]
)

#### NA count ####
sum(is.na(df_articles$descriptive.metrics_grp))

#### Preprocessing ####
metrics_list <- df_articles$descriptive.metrics_grp

split_metrics_raw <- strsplit(metrics_list, ",\\s*")
split_metrics_raw <- lapply(split_metrics_raw, trimws)

n_community <- sum(sapply(split_metrics_raw, function(x) sum(x %in% c("community detection", "clustering", "community-node sizes"), na.rm = TRUE)))
n_community

split_metrics <- lapply(split_metrics_raw, function(x) {
  x <- x[!x %in% c("community detection", "clustering", "community-node sizes")]
  
  x[x == "betweenness centrality"] <- "betweenness"
  x[x == "number of ties"] <- "number of edges"
  x[x == "transfer volume"] <- "weighted degree"
  
  x[x == "shortest path (Floyd-Warshall algorithm)"] <- "geodesic distance"
  x[x == "shortest path"] <- "geodesic distance"
  x[x == "shortest connecting path"] <- "geodesic distance"
  x[x == "shortest path distance"] <- "geodesic distance"
  x[x == "geodesic distance distance"] <- "geodesic distance"
  
  x[x == "average path length"] <- "average geodesic distance"
  x[x == "average shortest path length"] <- "average geodesic distance"
  
  x[x == "transitivity"] <- "local clustering coefficient"
  x[x == "global transitivity"] <- "global clustering coefficient"
  x[x == "average betweenness centrality"] <- "average betweenness"
  x[x == "PageRank"] <- "PageRank centrality"
  
  x[x == "weighted degree"] <- "node strength"
  x[x == "out-degree centrality"] <- "out-degree"
  x[x == "in-degree centrality"] <- "in-degree"
  x[x == "average in-degree centrality"] <- "average in-degree"
  x[x == "average out-degree centrality"] <- "average out-degree"
  
  x[x == "in-intensity"] <- "weighted in-degree"
  x[x == "out-intensity"] <- "weighted out-degree"
  x[x == "average degree centrality"] <- "average degree"
  
  x[x == "weighted edge density"] <- "weighted density"
  x[x == "unweighted betweenness"] <- "betweenness"
  x[x == "transitive ties"] <- "cyclic closure"
  
  x[x == "weighted strength"] <- "node strength"
  x[x == "strength"] <- "node strength"
  x[x == "average network node strength"] <- "average network strength"
  x[x == "in-node strength"] <- "in-strength"
  x[x == "out-node strength"] <- "out-strength"
  
  x[x == "recent receiving"] <- "receiving balance"
  x[x == "recent sending"] <- "sending balance"
  x[x == "closeness centrality"] <- "average closeness"
  x[x == "average closeness centrality"] <- "closeness"
  x[x == "connectivity"] <- "node connectivity"
  x[x == "multiple node connectivity"] <- "node connectivity"
  x[x == "network modularity"] <- "modularity"
  
  return(x)
})

all_metrics <- tolower(unlist(split_metrics))  # Convert to lowercase 
metric_frequency <- sort(table(all_metrics), decreasing = TRUE)

metric_df <- as.data.frame(metric_frequency)

metric_df <- metric_df %>%
  mutate(variation_lower = tolower(all_metrics))

dico_metrics <- dico_metrics %>%
  mutate(variation_lower = tolower(variation))

metric_df <- metric_df %>%
  mutate(variation_lower = tolower(all_metrics))

dico_metrics <- dico_metrics %>%
  mutate(variation_lower = tolower(variation))

dico_joined <- left_join(dico_metrics, metric_df, by = "variation_lower") %>%
  rename(frequency = Freq) %>%
  mutate(précisions = if_else(!is.na(précisions), "yes", NA_character_))

tab_freq <- bind_rows(dico_joined %>% select(level, measure, variation, frequency, précisions))
tab_freq <- tab_freq %>% 
  filter(!is.na(frequency)) %>%
  mutate(level = ifelse(level == "Aggregate network-level measures", "Network-level", level))

write.xlsx(tab_freq,"freq_metrics.xlsx")

top10 <- tab_freq %>%
  arrange(desc(frequency)) %>%
  filter(frequency >= 4) %>%
  #slice_head(n = 15) %>%
  mutate(variation = str_to_title(variation))


top10_col <- top10 %>%
  mutate(
    freq_capped = pmin(frequency, MAX_DARK),
    t           = rescale(freq_capped, to = c(0, 1), from = c(0, MAX_DARK)),
    base_col    = unname(base_colors[as.character(level)]),
    light_col   = lighten(base_col, amount = amount_lighten),
    dark_col    = darken(base_col, amount = amount_darken),
    fill_color  = mapply(function(lc, dc, tt) {
      rgb(colorRamp(c(lc, dc))(tt), maxColorValue = 255)
    }, light_col, dark_col, t, USE.NAMES = FALSE)
  )

#### Creation of the graph ####
treemap = ggplot(top10_col, aes(
  area = frequency,
  fill = fill_color,
  label = variation,
  subgroup = level
)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "black", size = 2) +
  geom_treemap_text(colour = "black", place = "centre", reflow = TRUE, size = 0.8, grow=TRUE) +
  scale_fill_identity() +
  theme_minimal() +
  theme(legend.position = "none")


# Legend
n_bins <- 400
limits <- tibble(level = names(base_colors)) %>%
  mutate(y = as.numeric(factor(level, levels = names(base_colors))) * 1.7)

legend_df <- limits %>%
  rowwise() %>%
  do({
    lvl <- .$level; y <- .$y
    xs <- seq(0, MAX_DARK, length.out = n_bins)
    
    col_base  <- base_colors[lvl]
    col_light <- lighten(col_base, amount = amount_lighten)
    col_dark  <- darken(col_base, amount = amount_darken)
    
    cols <- rgb(colorRamp(c(col_light, col_dark))(seq(0, 1, length.out = n_bins)),
                maxColorValue = 255)
    
    data.frame(
      level = lvl,
      x = xs,
      y = y,
      fill_color = cols,
      width = (MAX_DARK - 0) / (n_bins - 1)
    )
  }) %>%
  ungroup()

ticks_df <- limits %>%
  tidyr::expand_grid(x = c(0, MAX_DARK/2, MAX_DARK)) %>%
  mutate(label = x)

legende = ggplot() +
  geom_tile(
    data = legend_df,
    aes(x = x, y = y, fill = fill_color,
        width = width, height = 0.8),
    color = NA
  ) +
  scale_fill_identity() +
  geom_text(
    data = limits,
    aes(x = -MAX_DARK * 0.06, y = y, label = level),  
    hjust = 1, vjust = 0.5,
    size = 4
  ) +
  geom_text(
    data = ticks_df,
    aes(x = x, y = y + 0.45, label = label),
    size = 3, vjust = 0, check_overlap = TRUE
  ) +
  scale_x_continuous(
    limits = c(-MAX_DARK * 0.1, MAX_DARK),           
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(NULL, breaks = NULL,
                     expand = expansion(mult = c(0.2, 0.2))) +
  coord_cartesian(clip = "off") +                     
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(10, # top
                         400, # tight
                         10, # bottom
                         400), # left
    axis.text.x = element_blank()
  )
legende

treemap_et_legende = ggarrange(treemap, legende, 
          nrow=2, ncol=1,
          heights=c(3,1))

ggsave("figures/treemap_metrics.png",
       dpi = 600,       
       width = 14,        
       height = 8,      
       units = "in")     


