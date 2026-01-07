# Definition of parameters
var_info <- list(
  lvl1 = list(
    title   = "Types of facilities",
    levels  = c("Hospitals", "Hospitals and other healthcare facilities"),
    labels  = c("Hospitals only", "Hospitals and other healthcare facilities"),
    palette = c("Hospitals only" = "#238b45", "Hospitals and other healthcare facilities" = "#74c476")
  ),
  lvl2 = list(
    title   = "Patient movement scale",
    levels  = c("Interfacility", "Intrafacility", "Intrafacility and interfacility"),
    labels  =  c("Interfacility", "Intrafacility", "Intrafacility and interfacility"),
    palette = c("Interfacility" = "#084594",
                "Intrafacility" = "#2171b5",
                "Intrafacility and interfacility" = "#6baed6")
  ),
  lvl3 = list(
    title   = "Type of patient transfers",
    levels  = c("Direct", "Indirect"),
    labels  = c("Direct transfers", "Indirect transfers"),
    palette = c("Direct transfers" = "#e6550d", "Indirect transfers" = "#fd8d3c")
  ),
  lvl4 = list(
    title   = "Network temporality",
    levels  = c("Static", "Temporal"),
    labels  = c("Static", "Temporal"),
    palette = c("Static" = "#8E2B88", "Temporal" = lighten("#8E2B88", 0.35))
  )
)

# Creation of df_unique, with 1 individual = 1 network
df_unique <- df_networks %>%
  group_by(network) %>%
  summarise(
    facilities_types_binary = ifelse(
      any(facilities_types_binary == "Hospitals and other healthcare facilities"),
      "Hospitals and other healthcare facilities",
      "Hospitals"
    ),
    transfer_type = ifelse(
      any(transfer_type == "Indirect"),
      "Indirect",
      "Direct"
    ),
    static_or_temporal = ifelse(
      any(static_or_temporal == "Temporal"),
      "Temporal",
      "Static"
    ),
    intra_or_inter = first(na.omit(intra_or_inter), default = NA_character_),
    max_delay_arrangé = paste(unique(max_delay_arrangé), collapse = ", "),
    .groups = "drop"
  )

convert_to_years <- function(x) { # To have the maximum per network
  x <- str_trim(x)
  if (x %in% c("", "NA", "Not specified")) return(NA_real_)
  
  if (str_detect(x, regex("year", ignore_case = TRUE))) {
    # "1 year", "1.5 years"
    as.numeric(str_extract(x, "\\d+\\.?\\d*"))
  } else if (str_detect(x, regex("day", ignore_case = TRUE))) {
    # "90 days", "10 days" -> conversion in years
    as.numeric(str_extract(x, "\\d+\\.?\\d*")) / 365
  } else if (str_detect(x, "^[0-9]+\\.?[0-9]*$")) {
    as.numeric(x)
  } else {
    NA_real_
  }
}

# Selecting the maximum value
df_unique <- df_unique %>%
  mutate(
    max_delay_years = sapply(
      str_split(max_delay_arrangé, ","),
      function(values) {
        vals <- sapply(values, convert_to_years)
        if (all(is.na(vals))) NA_real_ else max(vals, na.rm = TRUE)
      }
    )
  )

df_unique %>% count(max_delay_years)

df_counts = df_unique %>%
  rename(
    lvl1 = facilities_types_binary,
    lvl2 = intra_or_inter,
    lvl3 = transfer_type,
    lvl4 = static_or_temporal
  ) %>%
  mutate(
    lvl1 = factor(lvl1, levels = var_info$lvl1$levels, labels = var_info$lvl1$labels),
    lvl2 = factor(lvl2, levels = var_info$lvl2$levels, labels = var_info$lvl2$labels),
    lvl3 = factor(lvl3, levels = var_info$lvl3$levels, labels = var_info$lvl3$labels),
    lvl4 = factor(lvl4, levels = var_info$lvl4$levels, labels = var_info$lvl4$labels)
  ) %>%
  count(lvl1, lvl2, lvl3, lvl4, name = "n")

# Calculating angles for each ring

## Ring 1
lvl1 <- df_counts %>%
  group_by(lvl1) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(
    prop     = n / sum(n),
    start    = lag(cumsum(prop), default = 0) * 2 * pi,
    end      = cumsum(prop) * 2 * pi,
    ring     = 1,
    category = lvl1
  )

## Ring 2
lvl2 <- df_counts %>%
  group_by(lvl1, lvl2) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  left_join(select(lvl1, lvl1, parent_start = start, parent_end = end), by = "lvl1") %>%
  group_by(lvl1) %>%
  arrange(lvl2) %>%
  mutate(
    prop     = n / sum(n),
    start    = parent_start + lag(cumsum(prop), default = 0) * (parent_end - parent_start),
    end      = parent_start + cumsum(prop) * (parent_end - parent_start),
    ring     = 2,
    category = lvl2
  ) %>%
  ungroup()

## Ring 3
lvl3 <- df_counts %>%
  group_by(lvl1, lvl2, lvl3) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  left_join(select(lvl2, lvl1, lvl2, parent_start = start, parent_end = end),
            by = c("lvl1", "lvl2")) %>%
  group_by(lvl1, lvl2) %>%
  arrange(lvl3) %>%
  mutate(
    prop     = n / sum(n),
    start    = parent_start + lag(cumsum(prop), default = 0) * (parent_end - parent_start),
    end      = parent_start + cumsum(prop) * (parent_end - parent_start),
    ring     = 3,
    category = lvl3
  ) %>%
  ungroup()

## Ring 4
lvl4 <- df_counts %>%
  group_by(lvl1, lvl2, lvl3, lvl4) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  left_join(select(lvl3, lvl1, lvl2, lvl3, parent_start = start, parent_end = end),
            by = c("lvl1", "lvl2", "lvl3")) %>%
  group_by(lvl1, lvl2, lvl3) %>%
  arrange(lvl4) %>%
  mutate(
    prop     = n / sum(n),
    start    = parent_start + lag(cumsum(prop), default = 0) * (parent_end - parent_start),
    end      = parent_start + cumsum(prop) * (parent_end - parent_start),
    ring     = 4,
    category = lvl4
  ) %>%
  ungroup()

sun_df <- bind_rows(lvl1, lvl2, lvl3, lvl4)

# Constructing the sunburst
p <- ggplot()
var_levels <- c("lvl1", "lvl2", "lvl3", "lvl4")

for (i in seq_along(var_levels)) {
  var  <- var_levels[i]
  info <- var_info[[var]]
  slice <- sun_df %>% filter(ring == i)
  if (i > 1) p <- p + new_scale_fill()
  p <- p +
    geom_arc_bar(
      data   = slice,
      aes(
        x0    = 0, y0 = 0,
        r0    = ring - 1, r = ring,
        start = start, end = end,
        fill  = category
      ),
      colour = "white", size = 0.3
    ) +
    scale_fill_manual(
      name   = info$title,
      values = info$palette,
      guide  = guide_legend(
        order          = i,
        title.position = "top",
        title.hjust    = 0,
        ncol           = 1
      )
    )
}

# Plot theme
p <- p +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(
    legend.position      = c(1.02, 0.5),
    legend.justification = c("left", "center"),
    legend.box           = "vertical",
    legend.title         = element_text(size = 48, face = "bold", margin = margin(b = 10)),
    legend.text          = element_text(size = 36),
    legend.key.size      = unit(3, "cm"),
    legend.spacing       = unit(2, "lines"),
    legend.box.spacing   = unit(3, "lines"),
    plot.margin          = margin(t = 2, r = 0, b = 2, l = 0, unit = "cm")
  )

# Exporting the plot
print(p)
ggsave("figures/sunburst_hyps.tiff", p,
       width = 42, height = 22, units = "in", dpi = 600, bg="white")

doc <- read_pptx()
doc <- add_slide(doc, layout = "Blank", master = "Office Theme")
p_dml <- dml(ggobj = p)   # conversion via rvg
doc <- ph_with(doc, p_dml, location = ph_location_fullsize())
print(doc, target = "figures/sunburst_hyps.pptx")

