# ============================================================
# 03b_plots_C_and_D.R
# Plot C: export intensity (% of GVP) over time
# Plot D: dumbbell chart 2010 vs 2024
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

data_dir <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets"

plot_data <- readRDS(file.path(data_dir, "plot_data.rds"))

# High confidence commodities only
hc_commodities <- c("Beef", "Wheat", "Horticulture", "Wool")

hc_data <- plot_data %>%
  filter(commodity %in% hc_commodities) %>%
  mutate(
    export_pct = export_b / gvp_b * 100,
    commodity  = factor(commodity,
                        levels = c("Beef", "Wheat", "Wool", "Horticulture"))
  )

# Colours — one per commodity
colours_hc <- c(
  "Beef"         = "#2e8b57",
  "Wheat"        = "#1d6fa4",
  "Wool"         = "#808080",
  "Horticulture" = "#5a8a5a"
)

# ============================================================
# PLOT C — export intensity lines
# ============================================================
p_C <- ggplot(hc_data,
              aes(x = year, y = export_pct,
                  colour = commodity, group = commodity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 100, linetype = "dashed",
             colour = "grey60", linewidth = 0.5) +
  scale_colour_manual(values = colours_hc, name = NULL) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    limits = c(0, 120),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Export intensity of Australian agricultural commodities",
    subtitle = "Export value as a percentage of gross value of production, 2010–2024",
    x        = NULL,
    y        = "Export value (% of GVP)",
    caption  = "Source: ABARES Agricultural Commodity Statistics 2024–25.\nExport value expressed as % of farm-gate gross value of production."
  ) +
  theme_classic() +
  theme(
    legend.position    = "bottom",
    legend.key.size    = unit(0.5, "cm"),
    legend.text        = element_text(size = 9),
    plot.title         = element_text(size = 12, face = "bold"),
    plot.subtitle      = element_text(size = 10, colour = "grey40"),
    plot.caption       = element_text(size = 8,  colour = "grey40"),
    axis.text          = element_text(size = 9),
    panel.grid.major.y = element_line(colour = "grey90")
  )

p_C

ggsave(file.path(data_dir, "plot_C_export_intensity.png"),
       plot = p_C, width = 10, height = 6, dpi = 300)

cat("Plot C saved\n")


# Plot C2b — faceted stacked bar, domestic vs exported by year

# Plot C2b — absolute values, reordered: Wheat, Beef, Horticulture, Wool

p_C2b <- hc_data %>%
  select(year, commodity, domestic_b, export_b) %>%
  pivot_longer(cols = c(domestic_b, export_b),
               names_to  = "segment",
               values_to = "value_b") %>%
  mutate(
    segment   = recode(segment,
                       "domestic_b" = "Domestic",
                       "export_b"   = "Exported"),
    segment   = factor(segment, levels = c("Domestic", "Exported")),
    commodity = factor(commodity,
                       levels = c("Wheat", "Beef", "Horticulture", "Wool"))
  ) %>%
  ggplot(aes(x = year, y = value_b, fill = segment)) +
  geom_col(width = 0.8) +
  facet_wrap(~ commodity, ncol = 2, scales = "free_y") +
  scale_fill_manual(
    values = c("Domestic" = "#a8cfe0", "Exported" = "#1d6fa4"),
    name   = NULL
  ) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  scale_y_continuous(
    labels = dollar_format(prefix = "$", suffix = "b"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Gross value of production — domestic and exported portions",
    subtitle = "Selected commodities, 2010–2024",
    x        = NULL,
    y        = "Value (A$ billion, nominal)",
    caption  = "Source: ABARES Agricultural Commodity Statistics 2024–25.\nDark shading = exported portion. Light shading = domestic portion."
  ) +
  theme_classic() +
  theme(
    legend.position    = "bottom",
    legend.key.size    = unit(0.5, "cm"),
    legend.text        = element_text(size = 9),
    strip.text         = element_text(size = 10, face = "bold"),
    strip.background   = element_blank(),
    plot.title         = element_text(size = 12, face = "bold"),
    plot.subtitle      = element_text(size = 10, colour = "grey40"),
    plot.caption       = element_text(size = 8,  colour = "grey40"),
    axis.text.x        = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y        = element_text(size = 9),
    panel.grid.major.y = element_line(colour = "grey90")
  )

p_C2b

ggsave(file.path(data_dir, "plot_C2b_reordered.png"),
       plot = p_C2b, width = 10, height = 8, dpi = 300)

cat("Plot C2b saved\n")







ggsave(file.path(data_dir, "plot_C2_faceted_bars.png"),
       plot = p_C2b, width = 10, height = 8, dpi = 300)




# Plot C4 — faceted stacked bar, % domestic vs exported
# Fixed order: Wheat, Beef, Horticulture, Wool
# Same y scale across all panels
# Export % label on exported segment

p_C4 <- hc_data %>%
  select(year, commodity, domestic_b, export_b, gvp_b) %>%
  mutate(
    export_pct   = export_b   / gvp_b * 100,
    domestic_pct = domestic_b / gvp_b * 100
  ) %>%
  pivot_longer(cols = c(domestic_pct, export_pct),
               names_to  = "segment",
               values_to = "value_pct") %>%
  mutate(
    segment   = recode(segment,
                       "domestic_pct" = "Domestic",
                       "export_pct"   = "Exported"),
    segment   = factor(segment, levels = c("Domestic", "Exported")),
    commodity = factor(commodity,
                       levels = c("Wheat", "Beef", "Horticulture", "Wool"))
  ) %>%
  ggplot(aes(x = year, y = value_pct, fill = segment)) +
  geom_col(width = 0.8) +
  facet_wrap(~ commodity, ncol = 2) +
  scale_fill_manual(
    values = c("Domestic" = "#a8cfe0", "Exported" = "#1d6fa4"),
    name   = NULL
  ) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  scale_y_continuous(
    limits = c(0, 125),
    breaks = c(0, 50, 100),
    labels = c("0%", "50%", "100%"),
    expand = expansion(mult = c(0, 0))
  ) +
  
  labs(
    title    = "Gross value of production — domestic and exported portions",
    subtitle = "Selected commodities, 2010–2024",
    x        = NULL,
    y        = "Share of gross value of production (%)",
    caption  = "Source: ABARES Agricultural Commodity Statistics 2024–25.\nExport value expressed as % of farm-gate gross value of production.\nNote: export value for some commodities may differ from farm-gate GVP basis — see data notes."
  ) +
  theme_classic() +
  theme(
    legend.position    = "bottom",
    strip.text         = element_text(size = 10, face = "bold"),
    strip.background   = element_blank(),
    plot.title         = element_text(size = 12, face = "bold"),
    plot.subtitle      = element_text(size = 10, colour = "grey40"),
    plot.caption       = element_text(size = 8,  colour = "grey40"),
    axis.text.x        = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y        = element_text(size = 9),
    panel.grid.major.y = element_line(colour = "grey90")
  )
p_C4


ggsave(file.path(data_dir, "plot_C4_pct_clean.png"),
       plot = p_C4, width = 10, height = 8, dpi = 300)


# ============================================================
# PLOT D — dumbbell 2010 vs 2024
# ============================================================
dumbbell_data <- hc_data %>%
  filter(year %in% c(2010, 2024)) %>%
  select(year, commodity, gvp_b, export_b) %>%
  pivot_longer(cols = c(gvp_b, export_b),
               names_to  = "measure",
               values_to = "value_b") %>%
  mutate(
    measure = recode(measure,
                     "gvp_b"    = "Total GVP",
                     "export_b" = "Export value"),
    year = factor(year)
  )

# Segment data for connecting lines between 2010 and 2024
segment_data <- dumbbell_data %>%
  pivot_wider(names_from = year, values_from = value_b,
              names_prefix = "y") %>%
  rename(x2010 = y2010, x2024 = y2024)

p_D <- ggplot() +
  # Connecting line between 2010 and 2024
  geom_segment(data = segment_data,
               aes(x = x2010, xend = x2024,
                   y = commodity, yend = commodity,
                   colour = commodity),
               linewidth = 1, alpha = 0.4) +
  # Points for 2010 (open) and 2024 (filled)
  geom_point(data = dumbbell_data %>% filter(year == 2010),
             aes(x = value_b, y = commodity, colour = commodity),
             size = 4, shape = 21, fill = "white", stroke = 1.5) +
  geom_point(data = dumbbell_data %>% filter(year == 2024),
             aes(x = value_b, y = commodity, colour = commodity),
             size = 4, shape = 19) +
  # Labels for GVP vs export within each measure
  facet_wrap(~ measure, scales = "free_x") +
  scale_colour_manual(values = colours_hc, name = NULL) +
  scale_x_continuous(
    labels = dollar_format(prefix = "$", suffix = "b"),
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  labs(
    title    = "Change in agricultural production and export value, 2010 to 2024",
    subtitle = "Open circle = 2010, filled circle = 2024",
    x        = "Value (A$ billion, nominal)",
    y        = NULL,
    caption  = "Source: ABARES Agricultural Commodity Statistics 2024–25."
  ) +
  theme_classic() +
  theme(
    legend.position    = "none",
    strip.text         = element_text(size = 10, face = "bold"),
    strip.background   = element_blank(),
    plot.title         = element_text(size = 12, face = "bold"),
    plot.subtitle      = element_text(size = 10, colour = "grey40"),
    plot.caption       = element_text(size = 8,  colour = "grey40"),
    axis.text          = element_text(size = 9),
    panel.grid.major.x = element_line(colour = "grey90")
  )


p_D
ggsave(file.path(data_dir, "plot_D_dumbbell.png"),
       plot = p_D, width = 10, height = 5, dpi = 300)
