# ============================================================
# 03_build_plot_data_and_figures.R
# Merge GVP and export values, build figures
# Output: plot_data.rds, plot_A.png, plot_B.png
# See: 00_data_notes.R for data quality notes
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

data_dir <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets"

# ============================================================
# LOAD
# ============================================================
fs_gvp       <- readRDS(file.path(data_dir, "fs_gvp.rds"))
export_values <- readRDS(file.path(data_dir, "export_values.rds"))

# ============================================================
# MERGE
# ============================================================

# GVP - keep only the commodities we have export values for
# and rename to match export_values commodity labels
gvp_plot <- fs_gvp %>%
  filter(commodity %in% c("Wheat", "Canola", "Cotton lint",
                          "Sugar cane", "Horticulture total",
                          "Beef cattle", "Wool", "Milk")) %>%
  mutate(commodity = recode(commodity,
                            "Cotton lint"       = "Cotton",
                            "Sugar cane"        = "Sugar",
                            "Horticulture total" = "Horticulture",
                            "Beef cattle"       = "Beef",
                            "Milk"              = "Dairy"
  )) %>%
  select(year, commodity, gvp_m = value)

# Join export values
plot_data <- gvp_plot %>%
  left_join(export_values %>% select(year, commodity, export_m = value),
            by = c("year", "commodity")) %>%
  filter(year >= 2010)

# Total export value per year (for overlay)
total_exports <- plot_data %>%
  group_by(year) %>%
  summarise(total_export_m = sum(export_m, na.rm = TRUE),
            total_gvp_m    = sum(gvp_m,    na.rm = TRUE),
            .groups = "drop") %>%
  mutate(total_export_b = total_export_m / 1000,
         total_gvp_b    = total_gvp_m    / 1000)

# Convert to $b for plotting
plot_data <- plot_data %>%
  mutate(gvp_b    = gvp_m    / 1000,
         export_b = export_m / 1000)

saveRDS(plot_data, file.path(data_dir, "plot_data.rds"))
write.csv(plot_data, file.path(data_dir, "plot_data.csv"), row.names = FALSE)

# ============================================================
# COMMODITY ORDER AND COLOURS
# ============================================================

# Plot A - all 8 commodities
commodities_A <- c("Wheat", "Coarse grains", "Cotton", "Sugar",
                   "Beef", "Dairy", "Wool", "Horticulture")

colours_A <- c(
  "Wheat"          = "#1d6fa4",
  "Coarse grains"  = "#4a9bc9",
  "Cotton"         = "#7bbfde",
  "Sugar"          = "#a8d4ec",
  "Beef"           = "#2e8b57",
  "Dairy"          = "#6ab187",
  "Wool"           = "#9ecfb0",
  "Horticulture"   = "#b0b0b0"
)

# Plot B - high confidence commodities only
commodities_B <- c("Wheat", "Coarse grains", "Beef", "Wool", "Horticulture")

colours_B <- c(
  "Wheat"          = "#1d6fa4",
  "Coarse grains"  = "#4a9bc9",
  "Beef"           = "#2e8b57",
  "Wool"           = "#6ab187",
  "Horticulture"   = "#b0b0b0"
)

# ============================================================
# PLOT A — all commodities
# ============================================================

plot_data_A <- plot_data %>%
  filter(commodity %in% commodities_A) %>%
  mutate(commodity = factor(commodity, levels = commodities_A))

# Total export line for Plot A
exports_A <- total_exports

p_A <- ggplot() +
  # Stacked GVP areas by commodity
  geom_area(data = plot_data_A,
            aes(x = year, y = gvp_b, fill = commodity),
            position = "stack", alpha = 0.85) +
  # Total export value as shaded area
  geom_ribbon(data = exports_A,
              aes(x = year, ymin = 0, ymax = total_export_b),
              fill = "grey20", alpha = 0.25) +
  # Total export value line
  geom_line(data = exports_A,
            aes(x = year, y = total_export_b),
            colour = "grey20", linewidth = 0.8, linetype = "dashed") +
  scale_fill_manual(values = colours_A, name = NULL) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "b"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Gross value of agricultural production and exports",
    subtitle = "Selected commodities, 2010–2024",
    x        = NULL,
    y        = "Value (A$ billion, nominal)",
    caption  = "Source: ABARES Agricultural Commodity Statistics 2024-25.\nShaded area and dashed line indicate total export value across commodities shown."
  ) +
  theme_classic() +
  theme(
    legend.position   = "bottom",
    legend.key.size   = unit(0.4, "cm"),
    plot.title        = element_text(size = 12, face = "bold"),
    plot.subtitle     = element_text(size = 10, colour = "grey40"),
    plot.caption      = element_text(size = 8,  colour = "grey40"),
    axis.text         = element_text(size = 9),
    panel.grid.major.y = element_line(colour = "grey90")
  )
p_A


ggsave(file.path(data_dir, "plot_A_all_commodities.png"),
       plot = p_A, width = 10, height = 6, dpi = 300)

cat("Plot A saved\n")

# ============================================================
# PLOT B — high confidence commodities only
# ============================================================

plot_data_B <- plot_data %>%
  filter(commodity %in% commodities_B) %>%
  mutate(commodity = factor(commodity, levels = commodities_B))

# Recalculate export total for Plot B commodities only
exports_B <- plot_data_B %>%
  group_by(year) %>%
  summarise(total_export_b = sum(export_b, na.rm = TRUE),
            total_gvp_b    = sum(gvp_b,    na.rm = TRUE),
            .groups = "drop")

p_B <- ggplot() +
  geom_area(data = plot_data_B,
            aes(x = year, y = gvp_b, fill = commodity),
            position = "stack", alpha = 0.85) +
  geom_ribbon(data = exports_B,
              aes(x = year, ymin = 0, ymax = total_export_b),
              fill = "grey20", alpha = 0.25) +
  geom_line(data = exports_B,
            aes(x = year, y = total_export_b),
            colour = "grey20", linewidth = 0.8, linetype = "dashed") +
  scale_fill_manual(values = colours_B, name = NULL) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = "b"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Gross value of agricultural production and exports",
    subtitle = "High confidence commodities, 2010–2024",
    x        = NULL,
    y        = "Value (A$ billion, nominal)",
    caption  = "Source: ABARES Agricultural Commodity Statistics 2024-25.\nShaded area and dashed line indicate total export value across commodities shown.\nCommodities selected where export series is directly comparable to farm-gate GVP."
  ) +
  theme_classic() +
  theme(
    legend.position    = "bottom",
    legend.key.size    = unit(0.4, "cm"),
    plot.title         = element_text(size = 12, face = "bold"),
    plot.subtitle      = element_text(size = 10, colour = "grey40"),
    plot.caption       = element_text(size = 8,  colour = "grey40"),
    axis.text          = element_text(size = 9),
    panel.grid.major.y = element_line(colour = "grey90")
  )

p_B

ggsave(file.path(data_dir, "plot_B_high_confidence.png"),
       plot = p_B, width = 10, height = 6, dpi = 300)

cat("Plot B saved\n")