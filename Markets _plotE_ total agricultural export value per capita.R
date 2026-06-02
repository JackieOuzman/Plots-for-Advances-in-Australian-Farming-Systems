# ============================================================
# Plot E — total agricultural export value per capita
# NOTE: population figures hardcoded from ABS estimates
# Replace with proper ABS download before publication
# ABS source: National, state and territory population
# Cat. 3101.0
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)

data_dir <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets"

# Reload saved data
export_values <- readRDS(file.path(data_dir, "export_values.rds"))
plot_data     <- readRDS(file.path(data_dir, "plot_data.rds"))

# Recreate hc_data (high confidence commodities)
hc_commodities <- c("Beef", "Wheat", "Horticulture", "Wool")

hc_data <- plot_data %>%
  filter(commodity %in% hc_commodities) %>%
  mutate(
    export_pct   = export_b / gvp_b * 100,
    domestic_pct = (gvp_b - export_b) / gvp_b * 100,
    commodity    = factor(commodity,
                          levels = c("Wheat", "Beef", "Horticulture", "Wool"))
  )


# ABS estimated resident population, Australia, June each year
# Source: ABS 3101.0
pop_data <- data.frame(
  year = 2010:2024,
  population_m = c(22.34, 22.62, 22.91, 23.13, 23.46,
                   23.78, 24.13, 24.49, 24.77, 25.18,
                   25.50, 25.47, 25.88, 26.27, 26.54)
)

# Total export value across all commodities in export_values
# using all available commodities not just high confidence
total_exp_yr <- export_values %>%
  group_by(year) %>%
  summarise(total_export_m = sum(value, na.rm = TRUE),
            .groups = "drop") %>%
  filter(year >= 2010)

# Join population and calculate per capita
percap_data <- total_exp_yr %>%
  left_join(pop_data, by = "year") %>%
  mutate(
    export_percap = (total_export_m * 1e6) / (population_m * 1e6),
    export_percap_k = export_percap / 1000
  )

cat("Per capita export value at spot years:\n")
percap_data %>%
  filter(year %in% c(2010, 2015, 2020, 2024)) %>%
  select(year, total_export_m, population_m, export_percap) %>%
  as.data.frame() %>%
  print()

p_E <- ggplot(percap_data,
              aes(x = year, y = export_percap)) +
  geom_area(fill = "#a8cfe0", alpha = 0.6) +
  geom_line(colour = "#1d6fa4", linewidth = 1) +
  geom_point(colour = "#1d6fa4", size = 2.5) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  scale_y_continuous(
    labels = dollar_format(prefix = "$"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Australian agricultural export value per capita",
    subtitle = "Total agricultural exports across selected commodities, 2010–2024",
    x        = NULL,
    y        = "Export value per capita (A$, nominal)",
    caption  = "Source: ABARES Agricultural Commodity Statistics 2024–25; ABS 3101.0.\nCommodities included: wheat, coarse grains, beef, wool, dairy, horticulture, cotton, sugar.\nNote: nominal values — not adjusted for inflation."
  ) +
  theme_classic() +
  theme(
    plot.title         = element_text(size = 12, face = "bold"),
    plot.subtitle      = element_text(size = 10, colour = "grey40"),
    plot.caption       = element_text(size = 8,  colour = "grey40"),
    axis.text          = element_text(size = 9),
    panel.grid.major.y = element_line(colour = "grey90")
  )

p_E

ggsave(file.path(data_dir, "plot_E_export_percap.png"),
       plot = p_E, width = 10, height = 6, dpi = 300)

cat("Plot E saved\n")