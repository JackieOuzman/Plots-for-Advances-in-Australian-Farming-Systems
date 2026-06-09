# =============================================================================
# Section 1: Load packages
# =============================================================================

library(tidyverse)

# =============================================================================
# Section 2: Read data
# =============================================================================

csv_path <- "N:/Advances in Australian Farming Systems Paper/Section 1/Farm size and Consolidation/fdp-national-historical (2).csv"

dat_raw <- read_csv(csv_path)

# =============================================================================
# Section 3: Filter and calculate percentage change labels
# =============================================================================

target_years    <- c(2010, 2024)
target_industry <- c("Cropping", "Mixed", "Sheep", "Beef", "Sheep-Beef")

dat_plot <- dat_raw |>
  filter(
    Variable == "Population",
    Year     %in% target_years,
    Industry %in% target_industry
  ) |>
  mutate(Year = as.integer(Year))

pct_labels <- dat_plot |>
  pivot_wider(names_from = Year, values_from = Value, names_prefix = "y") |>
  mutate(
    pct_change = (y2024 - y2010) / y2010 * 100,
    pct_label  = sprintf("%+.0f%%", pct_change),
    # Label y position: just above the taller of the two bars
    #label_y    = pmax(y2010, y2024)
    label_y    = 22000
  ) |>
  select(Industry, pct_change, pct_label, label_y)



dat_plot <- dat_plot |>
  left_join(pct_labels, by = "Industry") |>
  mutate(
    Industry = factor(Industry, levels = c(
      "Beef", "Mixed", "Cropping", "Sheep", "Sheep-Beef"
    )),
    Year = factor(Year, levels = target_years)
  )

# =============================================================================
# Section 4: Build plot
# =============================================================================

dodge_width  <- 0.35
year_colours <- c("2010" = "#2b6cb0", "2024" = "#90cdf4")

p <- ggplot(dat_plot,
            aes(x = Industry, y = Value, fill = Year)) +
  geom_col(
    position  = position_dodge(width = dodge_width),
    width     = dodge_width * 0.95,
    colour    = "white",
    linewidth = 0.3
  ) +
  geom_text(
    data = pct_labels |>
      mutate(Industry = factor(Industry, levels = c(
        "Beef", "Mixed", "Cropping", "Sheep", "Sheep-Beef"
      ))),
    aes(x = Industry, y = label_y, label = pct_label),
    inherit.aes = FALSE,
    vjust    = -0.4,
    size     = 4.5,
    colour   = "grey30",
    fontface = "bold"
  ) +
  scale_fill_manual(values = year_colours, name = NULL) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1e-3, suffix = "k"),
    limits = c(0, 26000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Number of broadacre and dairy farms, by industry",
    subtitle = "2009–10 vs 2023–24",
    x        = NULL,
    y        = "Number of farms",
    caption  = "Source: ABARES Australian Agricultural and Grazing Industries Survey (AAGIS)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "top",
    legend.direction   = "horizontal",
    legend.text        = element_text(size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(size = 13),
    axis.text.y        = element_text(size = 13),
    axis.title.y       = element_text(size = 13),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 10, colour = "grey40"),
    plot.caption       = element_text(size = 8, colour = "grey50", hjust = 0)
  )

p

################################################################################
p_clean <- ggplot(dat_plot,
                  aes(x = Industry, y = Value, fill = Year)) +
  geom_col(
    position  = position_dodge(width = dodge_width),
    width     = dodge_width * 0.95,
    colour    = "white",
    linewidth = 0.3
  ) +
  geom_text(
    data = pct_labels |>
      mutate(Industry = factor(Industry, levels = c(
        "Beef", "Mixed", "Cropping", "Sheep", "Sheep-Beef"
      ))),
    aes(x = Industry, y = label_y, label = pct_label),
    inherit.aes = FALSE,
    vjust    = -0.4,
    size     = 4.5,
    colour   = "grey30",
    fontface = "bold"
  ) +
  scale_fill_manual(values = year_colours, name = NULL) +
  scale_y_continuous(
    labels = scales::label_comma(scale = 1e-3, suffix = "k"),
    limits = c(0, 26000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = NULL, y = "Number of farms") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "top",
    legend.direction   = "horizontal",
    legend.text        = element_text(size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(size = 13),
    axis.text.y        = element_text(size = 13),
    axis.title.y       = element_text(size = 13)
  )

p_clean



# =============================================================================
# Section 5: Save outputs
# =============================================================================

output_dir <- "N:/Advances in Australian Farming Systems Paper/Section 1/Farm size and Consolidation"

ggsave(
  filename = file.path(output_dir, "fig_broadacre_farms_by_industry_ABARES.png"),
  plot     = p, width = 20, height = 12, units = "cm", dpi = 300
)


ggsave(
  filename = file.path(output_dir, "fig_broadacre_farms_by_industry_ABARES_CLEAN.png"),
  plot     = p_clean, width = 20, height = 12, units = "cm", dpi = 300
)
ggsave(
  filename = file.path(output_dir, "fig_broadacre_farms_by_industry_ABARES_CLEAN_600dpi.png"),
  plot     = p_clean, width = 20, height = 12, units = "cm", dpi = 600
)





# =============================================================================
# Diagnostics: check national CSV against ABARES Figure 13
# =============================================================================

# 1. Total farms across all industries (excl. All Broadacre) for 2010 and 2024
dat_raw |>
  filter(Variable == "Population",
         Year %in% c(2010, 2024),
         Industry != "All Broadacre") |>
  group_by(Year) |>
  summarise(total = sum(Value, na.rm = TRUE))

# 2. All industries available in the CSV - are Dairy included?
dat_raw |>
  filter(Variable == "Population", Year == 2010) |>
  select(Industry, Value) |>
  arrange(desc(Value))

# 3. Check what "All Broadacre" gives vs sum of parts
dat_raw |>
  filter(Variable == "Population", Year %in% c(2010, 2024)) |>
  group_by(Year) |>
  mutate(sum_parts = sum(Value[Industry != "All Broadacre"], na.rm = TRUE)) |>
  filter(Industry == "All Broadacre") |>
  select(Year, all_broadacre = Value, sum_of_parts = sum_parts)

# 4. Figure 13 approximate visual totals for comparison
# Beef: 2009-10 ~19,500, 2023-24 ~19,000 (roughly stable)
# Wheat and other crops: 2009-10 ~15,500, 2023-24 ~9,800
# Mixed: 2009-10 ~17,000, 2023-24 ~6,000
# Sheep: 2009-10 ~15,500, 2023-24 ~9,800
# Sheep-Beef: 2009-10 ~8,000, 2023-24 ~4,500
