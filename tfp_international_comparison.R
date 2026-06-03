# =============================================================================
# Script:   tfp_international_comparison.R
# Purpose:  Two complementary figures comparing Australian agricultural TFP
#           against international comparators using USDA ERS data:
#
#           Figure 1 (Bar chart): Average annual TFP growth rates for
#             selected countries and regional aggregates, 2000-2022,
#             ranked highest to lowest with Australia highlighted.
#
#           Figure 2 (Line chart): TFP index trajectories from 2000 to 2023
#             for Australia vs selected comparator countries, rebased to
#             2000 = 100 to show relative performance from a common start.
#
# Data source:
#   USDA Economic Research Service (ERS). International Agricultural
#   Productivity. Machine-readable long-format file of TFP indices and
#   components for countries, regions, and the world, 1961-2023.
#   Updated January 2026. Public domain (CC0).
#   https://www.ers.usda.gov/data-products/international-agricultural-productivity/
#
#   Key variable used: TFP_Index (base year 2015 = 100)
#   Growth rates computed by log-linear regression over 2000-2022
#   (2023 excluded as a partial/provisional year in some series).
#   Index series rebased to 2000 = 100 for Figure 2.
#
#   NOTE on comparability with ABARES:
#     USDA ERS uses FAO/ILO source data with simplifying assumptions for
#     international consistency. ABARES uses domestic farm survey data
#     with greater detail. Both apply Tornqvist index methods. Australia's
#     ERS TFP estimates may therefore differ from ABARES figures but are
#     appropriate for cross-country comparison.
#
# Output:
#   tfp_international_bar.png / .pdf    — Figure 1 (bar chart)
#   tfp_international_lines.png / .pdf  — Figure 2 (line chart)
#
# Author:   [Jackie]
# Date:     2025
# =============================================================================

# ── 0. Packages ---------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)

# ── 1. File path — update to your local copy ---------------------------------
usda_file <- "N:/Advances in Australian Farming Systems Paper/Section 2/Farm productivity/3_AgTFPInternational2023_long.csv"

out_dir   <- "N:/Advances in Australian Farming Systems Paper/Section 2/Farm productivity/"

# ── 2. Countries and aggregates to include -----------------------------------
focal_countries <- c(
  "Australia", "New Zealand",
  "United States", "Canada",
  "Argentina", "Brazil",
  "France", "Germany", "United Kingdom",
  "China", "India"
)

focal_aggregates <- c("World", "OECD (38 countries as of 2021)")

country_labels <- c(
  "Australia"                        = "Australia",
  "New Zealand"                      = "New Zealand",
  "United States"                    = "USA",
  "Canada"                           = "Canada",
  "Argentina"                        = "Argentina",
  "Brazil"                           = "Brazil",
  "France"                           = "France",
  "Germany"                          = "Germany",
  "United Kingdom"                   = "UK",
  "China"                            = "China",
  "India"                            = "India",
  "World"                            = "World (avg)",
  "OECD (38 countries as of 2021)"   = "OECD (avg)"
)

# ── 3. Read and filter data ---------------------------------------------------
raw <- read_csv(usda_file, show_col_types = FALSE) |>
  rename(country = `Country/territory`) |>
  filter(Variable == "TFP_Index") |>
  filter(country %in% c(focal_countries, focal_aggregates)) |>
  mutate(
    year      = as.integer(Year),
    tfp_index = as.numeric(Value),
    label     = country_labels[country]
  ) |>
  filter(!is.na(tfp_index), !is.na(year))

# ── 4. Figure 1: Average annual growth rates, 2000–2023 ----------------------

## Check data range
max(raw$year)
min(raw$year)

period_start <- 2000
period_end   <- 2023

growth <- raw |>
  filter(year >= period_start, year <= period_end) |>
  group_by(country, label) |>
  filter(n() >= 15) |>
  summarise(
    avg_growth = coef(lm(log(tfp_index) ~ year))[["year"]] * 100,
    .groups = "drop"
  ) |>
  mutate(
    is_australia = country == "Australia",
    is_aggregate = country %in% focal_aggregates,
    bar_colour   = case_when(
      is_australia ~ "Australia",
      is_aggregate ~ "Aggregate",
      TRUE         ~ "Other"
    ),
    label = reorder(label, avg_growth)
  )

col_australia <- "#1A4F82"
col_other     <- "#7BAFD4"
col_aggregate <- "#B0C4D8"

p1 <- ggplot(growth, aes(x = avg_growth, y = label, fill = bar_colour)) +
  
  geom_col(width = 0.70) +
  geom_vline(xintercept = 0, colour = "#6B7280", linewidth = 0.6) +
  
  geom_col(
    data = filter(growth, is_australia),
    aes(x = avg_growth, y = label),
    width = 0.70, fill = col_australia, colour = "#0D2F52", linewidth = 0.5
  ) +
  
  geom_text(
    aes(
      label = sprintf("%+.2f%%", avg_growth),
      hjust = ifelse(avg_growth >= 0, -0.12, 1.12)
    ),
    size = 2.9, colour = "grey20"
  ) +
  
  scale_fill_manual(
    values = c(
      "Australia" = col_australia,
      "Other"     = col_other,
      "Aggregate" = col_aggregate
    ),
    labels = c(
      "Australia" = "Australia",
      "Other"     = "Comparator countries",
      "Aggregate" = "Regional/global aggregates"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.02, 0.16))
  ) +
  
  labs(
    title    = "Average annual agricultural TFP growth — international comparison, 2000–2023",
    subtitle = "Log-linear trend rate (% per year); USDA ERS TFP index, base year 2015 = 100",
    x        = "Average annual TFP growth rate (%)",
    y        = NULL,
    caption  = paste0(
      "Source: USDA Economic Research Service (2026). International Agricultural Productivity, 1961–2023.\n",
      "https://www.ers.usda.gov/data-products/international-agricultural-productivity/\n",
      "Note: Growth rates estimated by log-linear regression over 2000–2023. ",
      "OECD = 38 member countries as of 2021."
    )
  ) +
  
  theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(size = 11, face = "bold", margin = margin(b = 3)),
    plot.subtitle      = element_text(size = 8.5, colour = "grey40", margin = margin(b = 10)),
    plot.caption       = element_text(size = 6.8, colour = "grey55", hjust = 0,
                                      margin = margin(t = 8)),
    axis.text.y        = element_text(size = 9, colour = "grey20"),
    axis.text.x        = element_text(size = 8),
    axis.title.x       = element_text(size = 9, margin = margin(t = 6)),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_blank(),
    axis.line.x        = element_line(colour = "grey60", linewidth = 0.4),
    legend.position    = "top",
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.45, "cm"),
    plot.margin        = margin(t = 10, r = 40, b = 10, l = 10)
  )
p1

p1_v2 <- ggplot(
  growth |> filter(!label %in% c("India", "China", "World (avg)", "Germany")),
  aes(x = avg_growth, y = label, fill = bar_colour)
) +
  
  geom_col(width = 0.70) +
  geom_vline(xintercept = 0, colour = "#6B7280", linewidth = 0.6) +
  
  geom_col(
    data = filter(growth, is_australia, !label %in% c("India", "China", "World", "Germany")),
    aes(x = avg_growth, y = label),
    width = 0.70, fill = col_australia, colour = "#0D2F52", linewidth = 0.5
  ) +
  
  geom_text(
    aes(
      label = sprintf("%+.2f%%", avg_growth),
      hjust = ifelse(avg_growth >= 0, -0.12, 1.12)
    ),
    size = 3.5, colour = "grey20"
  ) +
  
  scale_fill_manual(
    values = c(
      "Australia" = col_australia,
      "Other"     = col_other,
      "Aggregate" = col_aggregate
    ),
    labels = c(
      "Australia" = "Australia",
      "Other"     = "Comparator countries",
      "Aggregate" = "Regional/global aggregates"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.02, 0.16)),
    limits = c(-1.5, NA)          # ← extend left so NZ bar + label shows fully
  ) +
  
  labs(
    x = "Average annual TFP growth rate (%)",
    y = NULL
  ) +
  
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y        = element_text(size = 10, colour = "grey20"),
    axis.text.x        = element_text(size = 10),
    axis.title.x       = element_text(size = 10, margin = margin(t = 6)),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.35),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_blank(),
    axis.line.x        = element_line(colour = "grey60", linewidth = 0.4),
    #legend.position    = "top",
    legend.position    = "none",
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.45, "cm"),
    plot.margin        = margin(t = 10, r = 40, b = 10, l = 10)
  )

p1_v2


ggsave(file.path(out_dir, "tfp_international_bar.png"), 
       plot = p1,
       width = 18, height = 14, units = "cm", dpi = 300, bg = "white")

ggsave(file.path(out_dir, "tfp_international_bar_CLEAN.png"), 
       plot = p1_v2,
       width = 18, height = 10, units = "cm", dpi = 300, bg = "white")

ggsave(file.path(out_dir, "tfp_international_bar_CLEAN_600dpi.png"), 
       plot = p1_v2,
       width = 18, height = 10, units = "cm", dpi = 600, bg = "white")



# ── 5. Figure 2: TFP index trajectories, rebased to 2000 = 100 ---------------
line_countries <- c("Australia", "United States", "Canada",
                    "Argentina", "Brazil", "New Zealand", "France")

line_data <- raw |>
  filter(country %in% line_countries, year >= 2000) |>
  group_by(country, label) |>
  mutate(
    base_val    = tfp_index[year == 2000],
    tfp_rebased = (tfp_index / base_val) * 100
  ) |>
  ungroup() |>
  filter(!is.na(tfp_rebased))

end_labels <- line_data |>
  group_by(country, label) |>
  filter(year == max(year)) |>
  ungroup()

line_colours <- c(
  "Australia"    = "#1A4F82",
  "USA"          = "#4A90C4",
  "Canada"       = "#7BAFD4",
  "Argentina"    = "#5B9E8E",
  "Brazil"       = "#8DC4A8",
  "New Zealand"  = "#A0B8CC",
  "World (avg)"  = "#B0B0A8"
)

line_sizes <- c(
  "Australia"    = 1.4,
  "USA"          = 0.8,
  "Canada"       = 0.8,
  "Argentina"    = 0.8,
  "Brazil"       = 0.8,
  "New Zealand"  = 0.8,
  "France"  = 0.8
)

line_types <- c(
  "Australia"    = "solid",
  "USA"          = "solid",
  "Canada"       = "dashed",
  "Argentina"    = "solid",
  "Brazil"       = "dashed",
  "New Zealand"  = "dotdash",
  "France"  = "dotted"
)
p2 <- ggplot(line_data, aes(x = year, y = tfp_rebased,
                            colour    = label,
                            linewidth = label,
                            linetype  = label)) +
  
  geom_hline(yintercept = 100, colour = "#D1D5DB", linewidth = 0.4,
             linetype = "dotted") +
  
  geom_line() +
  
  geom_text_repel(
    data           = end_labels,
    aes(label      = label),
    hjust          = 0,
    nudge_x        = 0.5,
    size           = 2.8,
    direction      = "y",
    segment.size   = 0.3,
    segment.colour = "grey60",
    show.legend    = FALSE,
    box.padding    = 0.2,
    force          = 1.5
  ) +
  
  scale_colour_manual(values = line_colours, name = NULL) +
  scale_linewidth_manual(values = line_sizes, name = NULL) +
  scale_linetype_manual(values = line_types, name = NULL) +
  
  scale_x_continuous(
    breaks = seq(2000, 2023, by = 5),
    expand = expansion(mult = c(0.01, 0.18))
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    breaks = seq(80, 200, by = 20)
  ) +
  
  labs(
    title    = "Agricultural TFP index — Australia vs international comparators, 2000–2023",
    subtitle = "Index rebased to 2000 = 100; USDA ERS TFP index (original base year 2015 = 100)",
    x        = NULL,
    y        = "TFP index  (2000 = 100)",
    caption  = paste0(
      "Source: USDA Economic Research Service (2026). International Agricultural Productivity, 1961–2023.\n",
      "https://www.ers.usda.gov/data-products/international-agricultural-productivity/\n",
      "Note: Rebased to 2000 = 100 for comparability. World = global agricultural TFP aggregate."
    )
  ) +
  
  theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(size = 11, face = "bold", margin = margin(b = 3)),
    plot.subtitle      = element_text(size = 8.5, colour = "grey40", margin = margin(b = 10)),
    plot.caption       = element_text(size = 6.8, colour = "grey55", hjust = 0,
                                      margin = margin(t = 8)),
    axis.text          = element_text(size = 8, colour = "grey30"),
    axis.title.y       = element_text(size = 9, margin = margin(r = 6)),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_blank(),
    axis.line.x        = element_line(colour = "grey60", linewidth = 0.4),
    legend.position    = "none",
    plot.margin        = margin(t = 10, r = 90, b = 10, l = 10)
  ) +
  
  guides(linewidth = "none", linetype = "none")
p2
ggsave(file.path(out_dir, "tfp_international_lines.png"), plot = p2,
       width = 18, height = 11, units = "cm", dpi = 300, bg = "white")


p2_v2 <- ggplot(line_data, aes(x = year, y = tfp_rebased,
                               colour    = label,
                               linewidth = label,
                               linetype  = label)) +
  
  geom_hline(yintercept = 100, colour = "#D1D5DB", linewidth = 0.4,
             linetype = "dotted") +
  
  geom_line() +
  
  geom_text_repel(
    data           = end_labels,
    aes(label      = label),
    hjust          = 0,
    nudge_x        = 0.5,
    size           = 2.8,
    direction      = "y",
    segment.size   = 0.3,
    segment.colour = "grey60",
    show.legend    = FALSE,
    box.padding    = 0.2,
    force          = 1.5,
    colour         = ifelse(end_labels$label == "Australia", "#0D2F52", "grey55")
  ) +
  
  scale_colour_manual(values = line_colours, name = NULL) +
  scale_linewidth_manual(values = line_sizes, name = NULL) +
  scale_linetype_manual(values = line_types, name = NULL) +
  
  scale_x_continuous(
    breaks = c(seq(2000, 2020, by = 5), 2023),   # ← adds 2023 explicitly
    expand = expansion(mult = c(0.01, 0.18))
  ) +
  scale_y_continuous(
    labels = label_number(accuracy = 1),
    breaks = seq(80, 200, by = 20)
  ) +
  
  labs(
    x = NULL,
    y = "TFP index"
  ) +
  
  theme_minimal(base_size = 10) +
  theme(
    axis.text          = element_text(size = 8, colour = "grey30"),
    axis.title.y       = element_text(size = 9, margin = margin(r = 6)),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.border       = element_blank(),
    axis.line.x        = element_line(colour = "grey60", linewidth = 0.4),
    axis.ticks.x       = element_line(colour = "grey60", linewidth = 0.4),  # ← adds ticks
    axis.ticks.length  = unit(0.2, "cm"),                                   # ← tick length
    legend.position    = "none",
    plot.margin        = margin(t = 10, r = 90, b = 10, l = 10)
  )+
  
  guides(linewidth = "none", linetype = "none")

p2_v2

ggsave(file.path(out_dir, "tfp_international_lines_CLEAN.png"), 
       plot = p2_v2,
       width = 18, height = 11, units = "cm", dpi = 300, bg = "white")

ggsave(file.path(out_dir, "tfp_international_lines_CLEAN_600dpi.png"), 
       plot = p2_v2,
       width = 18, height = 11, units = "cm", dpi = 600, bg = "white")


# Check 1 — print Australia's raw TFP index values
raw |>
  filter(country == "Australia", year >= 2000) |>
  select(year, tfp_index) |>
  print(n = Inf)

# Check 2 — print all growth rates from Figure 1
growth |>
  arrange(desc(avg_growth)) |>
  select(label, avg_growth) |>
  mutate(avg_growth = round(avg_growth, 2)) |>
  print(n = Inf)

# Check 3 — cross-check Australia's growth rate manually
# Should match roughly what ABARES reports for a similar period
raw |>
  filter(country == "Australia", year %in% c(2000, 2022)) |>
  select(year, tfp_index)
# Simple check: (end/start)^(1/22) - 1 gives compound annual growth rate
