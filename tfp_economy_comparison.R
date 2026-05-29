# =============================================================================
# Script:   tfp_economy_comparison.R
# Purpose:  Compare average annual MFP/TFP growth rates across Australian
#           broadacre agricultural industries (ABARES) and broader economy
#           industries (ABS), over a common period 2000-01 to 2023-24.
#           Produces a ranked horizontal bar chart.
#
# Data sources:
#
#   1. Agricultural industries (TFP, unadjusted):
#      ABARES (2024). Australian Agricultural Productivity, 2023-24 data
#      dashboard. CC BY 4.0. DOI: https://doi.org/10.25814/h05q-c151
#      Download: https://www.agriculture.gov.au/abares/research-topics/
#        productivity/agricultural-productivity-estimates#download-data
#      Sheets used: table_1 to table_5 (All Australia column).
#
#   2. Economy-wide industries (MFP, hours-worked basis):
#      ABS (2025). Estimates of Industry Multifactor Productivity, 2024-25.
#      Cat. no. 5260.0.55.002. Australian Bureau of Statistics, Canberra.
#      Download: https://www.abs.gov.au/statistics/industry/industry-overview/
#        estimates-industry-multifactor-productivity/latest-release
#      Under "Data downloads" > "Tables 1-19" (file: 52600550021_2025.xlsx)
#      Sheet used: "Table 1" — GVA-based MFP indexes, hours-worked basis.
#        Row 6  = financial year column headers (1989-90 … 2024-25)
#        Rows 28-43 = 16 market-sector industries (hours-worked basis)
#        Index base year: 2023-24 = 100
#
#   NOTE on comparability:
#      ABARES TFP uses a Tornqvist index from farm survey data.
#      ABS MFP uses a national accounts framework. Both measure output
#      relative to combined inputs but differ in coverage and method.
#      The comparison is indicative, not strictly like-for-like.
#      The ABS "Agriculture, forestry and fishing" series is included
#      as a bridge between the two data sources.
#
# Output:
#   tfp_economy_comparison.png  (300 dpi)
#   tfp_economy_comparison.pdf
#
# Author:   [Jackie]
# Date:     2025
# =============================================================================
# ── 0. Packages ---------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ── 1. File paths — update both to your local copies -------------------------
abares_file <- "N:/Advances in Australian Farming Systems Paper/Section 2/Farm productivity/1_Data_AustAgPrdctvty2023-24_v1.0.0.xlsx"

abs_file <- "N:/Advances in Australian Farming Systems Paper/Section 2/Farm productivity/2_52600550021_2025.xlsx"

# ── 2. ABARES ag TFP — read from file ----------------------------------------
read_tfp <- function(sheet, industry_label) {
  df <- read_excel(abares_file, sheet = sheet, skip = 1, na = c("nr", "na", ""))
  df <- df[, 1:2]
  names(df) <- c("fy", "tfp")
  df |>
    filter(!is.na(fy), !is.na(tfp)) |>
    mutate(
      fy       = as.character(fy),
      tfp      = as.numeric(tfp),
      industry = industry_label
    )
}

ag_raw <- bind_rows(
  read_tfp("table_1", "Broadacre (all)"),
  read_tfp("table_2", "Cropping"),
  read_tfp("table_3", "Mixed"),
  read_tfp("table_4", "Sheep"),
  read_tfp("table_5", "Beef")
) |>
  mutate(year_start = as.integer(sub("^(\\d{4}).*", "\\1", fy)))

# ── 3. ABS MFP — read Table 1, hours-worked basis ----------------------------
#
# File structure (confirmed from inspection):
#   Row 6  : year headers — "1989-90", "1990-91", ... "2024-25"
#   Rows 8-25 : quality-adjusted section (discard)
#   Row 27 : "Hours worked basis" section label
#   Rows 28-43: 16 market-sector industries (hours-worked basis)
#   Row 44 : "12 Selected industries" aggregate  (discard)
#   Row 45 : "16 Market sector industries" aggregate (discard)
#   Values : "na" for not available; numeric index otherwise
#   Index base: 2023-24 = 100
#   Note: all spaces in industry names are non-breaking (\u00a0)

abs_raw <- read_excel(
  abs_file,
  sheet = "Table 1",
  skip  = 5,
  na    = c("na", "")
)

abs_long <- abs_raw |>
  rename(industry = 1) |>
  filter(!is.na(industry)) |>
  # Normalise non-breaking spaces to regular spaces
  mutate(industry = gsub("\u00a0", " ", industry)) |>
  # Discard quality-adjusted section — keep only rows from "Hours worked basis" onward
  filter(cumsum(industry == "Hours worked basis") >= 1) |>
  filter(industry != "Hours worked basis") |>
  # Keep only the 16 industry rows (start with a division letter code)
  filter(grepl("^[A-NS] ", industry)) |>
  # Strip leading division code e.g. "A ", "B "
  mutate(industry = sub("^[A-NS] ", "", industry)) |>
  pivot_longer(-industry, names_to = "fy", values_to = "mfp_index") |>
  mutate(
    mfp_index  = suppressWarnings(as.numeric(mfp_index)),
    year_start = as.integer(sub("^(\\d{4}).*", "\\1", fy))
  ) |>
  filter(!is.na(mfp_index), !is.na(year_start))

# ── 4. Compute average annual growth rates, 2000-01 to 2023-24 ---------------
# Log-linear regression — consistent with ABARES methodology
period_start <- 2000
period_end   <- 2023

compute_growth <- function(df, value_col) {
  df |>
    filter(year_start >= period_start, year_start <= period_end) |>
    mutate(val = as.numeric(.data[[value_col]])) |>
    filter(!is.na(val), val > 0) |>
    group_by(industry) |>
    filter(n() >= 10) |>
    summarise(
      avg_growth = coef(lm(log(val) ~ year_start))[["year_start"]] * 100,
      .groups = "drop"
    )
}

ag_growth  <- compute_growth(ag_raw,   "tfp") |>
  mutate(source = "ABARES broadacre TFP\n(unadjusted, 2000–01 to 2023–24)")

abs_growth <- compute_growth(abs_long, "mfp_index") |>
  mutate(source = "ABS industry MFP\n(hours worked, 2000–01 to 2023–24)")

# ── 5. Combine and order -------------------------------------------------------
combined <- bind_rows(ag_growth, abs_growth) |>
  mutate(
    source   = factor(source, levels = c(
      "ABARES broadacre TFP\n(unadjusted, 2000–01 to 2023–24)",
      "ABS industry MFP\n(hours worked, 2000–01 to 2023–24)"
    )),
    industry = reorder(industry, avg_growth)
  )

# Print to console for a quick sanity check
print(combined |> arrange(desc(avg_growth)), n = Inf)

# ── 6. Plot -------------------------------------------------------------------
col_abares <- "#1A4F82"   # dark navy  — ag industries
col_abs    <- "#7BAFD4"   # mid blue   — economy industries

p <- ggplot(combined, aes(x = avg_growth, y = industry, fill = source)) +
  
  geom_col(width = 0.72) +
  geom_vline(xintercept = 0, colour = "#6B7280", linewidth = 0.6) +
  
  geom_text(
    aes(
      label = sprintf("%+.2f%%", avg_growth),
      hjust = ifelse(avg_growth >= 0, -0.12, 1.12)
    ),
    size = 2.8, colour = "grey25"
  ) +
  
  scale_fill_manual(
    values = c(
      "ABARES broadacre TFP\n(unadjusted, 2000–01 to 2023–24)" = col_abares,
      "ABS industry MFP\n(hours worked, 2000–01 to 2023–24)"   = col_abs
    ),
    name = NULL
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.05, 0.14))
  ) +
  
  labs(
    title    = "Average annual productivity growth by industry — Australia, 2000–01 to 2023–24",
    subtitle = "ABARES TFP (broadacre ag subsectors) vs ABS MFP (economy-wide industries)",
    x        = "Average annual growth rate (%)",
    y        = NULL,
    caption  = paste0(
      "Sources: ABARES (2024), Australian Agricultural Productivity 2023-24 data dashboard, ",
      "CC BY 4.0, doi:10.25814/h05q-c151;\n",
      "ABS (2025), Estimates of Industry Multifactor Productivity 2024-25, Cat. 5260.0.55.002, Table 1.\n",
      "Note: ABARES TFP and ABS MFP use different methodologies and are not strictly comparable (see script header)."
    )
  ) +
  
  theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(size = 11, face = "bold", margin = margin(b = 3)),
    plot.subtitle      = element_text(size = 8.5, colour = "grey40", margin = margin(b = 10)),
    plot.caption       = element_text(size = 6.8, colour = "grey55", hjust = 0,
                                      margin = margin(t = 8)),
    axis.text.y        = element_text(size = 8.5, colour = "grey20"),
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
    plot.margin        = margin(t = 10, r = 30, b = 10, l = 10)
  )
p


# ── 7. Save ------------------------------------------------------------------
out_dir <- "N:/Advances in Australian Farming Systems Paper/Section 2/Farm productivity/"

ggsave(file.path(out_dir, "tfp_economy_comparison.png"), plot = p,
       width = 18, height = 22, units = "cm", dpi = 300, bg = "white")

