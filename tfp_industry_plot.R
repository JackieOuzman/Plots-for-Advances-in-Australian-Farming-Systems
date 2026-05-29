# =============================================================================
# Script:   tfp_industry_plot.R
# Purpose:  Plot unadjusted Total Factor Productivity (TFP) indices for
#           Australian broadacre agriculture by industry, 1977-78 to 2023-24.
#           Produces a publication-quality figure (PNG + PDF).
#
# Data source:
#   ABARES (2024). Australian Agricultural Productivity, 2023-24 data dashboard.
#   Australian Bureau of Agricultural and Resource Economics and Sciences,
#   Canberra, July. CC BY 4.0.
#   DOI: https://doi.org/10.25814/h05q-c151
#   Downloaded from:
#   https://www.agriculture.gov.au/abares/research-topics/productivity/
#     agricultural-productivity-estimates#download-data
#
#   Sheets used:
#     table_1  – Broadacre productivity index, All industries (col: All Australia)
#     table_2  – Broadacre productivity index, Cropping    (col: All Australia)
#     table_3  – Broadacre productivity index, Mixed       (col: All Australia)
#     table_4  – Broadacre productivity index, Sheep       (col: All Australia)
#     table_5  – Broadacre productivity index, Beef        (col: All Australia)
#   All indices are set to 100 in 1977-78 (base year).
#
# Output:
#   tfp_industry_figure.png  (300 dpi)
#   tfp_industry_figure.pdf
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

# ── 1. File path — update to your local copy ---------------------------------
data_file <- "N:/Advances in Australian Farming Systems Paper/Section 2/Farm productivity/1_Data_AustAgPrdctvty2023-24_v1.0.0.xlsx"

# ── 2. Read each sheet (All Australia column only) ----------------------------
read_tfp <- function(sheet, industry_label) {
  df <- read_excel(data_file, sheet = sheet, skip = 1, na = c("nr", "na", ""))
  df <- df[, 1:2]
  names(df) <- c("fy", "tfp")
  df <- df |>
    filter(!is.na(fy), !is.na(tfp)) |>
    mutate(
      fy       = as.character(fy),
      tfp      = as.numeric(tfp),
      industry = industry_label
    )
  df
}

tfp_all      <- read_tfp("table_1", "All broadacre")
tfp_cropping <- read_tfp("table_2", "Cropping")
tfp_mixed    <- read_tfp("table_3", "Mixed")
tfp_sheep    <- read_tfp("table_4", "Sheep")
tfp_beef     <- read_tfp("table_5", "Beef")

# ── 3. Combine and tidy -------------------------------------------------------
tfp <- bind_rows(tfp_all, tfp_cropping, tfp_mixed, tfp_sheep, tfp_beef) |>
  mutate(
    # fy is "1977-78", "1999-2000" etc — use the START year as the plot x value
    # so "2023-24" plots at x = 2023, and the axis label reads "2023-24"
    year_start = as.integer(sub("^(\\d{4}).*", "\\1", fy)),
    industry   = factor(industry,
                        levels = c("All broadacre", "Cropping", "Mixed",
                                   "Sheep", "Beef"))
  )

# ── 4. Colour palette (blues + warm terracotta for beef) ---------------------
industry_colours <- c(
  "All broadacre" = "#1A4F82",
  "Cropping"      = "#2E7BB5",
  "Mixed"         = "#6AAED6",
  "Sheep"         = "#9ECAE1",
  "Beef"          = "#B85C38"
)

industry_linetypes <- c(
  "All broadacre" = "solid",
  "Cropping"      = "solid",
  "Mixed"         = "dashed",
  "Sheep"         = "dotdash",
  "Beef"          = "solid"
)

industry_linewidths <- c(
  "All broadacre" = 1.1,
  "Cropping"      = 0.75,
  "Mixed"         = 0.75,
  "Sheep"         = 0.75,
  "Beef"          = 0.75
)

# ── 5. End-of-series labels with manual y nudges to avoid overlap ------------
end_labels <- tfp |>
  group_by(industry) |>
  filter(year_start == max(year_start)) |>
  ungroup() |>
  # Manual y offsets (tweak if needed after inspecting output)
  mutate(y_nudge = case_when(
    industry == "Cropping"      ~  0,
    industry == "All broadacre" ~  0,
    industry == "Mixed"         ~  8,
    industry == "Beef"          ~  0,
    industry == "Sheep"         ~ -8,
    TRUE                        ~  0
  ))

# ── 6. Plot ------------------------------------------------------------------
p <- ggplot(tfp, aes(x = year_start, y = tfp,
                     colour    = industry,
                     linetype  = industry,
                     linewidth = industry)) +
  
  geom_hline(yintercept = 100, colour = "#D1D5DB", linewidth = 0.4,
             linetype = "dotted") +
  
  geom_line() +
  
  # End labels with per-industry y nudge
  geom_text(
    data    = end_labels,
    aes(y   = tfp + y_nudge, label = industry),
    hjust   = 0,
    nudge_x = 0.5,
    size    = 2.7,
    fontface = "plain",
    show.legend = FALSE
  ) +
  
  scale_colour_manual(values = industry_colours) +
  scale_linetype_manual(values = industry_linetypes) +
  scale_linewidth_manual(values = industry_linewidths) +
  
  # x-axis: breaks at start years, labels as financial year strings
  scale_x_continuous(
    breaks = seq(1979, 2024, by = 5),
    labels = function(x) paste0(x, "–", substr(x + 1, 3, 4)),
    expand = expansion(mult = c(0.01, 0.17))
  ) +
  scale_y_continuous(
    breaks = seq(80, 320, by = 40),
    labels = label_number(accuracy = 1)
  ) +
  
  labs(
    title    = "Total factor productivity — Australian broadacre agriculture by industry",
    subtitle = "TFP index (1977–78 = 100); unadjusted for climate",
    x        = NULL,
    y        = "TFP index  (1977–78 = 100)",
    caption  = "Source: ABARES (2024). Australian Agricultural Productivity, 2023–24 data dashboard. CC BY 4.0. https://doi.org/10.25814/h05q-c151"
  ) +
  
  theme_minimal(base_size = 10) +
  theme(
    plot.title         = element_text(size = 11, face = "bold",   margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9,  colour = "grey40", margin = margin(b = 10)),
    plot.caption       = element_text(size = 7,  colour = "grey55", hjust = 0,
                                      margin = margin(t = 8)),
    axis.title.y       = element_text(size = 8.5, angle = 90, margin = margin(r = 6)),
    axis.text          = element_text(size = 8, colour = "grey30"),
    axis.text.x        = element_text(angle = 35, hjust = 1),
    axis.ticks         = element_line(colour = "grey70", linewidth = 0.3),
    axis.ticks.length  = unit(2.5, "pt"),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "none",
    panel.border       = element_blank(),
    axis.line.x        = element_line(colour = "grey60", linewidth = 0.4),
    axis.line.y        = element_blank(),
    plot.margin        = margin(t = 10, r = 90, b = 10, l = 10)
  ) +
  
  guides(linewidth = "none")
p
# ── 7. Save ------------------------------------------------------------------
ggsave("N:/Advances in Australian Farming Systems Paper/Section 2/Farm productivity/tfp_industry_figure.png", plot = p,
       width = 18, height = 11, units = "cm", dpi = 300, bg = "white")

