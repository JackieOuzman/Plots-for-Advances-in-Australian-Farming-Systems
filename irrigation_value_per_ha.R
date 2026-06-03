# =============================================================================
# SCRIPT:   irrigation_value_per_ha.R
# PURPOSE:  Demonstrate that irrigated agriculture generates disproportionate
#           value relative to its land footprint, by computing gross value of
#           production per cropped hectare for irrigated vs non-irrigated
#           agriculture in Australia, 2017-18.
#
# STATISTIC REPRODUCED:
#   "Irrigation contributes one third to the sector's value while only
#    representing 7.2 percent of the cropped area" (based on 2017-18 data).
#
# DATA SOURCES (all ABS, 2017-18 reference year):
#   FILE 1 — Irrigated area (total area watered, ha)
#     ABS cat. 4618.0 | Water Use on Australian Farms, 2017-18
#     Released: 30 April 2019
#     Local path: N:\Advances in Australian Farming Systems Paper\Section 1\
#                 Irrigation contribution\1.46180do001_201718.xls
#     Sheet used: 'Aust.' | Row 24 | "Total area watered (ha)"
#
#   FILE 2 — Total cropped area (ha)
#     ABS cat. 7121.0 | Agricultural Commodities, Australia, 2017-18
#     Released: 30 April 2019
#     Local path: N:\Advances in Australian Farming Systems Paper\Section 1\
#                 Irrigation contribution\2.71210do001_201718.xls
#     Sheet used: 'Aust.' | Row 11 | "Land use - Land mainly used for crops"
#
#   FILE 3 — Total gross value of agricultural production ($)
#     ABS cat. 7503.0 | Value of Agricultural Commodities Produced,
#                        Australia, 2017-18
#     Released: 30 April 2019
#     Local path: N:\Advances in Australian Farming Systems Paper\Section 1\
#                 Irrigation contribution\3.75030do001_201718.xls
#     Sheet used: 'Aust.' | Row 6 | "Total agriculture"
#
#   FILE 4 — Gross value of irrigated agricultural production ($)
#     ABS cat. 4610.0.55.008 | Gross Value of Irrigated Agricultural
#                               Production, 2017-18
#     Released: 31 May 2019
#     Local path: N:\Advances in Australian Farming Systems Paper\Section 1\
#                 Irrigation contribution\4.461005008do001_201718.xls
#     Sheet used: 'Aust.' | Row 6 | "Total" (GVIAP and GVAP columns)
#
# NOTE ON DERIVED STATISTIC:
#   Value per hectare is not published directly by the ABS. It is calculated
#   here by dividing gross value ($) by the relevant cropped area (ha). The
#   non-irrigated figures are the residual (total minus irrigated). Results
#   should be cited as derived from the four ABS sources listed above.
#
# OUTPUTS:
#   irrigation_value_per_ha.png  — publication-quality horizontal bar chart
#
# AUTHOR:   Jackie Ouzman
# DATE:     28/5/2026
# PROJECT:  Advances in Australian Farming Systems
# =============================================================================

library(tidyverse)
library(readxl)
library(scales)

# =============================================================================
# 1. FILE PATHS
#    Update these to match your local or network drive location.
# =============================================================================

path_water_use <- "N:/Advances in Australian Farming Systems Paper/Section 1/Irrigation contribution/1.46180do001_201718.xls"
path_ag_comm   <- "N:/Advances in Australian Farming Systems Paper/Section 1/Irrigation contribution/2.71210do001_201718.xls"
path_vacp      <- "N:/Advances in Australian Farming Systems Paper/Section 1/Irrigation contribution/3.75030do001_201718.xls"
path_gviap     <- "N:/Advances in Australian Farming Systems Paper/Section 1/Irrigation contribution/4.461005008do001_201718.xls"

# =============================================================================
# 2. READ DATA
#    Each file uses a consistent ABS layout:
#      Row 1  = header (skip)
#      Row 6+ = data (col 3 = commodity description, col 4 = estimate)
#    We read with col_names = FALSE and skip = 5 to land on the data rows,
#    then filter by the exact commodity description string.
# =============================================================================

read_abs_xls <- function(path, sheet = "Aust.", skip = 5) {
  read_xls(
    path,
    sheet     = sheet,
    skip      = skip,
    col_names = FALSE
  )
}


# -- File 1: Total area watered (ha) -----------------------------------------
raw_water <- read_abs_xls(path_water_use)

area_irrigated_ha <- raw_water |>
  filter(trimws(...3) == "Total area watered (ha)") |>
  pull(...4) |>
  as.numeric()

# -- File 2: Total land mainly used for crops (ha) ---------------------------
raw_agcomm <- read_abs_xls(path_ag_comm)

area_cropped_total_ha <- raw_agcomm |>
  filter(trimws(...3) == "Land use - Land mainly used for crops - Area (ha) (d)") |>
  pull(...4) |>
  as.numeric()

# -- File 3: Total gross value of agricultural production ($) ----------------
raw_vacp <- read_abs_xls(path_vacp)

gvap_total <- raw_vacp |>
  filter(trimws(...3) == "Total agriculture") |>
  pull(...4) |>
  as.numeric()

# -- File 4: Gross value of irrigated agricultural production ($) ------------
#    Column 4 = GVIAP ($), column 6 = GVAP ($); filter on "Total" row
raw_gviap <- read_abs_xls(path_gviap)

gviap_total <- raw_gviap |>
  filter(trimws(...3) == "Total") |>
  pull(...4) |>
  as.numeric()


# =============================================================================
# 3. DERIVE STATISTICS
# =============================================================================

area_non_irrigated_ha <- area_cropped_total_ha - area_irrigated_ha
gvap_non_irrigated    <- gvap_total - gviap_total

value_per_ha_irrigated     <- gviap_total         / area_irrigated_ha
value_per_ha_non_irrigated <- gvap_non_irrigated   / area_non_irrigated_ha

# Quick console summary for verification
cat("\n--- Data check ---\n")
cat(sprintf("GVIAP (irrigated):           $%.1f billion\n", gviap_total / 1e9))
cat(sprintf("GVAP  (total):               $%.1f billion\n", gvap_total  / 1e9))
cat(sprintf("Irrigated share of value:    %.1f%%\n",   gviap_total / gvap_total * 100))
cat(sprintf("Irrigated area:              %.2f million ha\n", area_irrigated_ha / 1e6))
cat(sprintf("Total cropped area:          %.2f million ha\n", area_cropped_total_ha / 1e6))
cat(sprintf("Irrigated share of area:     %.1f%%\n",   area_irrigated_ha / area_cropped_total_ha * 100))
cat(sprintf("Value/ha (irrigated):        $%.0f/ha\n", value_per_ha_irrigated))
cat(sprintf("Value/ha (non-irrigated):    $%.0f/ha\n", value_per_ha_non_irrigated))
cat(sprintf("Ratio:                       %.1fx\n",    value_per_ha_irrigated / value_per_ha_non_irrigated))


# =============================================================================
# 4. BUILD PLOT DATA FRAME
# =============================================================================

plot_df <- tibble(
  category     = factor(
    c("Non-irrigated", "Irrigated"),
    levels = c("Non-irrigated", "Irrigated")   # Irrigated plots on top
  ),
  value_per_ha = c(value_per_ha_non_irrigated, value_per_ha_irrigated),
  fill_col     = c("#B4B2A9", "#1D9E75")
)

# =============================================================================
# 5. PLOT
# =============================================================================

pal <- c("Irrigated" = "#1D9E75", "Non-irrigated" = "#B4B2A9")

p <- ggplot(plot_df, aes(x = value_per_ha, y = category, fill = category)) +
  geom_col(width = 0.45, show.legend = FALSE) +
  geom_text(
    aes(label = paste0("$", comma(round(value_per_ha)), " / ha")),
    hjust    = -0.1,
    size     = 3.8,
    fontface = "bold",
    colour   = "grey25"
  ) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.28)),
    labels = label_dollar(scale = 1e-3, suffix = "k", accuracy = 1)
  ) +
  labs(
    title    = "Irrigated agriculture generates ~4\u00d7 more value per hectare",
    subtitle = "Gross value of production per cropped hectare, Australia 2017\u201318",
    x        = "Gross value of production ($ / ha)",
    y        = NULL,
    caption  = paste0(
      "Sources: ABS cat. 4610.0.55.008 (Gross Value of Irrigated Agricultural Production, 2017-18);\n",
      "ABS cat. 7503.0 (Value of Agricultural Commodities Produced, 2017-18);\n",
      "ABS cat. 4618.0 (Water Use on Australian Farms, 2017-18);\n",
      "ABS cat. 7121.0 (Agricultural Commodities, Australia, 2017-18).\n",
      "Value per hectare is derived (not directly published). Non-irrigated is the residual of total minus irrigated."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13, margin = margin(b = 4)),
    plot.subtitle      = element_text(colour = "grey40", size = 10, margin = margin(b = 14)),
    plot.caption       = element_text(colour = "grey55", size = 7.5, hjust = 0,
                                      margin = margin(t = 12), lineheight = 1.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.4),
    axis.text.y        = element_text(size = 11, colour = "grey20"),
    axis.text.x        = element_text(size = 9,  colour = "grey50"),
    axis.title.x       = element_text(size = 9,  colour = "grey40", margin = margin(t = 6)),
    plot.margin        = margin(14, 20, 12, 14)
  )

print(p)


# =============================================================================
# 6. SAVE
# =============================================================================

ggsave(
  filename = "N:/Advances in Australian Farming Systems Paper/Section 1/Irrigation contribution/irrigation_value_per_ha.png",
  plot     = p,
  width    = 7,
  height   = 3.5,
  dpi      = 300,
  bg       = "white"
)

# =============================================================================
# 5.b. CLEAN
# =============================================================================

p_clean <- ggplot(plot_df, aes(x = value_per_ha, y = category, fill = category)) +
  geom_col(width = 0.45, show.legend = FALSE) +
  geom_text(
    aes(label = paste0("$", comma(round(value_per_ha)), " / ha")),
    hjust    = -0.1,
    size     = 3.8,
    fontface = "bold",
    colour   = "grey25"
  ) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.28)),
    labels = label_dollar(scale = 1e-3, suffix = "k", accuracy = 1)
  ) +
  labs(
    #title    = "Irrigated agriculture generates ~4\u00d7 more value per hectare",
    #subtitle = "Gross value of production per cropped hectare, Australia 2017\u201318",
    x        = "Gross value of production ($ / ha)",
    y        = NULL#,
    #caption  = paste0(
     # "Sources: ABS cat. 4610.0.55.008 (Gross Value of Irrigated Agricultural Production, 2017-18);\n",
     # "ABS cat. 7503.0 (Value of Agricultural Commodities Produced, 2017-18);\n",
     # "ABS cat. 4618.0 (Water Use on Australian Farms, 2017-18);\n",
     # "ABS cat. 7121.0 (Agricultural Commodities, Australia, 2017-18).\n",
     # "Value per hectare is derived (not directly published). Non-irrigated is the residual of total minus irrigated."
    #)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13, margin = margin(b = 4)),
    plot.subtitle      = element_text(colour = "grey40", size = 10, margin = margin(b = 14)),
    plot.caption       = element_text(colour = "grey55", size = 7.5, hjust = 0,
                                      margin = margin(t = 12), lineheight = 1.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.4),
    axis.text.y        = element_text(size = 11, colour = "grey20"),
    axis.text.x        = element_text(size = 9,  colour = "grey50"),
    axis.title.x       = element_text(size = 9,  colour = "grey40", margin = margin(t = 6)),
    plot.margin        = margin(14, 20, 12, 14)
  )

print(p_clean)


ggsave(
  filename = "N:/Advances in Australian Farming Systems Paper/Section 1/Irrigation contribution/irrigation_value_per_ha_CLEAN.png",
  plot     = p_clean,
  width    = 7,
  height   = 3.5,
  dpi      = 300,
  bg       = "white"
)

ggsave(
  filename = "N:/Advances in Australian Farming Systems Paper/Section 1/Irrigation contribution/irrigation_value_per_ha_CLEAN_600dpi.png",
  plot     = p_clean,
  width    = 7,
  height   = 3.5,
  dpi      = 600,
  bg       = "white"
)
