# ============================================================
# NOTES: ABARES ACS 2024-25 Data Quality and Interpretation
# Prepared: June 2026
# ============================================================
#
# GENERAL
# -------
# All GVP series sourced from FarmSector1 sheet (03_ACS2024_25_FarmSectorTables)
# All export value series sourced from individual commodity files
# GVP = farm-gate gross value of production
# Export values = value at the Australian border (free on board)
# These are NOMINAL values - no inflation adjustment applied here
#
# YEAR LABELS
# -----------
# ACS uses fiscal year labels e.g. "1974-75"
# Parsed to start year integer e.g. 1974
# All series use Fiscal Year (July-June) unless noted
#
# SERIES-SPECIFIC NOTES
# ---------------------
#
# WHEAT
# - GVP from FarmSector1 col 62
# - Export value from Wheat sheet col 59 (Oct-Sep marketing year, not fiscal year)
# - Export slightly exceeded GVP in 2023 (101.8%) - acceptable given different
#   time period basis and stock drawdowns
#
# COARSE GRAINS
# - GVP not available as a standalone series in FarmSector1
#   (closest is "Grains oilseeds pulses" aggregate col 70)
# - Export value from CoarseGrains1 col 7 (total coarse grains, World, $m)
# - One year missing from export series (50 obs vs 51 for others) - not investigated
#
# WOOL
# - GVP from FarmSector1 col 95
# - Export value from Wool sheet col 79 (GREASY wool only, World, $m)
# - Col 71 (total wool inc processed) was rejected - exports exceeded GVP by
#   >50% in multiple years, indicating it captures value-added processing
# - Greasy wool export chosen as most comparable to farm-gate GVP
# - Low export % in 1990 (51.9%) reflects wool stockpile buildup under
#   reserve price scheme - historically accurate
#
# BEEF
# - GVP = cattle slaughtered (col 83) + live cattle exports (col 90) from FarmSector1
# - Export value = beef and veal world total (Beef1 col 125) +
#   total live cattle world total (Beef1 col 142), summed
# - Export % reached 95.7% in 2023 - high but consistent with record export values
#
# DAIRY
# - GVP from FarmSector1 col 96 (milk, farm gate)
# - Export value = sum of world totals for all dairy products:
#   cheese (col 18), butter (col 27), skim milk powder (col 37),
#   wholemilk powder (col 46), milk (col 47), condensed milk (col 48),
#   other powders (col 49), casein (col 50), other dairy (cols 51-53)
# - Export value EXCEEDS GVP in some years (e.g. 123.9% in 2000)
#   because GVP is raw milk farm-gate value, exports are processed
#   products (cheese, butter, powder) which are worth more at the border
#   This is expected and not a data error
#
# HORTICULTURE
# - GVP from FarmSector1 col 80 (horticulture total)
# - Export value = fruit (col 3) + nuts (col 5) + vegetables (col 8)
#   from Horticulture sheet - these are the top-level aggregates
# - Series starts 1988 for exports (not available before)
# - Export % consistently 10-19% - reflects horticulture being
#   primarily a domestic market commodity
#
# COTTON
# - GVP from FarmSector1 col 71 (cotton lint)
# - Export value from Cotton sheet col 17 (cotton lint, World, $m)
# - High variability in export % (40-133%) reflects drought-driven
#   production volatility; 2020 low (40%) was drought recovery year
# - Years where exports exceed GVP due to timing/pricing basis differences
#
# SUGAR
# - GVP from FarmSector1 col 72 (sugar cane, farm gate)
# - Export value from Sugar sheet col 24 (sugar, World, $m)
# - Export value CONSISTENTLY exceeds GVP (119-179%) because GVP is
#   raw cane value, exports are refined sugar - a processed product
#   worth considerably more than the raw cane
#   This is expected and not a data error
#
# SHEEP MEAT
# - GVP = sheep slaughtered (col 84) + lambs slaughtered (col 85) combined
#   "Sheep" alone in FarmSector1 is mutton only and appears incomplete
# - No standalone sheep meat export value series identified in available files
#   Sheep meat exports not included in export_values.rds
#
# COVERAGE GAPS
# -------------
# - Canola: GVP available (FarmSector1 col 64) but no export value $m
#   series identified that is clearly comparable - not included
# - Pulses: GVP available but no export file uploaded - not included
# - Sheep meat exports: not available in uploaded files
# - Pig meat, poultry: domestic commodities, exports minimal, not included
#
# TOTAL GVP
# ---------
# - From FarmSector1 col 19 ("Gross value of farm production")
# - Starts 1988 (37 years) vs individual commodity series from 1974
# - Used as denominator check only, not in plot_data directly
# ============================================================


# ============================================================
# 01_read_farm_sector_gvp.R
# Read ABARES ACS 2024-25 Farm Sector GVP by commodity
# Output: fs_gvp.rds
# See: 00_data_notes.R for data quality notes
# ============================================================

library(readxl)
library(dplyr)

data_dir <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets"

raw_fs <- read_excel(file.path(data_dir, "03_ACS2024_25_FarmSectorTables_v1.0.0.xlsx"),
                     sheet = "FarmSector1",
                     col_names = FALSE)

read_fs_col <- function(raw, col_index, label) {
  years  <- as.character(raw[[1]][12:nrow(raw)])
  values <- suppressWarnings(as.numeric(raw[[col_index]][12:nrow(raw)]))
  df <- data.frame(year_label = years,
                   value      = values,
                   commodity  = label,
                   stringsAsFactors = FALSE)
  df[!is.na(df$year_label) & !is.na(df$value), ]
}

fs_gvp <- bind_rows(
  read_fs_col(raw_fs, 19, "Total GVP"),
  read_fs_col(raw_fs, 62, "Wheat"),
  read_fs_col(raw_fs, 64, "Canola"),
  read_fs_col(raw_fs, 70, "Grains oilseeds pulses"),
  read_fs_col(raw_fs, 71, "Cotton lint"),
  read_fs_col(raw_fs, 72, "Sugar cane"),
  read_fs_col(raw_fs, 80, "Horticulture total"),
  read_fs_col(raw_fs, 82, "Total crops"),
  read_fs_col(raw_fs, 83, "Beef cattle"),
  read_fs_col(raw_fs, 84, "Sheep"),
  read_fs_col(raw_fs, 85, "Lambs"),
  read_fs_col(raw_fs, 90, "Live cattle exports"),
  read_fs_col(raw_fs, 91, "Live sheep exports"),
  read_fs_col(raw_fs, 94, "Livestock total"),
  read_fs_col(raw_fs, 95, "Wool"),
  read_fs_col(raw_fs, 96, "Milk"),
  read_fs_col(raw_fs, 99, "Livestock products total")
)

fs_gvp$year <- as.integer(substr(fs_gvp$year_label, 1, 4))

# Combine sheep + lambs into sheep meat
fs_gvp <- fs_gvp %>%
  bind_rows(
    fs_gvp %>%
      filter(commodity %in% c("Sheep", "Lambs")) %>%
      group_by(year, year_label) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(commodity = "Sheep meat")
  )

fs_gvp <- fs_gvp %>%
  mutate(category = case_when(
    commodity %in% c("Wheat", "Canola", "Cotton lint",
                     "Sugar cane", "Grains oilseeds pulses",
                     "Total crops")                        ~ "Crops",
    commodity %in% c("Beef cattle", "Sheep meat",
                     "Live cattle exports",
                     "Live sheep exports",
                     "Livestock total")                    ~ "Livestock - meat",
    commodity %in% c("Wool", "Milk",
                     "Livestock products total")           ~ "Livestock products",
    commodity == "Horticulture total"                      ~ "Horticulture",
    commodity == "Total GVP"                               ~ "Total"
  ))

saveRDS(fs_gvp, file.path(data_dir, "fs_gvp.rds"))
write.csv(fs_gvp, file.path(data_dir, "fs_gvp.csv"), row.names = FALSE)

cat("fs_gvp saved:", nrow(fs_gvp), "rows,",
    min(fs_gvp$year), "to", max(fs_gvp$year), "\n")
