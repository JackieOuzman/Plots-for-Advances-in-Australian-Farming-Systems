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
# 02_read_export_values.R
# Read export values ($m) from ABARES ACS commodity files
# Output: export_values.rds
# See: 00_data_notes.R for data quality and interpretation notes
# ============================================================

library(readxl)
library(dplyr)

data_dir <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets"

fs_gvp <- readRDS(file.path(data_dir, "fs_gvp.rds"))

read_col <- function(raw, col_index, label) {
  years  <- as.character(raw[[1]][12:nrow(raw)])
  values <- suppressWarnings(as.numeric(raw[[col_index]][12:nrow(raw)]))
  df <- data.frame(year_label = years,
                   value      = values,
                   commodity  = label,
                   stringsAsFactors = FALSE)
  df[!is.na(df$year_label) & !is.na(df$value), ]
}

# --- WHEAT: col 59, World, Value, $m ---
raw_wheat <- read_excel(file.path(data_dir, "21_ACS2024_25_WheatTables_v1.0.0 (3).xlsx"),
                        sheet = "Wheat", col_names = FALSE)
wheat_exp <- read_col(raw_wheat, 59, "Wheat")
wheat_exp$year <- as.integer(substr(wheat_exp$year_label, 1, 4))

# --- COARSE GRAINS: col 7, World, Value, $m ---
raw_cg <- read_excel(file.path(data_dir, "04_ACS2024_25_CoarseGrainsTables_v1.0.0 (1).xlsx"),
                     sheet = "CoarseGrains1", col_names = FALSE)
cg_exp <- read_col(raw_cg, 7, "Coarse grains")
cg_exp$year <- as.integer(substr(cg_exp$year_label, 1, 4))

# --- WOOL: col 79, greasy only, World, Value, $m ---
raw_wool <- read_excel(file.path(data_dir, "20_ACS2024_25_WoolTables_v1.0.0.xlsx"),
                       sheet = "Wool", col_names = FALSE)
wool_exp <- read_col(raw_wool, 79, "Wool")
wool_exp$year <- as.integer(substr(wool_exp$year_label, 1, 4))

# --- BEEF: col 125 (beef and veal) + col 142 (live cattle), World, Value, $m ---
raw_beef <- read_excel(file.path(data_dir, "13_ACS2024_25_Meat-BeefTables_v1.0.0.xlsx"),
                       sheet = "Beef1", col_names = FALSE)
beef_exp_veal <- read_col(raw_beef, 125, "Beef and veal")
beef_exp_veal$year <- as.integer(substr(beef_exp_veal$year_label, 1, 4))
beef_exp_live <- read_col(raw_beef, 142, "Live cattle")
beef_exp_live$year <- as.integer(substr(beef_exp_live$year_label, 1, 4))
beef_exp_total <- bind_rows(beef_exp_veal, beef_exp_live) %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(commodity = "Beef")

# --- DAIRY: sum of all product world totals ---
raw_dairy <- read_excel(file.path(data_dir, "06_ACS2024_25_DairyTables_v1.0.0.xlsx"),
                        sheet = "Dairy1", col_names = FALSE)
dairy_exp_total <- bind_rows(
  read_col(raw_dairy, 18, "Cheese"),
  read_col(raw_dairy, 27, "Butter"),
  read_col(raw_dairy, 37, "Skim milk powder"),
  read_col(raw_dairy, 46, "Wholemilk powder"),
  read_col(raw_dairy, 47, "Milk"),
  read_col(raw_dairy, 48, "Milk condensed"),
  read_col(raw_dairy, 49, "Milk powders other"),
  read_col(raw_dairy, 50, "Casein"),
  read_col(raw_dairy, 51, "Dairy other 1"),
  read_col(raw_dairy, 52, "Dairy other 2"),
  read_col(raw_dairy, 53, "Dairy other 3")
) %>%
  mutate(year = as.integer(substr(year_label, 1, 4))) %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(commodity = "Dairy")

# --- HORTICULTURE: fruit (col 3) + nuts (col 5) + veg (col 8) ---
raw_hort <- read_excel(file.path(data_dir, "11_ACS2024_25_HorticultureTables_v1.0.0.xlsx"),
                       sheet = "Horticulture", col_names = FALSE)
hort_exp_total <- bind_rows(
  read_col(raw_hort, 3, "Fruit"),
  read_col(raw_hort, 5, "Nuts"),
  read_col(raw_hort, 8, "Vegetables")
) %>%
  mutate(year = as.integer(substr(year_label, 1, 4))) %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(commodity = "Horticulture")

# --- COTTON: col 17, World, Value, $m ---
raw_cotton <- read_excel(file.path(data_dir, "05_ACS2024_25_CottonTables_v1.0.0.xlsx"),
                         sheet = "Cotton", col_names = FALSE)
cotton_exp <- read_col(raw_cotton, 17, "Cotton")
cotton_exp$year <- as.integer(substr(cotton_exp$year_label, 1, 4))

# --- SUGAR: col 24, World, Value, $m ---
raw_sugar <- read_excel(file.path(data_dir, "19_ACS2024_25_SugarTables_v1.0.0.xlsx"),
                        sheet = "Sugar", col_names = FALSE)
sugar_exp <- read_col(raw_sugar, 24, "Sugar")
sugar_exp$year <- as.integer(substr(sugar_exp$year_label, 1, 4))

# ============================================================
# COMBINE ALL EXPORT SERIES
# ============================================================
export_values <- bind_rows(
  wheat_exp      %>% select(year, commodity, value),
  cg_exp         %>% select(year, commodity, value),
  wool_exp       %>% select(year, commodity, value),
  beef_exp_total %>% select(year, commodity, value),
  dairy_exp_total %>% select(year, commodity, value),
  hort_exp_total %>% select(year, commodity, value),
  cotton_exp     %>% select(year, commodity, value),
  sugar_exp      %>% select(year, commodity, value)
)

saveRDS(export_values, file.path(data_dir, "export_values.rds"))
write.csv(export_values, file.path(data_dir, "export_values.csv"), row.names = FALSE)

cat("export_values saved:", nrow(export_values), "rows\n")
cat("Commodities:", paste(unique(export_values$commodity), collapse = ", "), "\n")
