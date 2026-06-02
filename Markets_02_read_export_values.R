
# ============================================================
# 02_read_export_values.R
# Read export values ($m) from ABARES ACS commodity files
# Output: export_values (long data frame, $m, fiscal year)
# ============================================================

library(readxl)
library(dplyr)

data_dir <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets"

# Load GVP data from script 01
fs_gvp <- readRDS(file.path(data_dir, "fs_gvp.rds"))

# Helper: scan a file for export value $m columns
find_export_cols <- function(raw) {
  results <- list()
  for (j in 1:ncol(raw)) {
    a1  <- as.character(raw[2, j, drop = TRUE])
    com <- as.character(raw[4, j, drop = TRUE])
    loc <- as.character(raw[6, j, drop = TRUE])
    mea <- as.character(raw[7, j, drop = TRUE])
    unt <- as.character(raw[8, j, drop = TRUE])
    if (grepl("Export", a1, ignore.case = TRUE) &&
        grepl("Value", mea, ignore.case = TRUE) &&
        grepl("\\$m", unt)) {
      results[[length(results)+1]] <- data.frame(
        col = j, activity1 = a1, commodity = com,
        location = loc, measure = mea, unit = unt
      )
    }
  }
  if (length(results) == 0) cat("  -- none found\n") else do.call(rbind, results)
}

# Helper: extract a single series from a raw sheet
read_col <- function(raw, col_index, label) {
  years  <- as.character(raw[[1]][12:nrow(raw)])
  values <- suppressWarnings(as.numeric(raw[[col_index]][12:nrow(raw)]))
  df <- data.frame(year_label = years,
                   value      = values,
                   commodity  = label,
                   stringsAsFactors = FALSE)
  df[!is.na(df$year_label) & !is.na(df$value), ]
}

# ============================================================
# WHEAT
# ============================================================
raw_wheat <- read_excel(file.path(data_dir, "21_ACS2024_25_WheatTables_v1.0.0 (3).xlsx"),
                        sheet = "Wheat", col_names = FALSE)
cat("=== WHEAT export value $m columns ===\n")
print(find_export_cols(raw_wheat), row.names = FALSE)

wheat_exp <- read_col(raw_wheat, 59, "Wheat")
wheat_exp$year <- as.integer(substr(wheat_exp$year_label, 1, 4))

# Check
cat("Year range:", min(wheat_exp$year), "to", max(wheat_exp$year), "\n")
cat("N rows:", nrow(wheat_exp), "\n")

cat("\nSpot check values ($m):\n")
wheat_exp %>%
  filter(year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  as.data.frame() %>%
  print()

# Sanity: export value should be less than GVP in most years
cat("\nExport vs GVP comparison:\n")
fs_gvp %>%
  filter(commodity == "Wheat", year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  select(year, gvp = value) %>%
  left_join(wheat_exp %>% select(year, export = value), by = "year") %>%
  mutate(export_pct_gvp = round(export / gvp * 100, 1)) %>%
  as.data.frame() %>%
  print()


# Wheat export value confirmed: col 59, World, Value, $m
wheat_exp <- read_col(raw_wheat, 59, "Wheat")
wheat_exp$year <- as.integer(substr(wheat_exp$year_label, 1, 4))

# ============================================================
# COARSE GRAINS
# ============================================================
raw_cg <- read_excel(file.path(data_dir, "04_ACS2024_25_CoarseGrainsTables_v1.0.0 (1).xlsx"),
                     sheet = "CoarseGrains1", col_names = FALSE)
cat("=== COARSE GRAINS export value $m columns ===\n")
print(find_export_cols(raw_cg), row.names = FALSE)

cg_exp <- read_col(raw_cg, 7, "Coarse grains")
cg_exp$year <- as.integer(substr(cg_exp$year_label, 1, 4))

cat("Year range:", min(cg_exp$year), "to", max(cg_exp$year), "\n")
cat("N rows:", nrow(cg_exp), "\n")

cat("\nSpot check values ($m):\n")
cg_exp %>%
  filter(year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  as.data.frame() %>%
  print()

# Sanity: compare export vs GVP from farm sector
# Note: farm sector has "Grains oilseeds pulses" as the closest aggregate
# so we just check coarse grains export is a plausible subset
cat("\nCoarse grains export vs wheat export for context:\n")
bind_rows(
  wheat_exp %>% filter(year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
    select(year, commodity, value),
  cg_exp %>% filter(year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
    select(year, commodity, value)
) %>%
  arrange(year, commodity) %>%
  as.data.frame() %>%
  print()
# Coarse grains export value confirmed: col 7, World, Value, $m
cg_exp <- read_col(raw_cg, 7, "Coarse grains")
cg_exp$year <- as.integer(substr(cg_exp$year_label, 1, 4))

# ============================================================
# WOOL
# ============================================================
raw_wool <- read_excel(file.path(data_dir, "20_ACS2024_25_WoolTables_v1.0.0.xlsx"),
                       sheet = "Wool", col_names = FALSE)
cat("=== WOOL export value $m columns ===\n")
print(find_export_cols(raw_wool), row.names = FALSE)
# Check rows 1-11 for both candidate columns
cat("=== WOOL col 71 metadata ===\n")
data.frame(
  row = c("Table","Activity1","Activity2","Commodity","Reporter",
          "Location","Measure","Unit","Frequency","Notes","Identifier"),
  value = as.character(raw_wool[1:11, 71, drop = TRUE])
) %>% print()

cat("\n=== WOOL col 79 metadata ===\n")
data.frame(
  row = c("Table","Activity1","Activity2","Commodity","Reporter",
          "Location","Measure","Unit","Frequency","Notes","Identifier"),
  value = as.character(raw_wool[1:11, 79, drop = TRUE])
) %>% print()
cat("=== WOOL col 71 metadata ===\n")
data.frame(
  row = c("Table","Activity1","Activity2","Commodity","Reporter",
          "Location","Measure","Unit","Frequency","Notes","Identifier"),
  value = as.character(raw_wool[1:11, 71, drop = TRUE])
) %>% print()

wool_exp <- read_col(raw_wool, 71, "Wool")
wool_exp$year <- as.integer(substr(wool_exp$year_label, 1, 4))

cat("Year range:", min(wool_exp$year), "to", max(wool_exp$year), "\n")
cat("N rows:", nrow(wool_exp), "\n")

cat("\nSpot check values ($m):\n")
wool_exp %>%
  filter(year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  as.data.frame() %>%
  print()

# Sanity: compare export vs GVP
cat("\nExport vs GVP comparison:\n")
fs_gvp %>%
  filter(commodity == "Wool", year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  select(year, gvp = value) %>%
  left_join(wool_exp %>% select(year, export = value), by = "year") %>%
  mutate(export_pct_gvp = round(export / gvp * 100, 1)) %>%
  as.data.frame() %>%
  print()
wool_exp_greasy <- read_col(raw_wool, 79, "Wool greasy")
wool_exp_greasy$year <- as.integer(substr(wool_exp_greasy$year_label, 1, 4))

cat("Col 71 (total) vs col 79 (greasy) vs GVP:\n")
fs_gvp %>%
  filter(commodity == "Wool", year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  select(year, gvp = value) %>%
  left_join(wool_exp %>% select(year, total = value), by = "year") %>%
  left_join(wool_exp_greasy %>% select(year, greasy = value), by = "year") %>%
  mutate(pct_total  = round(total  / gvp * 100, 1),
         pct_greasy = round(greasy / gvp * 100, 1)) %>%
  as.data.frame() %>%
  print()
# Wool export value confirmed: col 79, greasy, World, Value, $m
wool_exp <- read_col(raw_wool, 79, "Wool")
wool_exp$year <- as.integer(substr(wool_exp$year_label, 1, 4))

# ============================================================
# BEEF
# ============================================================
raw_beef <- read_excel(file.path(data_dir, "13_ACS2024_25_Meat-BeefTables_v1.0.0.xlsx"),
                       sheet = "Beef1", col_names = FALSE)
cat("=== BEEF export value $m columns ===\n")
print(find_export_cols(raw_beef), row.names = FALSE)
beef_exp <- read_col(raw_beef, 125, "Beef and veal")
beef_exp$year <- as.integer(substr(beef_exp$year_label, 1, 4))

live_cattle_exp <- read_col(raw_beef, 142, "Live cattle")
live_cattle_exp$year <- as.integer(substr(live_cattle_exp$year_label, 1, 4))

cat("Spot check beef and veal vs live cattle exports ($m):\n")
bind_rows(
  beef_exp %>% filter(year %in% c(1990, 2000, 2010, 2020, 2023)),
  live_cattle_exp %>% filter(year %in% c(1990, 2000, 2010, 2020, 2023))
) %>%
  arrange(year, commodity) %>%
  as.data.frame() %>%
  print()

# Combined vs GVP
cat("\nCombined beef export vs GVP:\n")
beef_combined <- bind_rows(beef_exp, live_cattle_exp) %>%
  group_by(year) %>%
  summarise(export = sum(value, na.rm = TRUE), .groups = "drop")

fs_gvp %>%
  filter(commodity %in% c("Beef cattle", "Live cattle exports"),
         year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  group_by(year) %>%
  summarise(gvp = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(beef_combined, by = "year") %>%
  mutate(export_pct_gvp = round(export / gvp * 100, 1)) %>%
  as.data.frame() %>%
  print()
# Beef export value confirmed: col 125 (beef and veal) + col 142 (live cattle), World, Value, $m
beef_exp <- read_col(raw_beef, 125, "Beef and veal")
beef_exp$year <- as.integer(substr(beef_exp$year_label, 1, 4))

live_cattle_exp <- read_col(raw_beef, 142, "Live cattle")
live_cattle_exp$year <- as.integer(substr(live_cattle_exp$year_label, 1, 4))

# Combine into single beef export series
beef_exp_total <- bind_rows(beef_exp, live_cattle_exp) %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(commodity = "Beef", year_label = paste0(year, "-", year + 1))

# ============================================================
# DAIRY
# ============================================================
raw_dairy <- read_excel(file.path(data_dir, "06_ACS2024_25_DairyTables_v1.0.0.xlsx"),
                        sheet = "Dairy1", col_names = FALSE)
cat("=== DAIRY export value $m columns ===\n")
print(find_export_cols(raw_dairy), row.names = FALSE)
dairy_cheese   <- read_col(raw_dairy, 18, "Cheese")
dairy_butter   <- read_col(raw_dairy, 27, "Butter")
dairy_smp      <- read_col(raw_dairy, 37, "Skim milk powder")
dairy_wmp      <- read_col(raw_dairy, 46, "Wholemilk powder")
dairy_milk     <- read_col(raw_dairy, 47, "Milk")
dairy_condensed <- read_col(raw_dairy, 48, "Milk condensed")
dairy_otherpow <- read_col(raw_dairy, 49, "Milk powders other")
dairy_casein   <- read_col(raw_dairy, 50, "Casein")
dairy_other1   <- read_col(raw_dairy, 51, "Dairy other 1")
dairy_other2   <- read_col(raw_dairy, 52, "Dairy other 2")
dairy_other3   <- read_col(raw_dairy, 53, "Dairy other 3")

# Add year to all
for (obj in c("dairy_cheese","dairy_butter","dairy_smp","dairy_wmp",
              "dairy_milk","dairy_condensed","dairy_otherpow",
              "dairy_casein","dairy_other1","dairy_other2","dairy_other3")) {
  x <- get(obj)
  x$year <- as.integer(substr(x$year_label, 1, 4))
  assign(obj, x)
}

# Combine and sum to total dairy export
dairy_exp_total <- bind_rows(dairy_cheese, dairy_butter, dairy_smp, dairy_wmp,
                             dairy_milk, dairy_condensed, dairy_otherpow,
                             dairy_casein, dairy_other1, dairy_other2, dairy_other3) %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(commodity = "Dairy")

# Check product breakdown at spot years to see what dominates
cat("Product breakdown at spot years ($m):\n")
bind_rows(dairy_cheese, dairy_butter, dairy_smp, dairy_wmp,
          dairy_milk, dairy_condensed, dairy_otherpow,
          dairy_casein, dairy_other1, dairy_other2, dairy_other3) %>%
  filter(year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  group_by(year, commodity) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, desc(value)) %>%
  as.data.frame() %>%
  print(n = 60)

# Compare total export vs GVP
cat("\nTotal dairy export vs GVP:\n")
fs_gvp %>%
  filter(commodity == "Milk", year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  select(year, gvp = value) %>%
  left_join(dairy_exp_total, by = "year") %>%
  mutate(export_pct_gvp = round(value / gvp * 100, 1)) %>%
  as.data.frame() %>%
  print()
# Dairy export value: sum of all product world totals
dairy_exp_total <- bind_rows(dairy_cheese, dairy_butter, dairy_smp, dairy_wmp,
                             dairy_milk, dairy_condensed, dairy_otherpow,
                             dairy_casein, dairy_other1, dairy_other2, dairy_other3) %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(commodity = "Dairy")

# ============================================================
# HORTICULTURE
# ============================================================
raw_hort <- read_excel(file.path(data_dir, "11_ACS2024_25_HorticultureTables_v1.0.0.xlsx"),
                       sheet = "Horticulture", col_names = FALSE)
cat("=== HORTICULTURE export value $m columns ===\n")
print(find_export_cols(raw_hort), row.names = FALSE)
hort_fruit <- read_col(raw_hort, 3, "Fruit")
hort_nuts  <- read_col(raw_hort, 5, "Nuts")
hort_veg   <- read_col(raw_hort, 8, "Vegetables")

for (obj in c("hort_fruit", "hort_nuts", "hort_veg")) {
  x <- get(obj)
  x$year <- as.integer(substr(x$year_label, 1, 4))
  assign(obj, x)
}

# Sum to total horticulture exports
hort_exp_total <- bind_rows(hort_fruit, hort_nuts, hort_veg) %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(commodity = "Horticulture")

# Check breakdown
cat("Fruit vs nuts vs veg at spot years ($m):\n")
bind_rows(hort_fruit, hort_nuts, hort_veg) %>%
  filter(year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  arrange(year, commodity) %>%
  as.data.frame() %>%
  print()

# Compare total export vs GVP
cat("\nTotal horticulture export vs GVP:\n")
fs_gvp %>%
  filter(commodity == "Horticulture total", year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  select(year, gvp = value) %>%
  left_join(hort_exp_total, by = "year") %>%
  mutate(export_pct_gvp = round(value / gvp * 100, 1)) %>%
  as.data.frame() %>%
  print()
# Horticulture export value confirmed: cols 3 + 5 + 8 (fruit, nuts, veg), World, Value, $m
hort_exp_total <- bind_rows(hort_fruit, hort_nuts, hort_veg) %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(commodity = "Horticulture")

# ============================================================
# COTTON
# ============================================================
raw_cotton <- read_excel(file.path(data_dir, "05_ACS2024_25_CottonTables_v1.0.0.xlsx"),
                         sheet = "Cotton", col_names = FALSE)
cat("=== COTTON export value $m columns ===\n")
print(find_export_cols(raw_cotton), row.names = FALSE)
cotton_exp <- read_col(raw_cotton, 17, "Cotton")
cotton_exp$year <- as.integer(substr(cotton_exp$year_label, 1, 4))

cat("Year range:", min(cotton_exp$year), "to", max(cotton_exp$year), "\n")

cat("\nExport vs GVP comparison:\n")
fs_gvp %>%
  filter(commodity == "Cotton lint", year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  select(year, gvp = value) %>%
  left_join(cotton_exp %>% select(year, export = value), by = "year") %>%
  mutate(export_pct_gvp = round(export / gvp * 100, 1)) %>%
  as.data.frame() %>%
  print()
# Cotton export value confirmed: col 17, World, Value, $m
cotton_exp <- read_col(raw_cotton, 17, "Cotton")
cotton_exp$year <- as.integer(substr(cotton_exp$year_label, 1, 4))

# ============================================================
# SUGAR
# ============================================================
raw_sugar <- read_excel(file.path(data_dir, "19_ACS2024_25_SugarTables_v1.0.0.xlsx"),
                        sheet = "Sugar", col_names = FALSE)
cat("=== SUGAR export value $m columns ===\n")
print(find_export_cols(raw_sugar), row.names = FALSE)
sugar_exp <- read_col(raw_sugar, 24, "Sugar")
sugar_exp$year <- as.integer(substr(sugar_exp$year_label, 1, 4))

cat("Year range:", min(sugar_exp$year), "to", max(sugar_exp$year), "\n")

cat("\nExport vs GVP comparison:\n")
fs_gvp %>%
  filter(commodity == "Sugar cane", year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  select(year, gvp = value) %>%
  left_join(sugar_exp %>% select(year, export = value), by = "year") %>%
  mutate(export_pct_gvp = round(export / gvp * 100, 1)) %>%
  as.data.frame() %>%
  print()
# Sugar export value confirmed: col 24, World, Value, $m
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

cat("=== Export values summary ===\n")
export_values %>%
  group_by(commodity) %>%
  summarise(first_year = min(year), last_year = max(year),
            n_years = n()) %>%
  as.data.frame() %>%
  print()
