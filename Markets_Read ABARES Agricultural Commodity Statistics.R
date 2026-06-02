library(readxl)
library(dplyr)

data_dir <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets"

# Step 1: scan FarmSector1 for all Production, Value, $m columns
raw_fs <- read_excel(file.path(data_dir, "03_ACS2024_25_FarmSectorTables_v1.0.0.xlsx"),
                     sheet = "FarmSector1",
                     col_names = FALSE)

# Find every column that is Production / Value / $m
results <- list()
for (j in 1:ncol(raw_fs)) {
  a1  <- as.character(raw_fs[2, j, drop = TRUE])
  a2  <- as.character(raw_fs[3, j, drop = TRUE])
  com <- as.character(raw_fs[4, j, drop = TRUE])
  mea <- as.character(raw_fs[7, j, drop = TRUE])
  unt <- as.character(raw_fs[8, j, drop = TRUE])
  
  if (grepl("Production", a1, ignore.case = TRUE) &&
      grepl("Value", mea, ignore.case = TRUE) &&
      grepl("\\$m", unt)) {
    results[[length(results)+1]] <- data.frame(
      col = j, activity2 = a2, commodity = com, measure = mea, unit = unt
    )
  }
}

fs_cols <- do.call(rbind, results)
print(fs_cols, row.names = FALSE)


# Step 3: extract the GVP series from raw_fs using correct column numbers

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

# Check 1: year range per commodity
cat("=== Year range by commodity ===\n")
fs_gvp %>%
  group_by(commodity) %>%
  summarise(first_year = min(year), last_year = max(year), n_years = n()) %>%
  print(n = 20)

# Check 2: spot check values at selected years
cat("\n=== Spot check values ($m) ===\n")
fs_gvp %>%
  filter(year %in% c(1990, 2000, 2010, 2020, 2023)) %>%
  select(year, commodity, value) %>%
  arrange(commodity, year) %>%
  as.data.frame() %>%
  print()


# Check 2: year label parsing
cat("=== Raw year labels (first and last 5) ===\n")
fs_gvp %>%
  filter(commodity == "Wheat") %>%
  arrange(year) %>%
  select(year_label, year) %>%
  slice(c(1:5, (n()-4):n())) %>%
  as.data.frame() %>%
  print()

# Any labels that failed to parse?
cat("\n=== Any unparsed years? ===\n")
fs_gvp %>%
  filter(is.na(year)) %>%
  as.data.frame() %>%
  print()

# Any duplicate year x commodity combinations?
cat("\n=== Any duplicate year x commodity? ===\n")
fs_gvp %>%
  group_by(commodity, year) %>%
  filter(n() > 1) %>%
  as.data.frame() %>%
  print()


# Check 3: compare against known ABARES published figures
# Published benchmarks (from ABARES Agricultural Commodities Reports):
# Total GVP 2022-23: ~$85b
# Wheat GVP 2022-23: ~$9.7b
# Beef GVP 2022-23: ~$13b  
# Wool GVP 2022-23: ~$3.5b
# Horticulture 2022-23: ~$17b

cat("=== 2023-24 values vs published benchmarks ===\n")
fs_gvp %>%
  filter(year == 2023) %>%
  select(commodity, value) %>%
  mutate(value_b = round(value / 1000, 1)) %>%
  arrange(desc(value)) %>%
  as.data.frame() %>%
  print()

# Also check the totals add up internally
cat("\n=== Internal consistency: do parts sum to totals? ===\n")
fs_gvp %>%
  filter(year == 2023) %>%
  select(commodity, value) %>%
  as.data.frame() -> spot

beef_sheep_lamb <- sum(spot$value[spot$commodity %in% c("Beef cattle", "Sheep", "Lambs",
                                                        "Live cattle exports", "Live sheep exports")])
livestock_total <- spot$value[spot$commodity == "Livestock total"]
cat("Beef + sheep + lambs + live exports:", round(beef_sheep_lamb/1000, 1), "$b\n")
cat("Livestock total (from data):        ", round(livestock_total/1000, 1), "$b\n")

wool_milk <- sum(spot$value[spot$commodity %in% c("Wool", "Milk")])
lvprod_total <- spot$value[spot$commodity == "Livestock products total"]
cat("\nWool + Milk:                        ", round(wool_milk/1000, 1), "$b\n")
cat("Livestock products total (from data):", round(lvprod_total/1000, 1), "$b\n")
