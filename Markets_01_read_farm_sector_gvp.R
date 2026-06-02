# ============================================================
# 01_read_farm_sector_gvp.R
# Read ABARES ACS 2024-25 Farm Sector GVP by commodity
# Output: fs_gvp (long data frame, $m, fiscal year)
# ============================================================

library(readxl)
library(dplyr)

data_dir <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets"

# Read raw sheet
raw_fs <- read_excel(file.path(data_dir, "03_ACS2024_25_FarmSectorTables_v1.0.0.xlsx"),
                     sheet = "FarmSector1",
                     col_names = FALSE)

# Extract GVP series by column number (verified 2024-06-02)
# col 19 = Total GVP, col 62 = Wheat, col 64 = Canola, etc.
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

# Combine sheep + lambs into single sheep meat category
fs_gvp <- fs_gvp %>%
  bind_rows(
    fs_gvp %>%
      filter(commodity %in% c("Sheep", "Lambs")) %>%
      group_by(year, year_label) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(commodity = "Sheep meat")
  )

# Broad category grouping
fs_gvp <- fs_gvp %>%
  mutate(category = case_when(
    commodity %in% c("Wheat", "Canola", "Cotton lint",
                     "Sugar cane", "Grains oilseeds pulses",
                     "Total crops")                          ~ "Crops",
    commodity %in% c("Beef cattle", "Sheep meat",
                     "Live cattle exports",
                     "Live sheep exports",
                     "Livestock total")                      ~ "Livestock – meat",
    commodity %in% c("Wool", "Milk",
                     "Livestock products total")             ~ "Livestock products",
    commodity == "Horticulture total"                        ~ "Horticulture",
    commodity == "Total GVP"                                 ~ "Total"
  ))

cat("fs_gvp ready:", nrow(fs_gvp), "rows,",
    min(fs_gvp$year), "to", max(fs_gvp$year), "\n")

# Save output
saveRDS(fs_gvp, file.path(data_dir, "fs_gvp.rds"))
write.csv(fs_gvp, file.path(data_dir, "fs_gvp.csv"), row.names = FALSE)

cat("Saved fs_gvp.rds and fs_gvp.csv to", data_dir, "\n")
