#install.packages("read.abares")
library(read.abares)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(readxl)
library(stringr)

#################################################################################
## National wheat data t/ha using ABARES_Ag_commodities
#################################################################################





path <- "N:/Advances in Australian Farming Systems Paper/Section 1/ABARES_crop_reports/02_AustCropRrt20260303_CropData_v1.0.0.xlsx"
                                                               
# Read the Wheat sheet, skipping the 11 header rows
wheat <- read_excel(
  path,
  sheet     = "Table 11",
  skip      = 9,          # skips the 6 blank rows + 3 header rows (rows 1-9)
  col_names = FALSE
) %>%
  select(
    year             = 1,   # col B — Year
    area_sown_000ha  = 2,   # col C — Wheat area '000 ha
    production_kt    = 3    # col D — Wheat production kt
  ) %>%
  filter(!is.na(year))      # drop any trailing empty rows

head(wheat)



wheat <- wheat %>%
  mutate(
    Year = as.integer(str_extract(year, "^\\d{4}")) + 1,
    `Wheat area sown (ha)` = as.integer(area_sown_000ha) * 1000,
    `Wheat produced (t)`   = as.integer(production_kt) * 1000,
    wheat_t_per_wheat_sown_ha = round(`Wheat produced (t)` / `Wheat area sown (ha)`, 2)
  ) %>%
  select(Year, `Wheat area sown (ha)`, `Wheat produced (t)`, wheat_t_per_wheat_sown_ha)


wheat <- wheat %>%
  mutate(wheat_t_per_ha_5yr_avg = round(zoo::rollmean(wheat_t_per_wheat_sown_ha, 5, fill = NA, align = "right"), 2))

names(wheat)
unique(wheat$Year)


write.csv(wheat, 
          "N:/Advances in Australian Farming Systems Paper/Section 1/ABARES_crop_reports/wheat_ABARES_est.csv", 
          row.names = FALSE)
