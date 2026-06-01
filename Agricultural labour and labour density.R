# ============================================================
# Figure: AFF employed persons and labour density, Australia
# 
# Labour source:
#   ABS Labour Force, Australia, Detailed (final release Mar 2026)
#   Table 04: Employed persons by Industry division (ANZSIC)
#   Cat. 6291 — https://www.abs.gov.au/statistics/labour/
#     employment-and-unemployment/labour-force-australia-detailed/
#     mar-2026/6291004.xlsx
#   Series: Agriculture, Forestry & Fishing, Trend (A84090276A)
#   Units: '000 persons, quarterly, Nov 1984 – Feb 2026
#
# Land area source:
#   ABS Agricultural Commodities, Australia (Cat. 7121.0)
#   Various annual releases; values hard-coded below with notes.
#   Interpolated years flagged in output CSV.
#
# Author: Jackie Ouzman, CSIRO Agriculture & Food
# ============================================================

library(tidyverse)
library(readxl)
library(scales)

# ---- Paths -------------------------------------------------

data_dir <- "N:/Advances in Australian Farming Systems Paper/Section 2/Labour"
lfs_file <- file.path(data_dir, "6291004.xlsx")

# ---- 1. Read Table 04, Data1 sheet -------------------------
# Structure confirmed:
#   Row 1:  industry label
#   Row 3:  series type (Trend / Seasonally Adjusted / Original)
#   Row 10: series ID
#   Row 11+: date (col 1), values (cols 2+)
#   AFF Trend = col 2 (series ID A84090276A)
#   Units: '000 persons

raw <- read_excel(lfs_file, sheet = "Data1", col_names = FALSE)
df_raw <- raw[11:nrow(raw), c(1, 2)] |>
  set_names(c("date", "aff_000"))

# Check: should be 166 rows, dates and numbers
head(df_raw)
tail(df_raw)
nrow(df_raw)

df_parsed <- df_raw |>
  mutate(
    date    = as.Date(as.numeric(date), origin = "1899-12-30"),
    aff_000 = as.numeric(aff_000),
    year    = year(date)
  )

summary(df_parsed)



df_annual <- df_parsed |>
  filter(!is.na(aff_000)) |>
  group_by(year) |>
  summarise(
    aff_persons = mean(aff_000) * 1000,
    n_quarters  = n(),
    .groups     = "drop"
  )

# Check: which years are incomplete (should be first and/or last)
filter(df_annual, n_quarters != 4)


#Step 5 — Drop incomplete years and save
df_lfs <- filter(df_annual, n_quarters == 4)

write_csv(df_lfs, file.path(data_dir, "lfs_aff_annual_trend.csv"))


# ---- 2. Land area data -------------------------------------
# Read verified values from ABS land area tracker
# Source: ABS Agricultural Commodities, Australia (Cat. 7121.0)
# various years — see tracker for individual URLs and screenshots

land_file <- "N:/Advances in Australian Farming Systems Paper/Section 2/Labour/ABS_land_area_tracker.xlsx"

land_raw <- read_excel(land_file, sheet = "Land Area Data", skip = 3) |>
  select(
    fin_year    = 1,
    year        = 2,
    land_mha    = 3,
    source      = 4
  ) |>
  filter(!is.na(year), is.numeric(year)) |>
  mutate(
    year     = as.integer(year),
    land_mha = as.numeric(land_mha)
  )

write_csv(land_raw, file.path(data_dir, "land_area_verified.csv"))


## ---- 3. Join and compute density ---------------------------

df <- df_lfs |>
  left_join(land_raw, by = "year") |>
  filter(!is.na(land_mha)) |>
  mutate(density = aff_persons / land_mha)

write_csv(df, file.path(data_dir, "ag_labour_density_combined.csv"))


p <- ggplot(df, aes(x = year, y = density)) +
  
  geom_line(colour = "#3266ad", linewidth = 0.9) +
  geom_point(colour = "#3266ad", size = 2.5, shape = 21,
             fill = "white", stroke = 1.3) +
  
  scale_x_continuous(
    breaks = seq(2010, 2022, by = 2),
    expand = expansion(mult = c(0.03, 0.03))
  ) +
  scale_y_continuous(
    name   = "Employed persons per million ha agricultural land",
    labels = label_comma(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  
  labs(
    x       = NULL,
    caption = paste0(
      "Labour: ABS Labour Force, Australia, Detailed (Cat. 6291, final release Mar 2026), ",
      "Table 04, AFF Trend series (A84090276A). Annual averages of quarterly data.\n",
      "Note: AFF includes forestry and fishing. ",
      "Land area: ABS Agricultural Commodities, Australia (Cat. 7121.0), various years — ",
      "see ABS_land_area_tracker.xlsx for sources."
    )
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_blank(),
    plot.caption         = element_text(size = 7, colour = "grey50",
                                        hjust = 0, margin = margin(t = 10)),
    plot.caption.position = "plot",
    plot.margin          = margin(t = 10, r = 15, b = 5, l = 5)
  )





# ---- 6. Save -----------------------------------------------
p
ggsave(file.path(data_dir, "fig_labour_density_aff.png"),
       plot = p, width = 18, height = 11, units = "cm", dpi = 300)







