# Labour per hectare in Australian agriculture — state-level land area
# Data sources:
#   Workforce: ABS Census of Population and Housing 2006, 2011, 2016, 2021
#              Barr & Kancans (ABARES Research Report 20.19, 2020)
#   Land area: ABS Agricultural Commodities, Australia (Cat. 7121.0)
#              2005-06, 2010-11, 2015-16, 2020-21
#install.packages("writexl")
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(writexl)



# ---- 1. Agricultural workforce (national total, ABS Census) ----

path_workforce <- "N:/Advances in Australian Farming Systems Paper/Section 2/Labour/agricultural_workforce.xlsx"

workforce_raw <- read_excel(path_workforce, sheet = "Agricultural Workforce")

workforce <- workforce_raw %>%
  setNames(as.character(slice(., 1))) %>%
  slice(-1) %>%
  filter(State == "Total") %>%
  select(-State) %>%
  pivot_longer(everything(), names_to = "year", values_to = "workers") %>%
  mutate(year    = as.integer(year),
         workers = as.integer(workers))
r# Labour per hectare in Australian agriculture — state-level land area
# Data sources:
#   Workforce: ABS Census of Population and Housing 2006, 2011, 2016, 2021
#              Barr & Kancans (ABARES Research Report 20.19, 2020)
#   Land area: ABS Agricultural Commodities, Australia (Cat. 7121.0)
#              2005-06, 2010-11, 2015-16, 2020-21

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


# ---- 1. File paths ----

path_workforce <- "N:/Advances in Australian Farming Systems Paper/Section 2/Labour/agricultural_workforce.xlsx"
path_2006      <- "N:/Advances in Australian Farming Systems Paper/Section 2/Labour/2006_land_mang_ha_71210DO005_200506.xls"
path_2011      <- "N:/Advances in Australian Farming Systems Paper/Section 2/Labour/2010_land_mag_ha_71210do001_201011 (1).xls"
path_2016      <- "N:/Advances in Australian Farming Systems Paper/Section 2/Labour/2015_land_mang_ha_7121do001_201516 (1).xls"
path_2021      <- "N:/Advances in Australian Farming Systems Paper/Section 2/Labour/2020_land_use_ha_AGCDCASGS202021.xlsx"


# ---- 2. Agricultural workforce ----

workforce_raw <- read_excel(path_workforce, sheet = "Agricultural Workforce")

workforce <- workforce_raw %>%
  setNames(as.character(slice(., 1))) %>%
  slice(-1) %>%
  filter(State == "Total") %>%
  select(-State) %>%
  pivot_longer(everything(), names_to = "year", values_to = "workers") %>%
  mutate(year    = as.integer(year),
         workers = as.integer(workers))
# ---- 3. Land area from each ABS file ----

# 2006: Table 1, row 19 (1-indexed), "Total area of holding", values in '000 ha
raw_2006 <- read_excel(path_2006, sheet = "Table 1", col_names = FALSE)
land_2006 <- raw_2006 %>%
  filter(if_any(1, ~ grepl("Total area of holding", .x, fixed = TRUE))) %>%
  pull(10) %>%        # column 10 = Australia total
  as.numeric() * 1000 # convert '000 ha to ha

# 2011: Table_1, "Area of holding - Total area of holding (ha)", column 2 = Australia total
raw_2011 <- read_excel(path_2011, sheet = "Table_1", col_names = FALSE)
land_2011 <- raw_2011 %>%
  filter(if_any(1, ~ grepl("Total area of holding \\(ha\\)", .x))) %>%
  pull(2) %>%
  as.numeric()

# 2016: Aust. sheet, "Area of holding - Total area (ha)", column 4 = estimate
raw_2016 <- read_excel(path_2016, sheet = "Aust.", col_names = FALSE)
land_2016 <- raw_2016 %>%
  filter(if_any(3, ~ grepl("Area of holding - Total area", .x, fixed = TRUE))) %>%
  pull(4) %>%
  as.numeric()

# 2021: Table 1, commodity code AGLANDTOTLAHA_F, region label = Australia
raw_2021 <- read_excel(path_2021, sheet = "Table 1", skip = 6, col_names = FALSE)
colnames(raw_2021) <- c("region_code", "region_label", "commodity_code",
                        "commodity_desc", "estimate", "rse", "n_businesses", "n_rse")
land_2021 <- raw_2021 %>%
  filter(commodity_code == "AGLANDTOTLAHA_F", region_label == "Australia") %>%
  pull(estimate) %>%
  as.numeric()


# ---- 4. Combine land area ----

land <- data.frame(
  year    = c(2006,      2011,      2016,      2021),
  land_ha = c(land_2006, land_2011, land_2016, land_2021)
)


# ---- 5. Join and calculate ----

combined <- workforce %>%
  inner_join(land, by = "year") %>%
  mutate(
    land_mha        = land_ha / 1e6,
    workers_per_kha = workers / (land_ha / 1000)
  )

print(combined %>% select(year, workers, land_mha, workers_per_kha))

# ---- 6. Plot ----

p <- ggplot(combined, aes(x = year, y = workers_per_kha)) +
  geom_line(colour = "#2B7A6F", linewidth = 1) +
  geom_point(colour = "#1F5049", size = 3.5) +
  geom_text(aes(label = round(workers_per_kha, 2)),
            vjust = -1.2, size = 3.5, colour = "#1F5049") +
  scale_x_continuous(breaks = combined$year) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Australian agricultural workforce per 1,000 ha of farmland",
    subtitle = "Production agriculture workers (ABS Census) ÷ total area of agricultural holdings (ABS 7121.0)",
    x        = NULL,
    y        = "Workers per 1,000 ha",
    caption  = paste0(
      "Sources: ABS Census of Population and Housing 2006, 2011, 2016, 2021;\n",
      "ABS Agricultural Commodities, Australia (Cat. 7121.0), 2005-06, 2010-11, 2015-16, 2020-21.\n",
      "Note: Land area denominator is total area of agricultural holdings;\n",
      "workforce excludes services to agriculture and meat/poultry processing."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    plot.caption     = element_text(colour = "grey50", size = 8, hjust = 0),
    panel.grid.minor = element_blank()
  )

print(p)


# ---- 7. Summary table ----



summary_table <- combined %>%
  mutate(land_mha        = round(land_mha, 1),
         workers_per_kha = round(workers_per_kha, 3)) %>%
  select(Year                   = year,
         Workers                = workers,
         `Farm area (Mha)`      = land_mha,
         `Workers per 1,000 ha` = workers_per_kha)

print(summary_table)


out_dir        <- "N:/Advances in Australian Farming Systems Paper/Section 2/Labour/"

ggsave(paste0(out_dir, "labour_per_ha.png"), plot = p, width = 9, height = 5.5, dpi = 300)
write_xlsx(summary_table, paste0(out_dir, "labour_per_ha_table.xlsx"))
