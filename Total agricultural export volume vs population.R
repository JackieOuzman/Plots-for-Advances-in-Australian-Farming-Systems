# ============================================================
# Plot F — Total agricultural export volume vs population
# Dual-axis: export volume (left, kt) + population (right)
# 1990–2024
#
# Commodities: wheat, coarse grains, cotton, beef, sugar,
#              wool, lamb, mutton, dairy
# Units: all in kt (thousand tonnes)
# Source files in:
#   N:/Advances in Australian Farming Systems Paper/Section 2/Markets/
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)
library(readxl)


# ── 0. Paths ────────────────────────────────────────────────

mkt_dir  <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets"
pop_dir  <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets/value vs pop"

path_pop     <- file.path(pop_dir, "310101.xlsx")


# ── 1. Helper: extract a single column from ABARES file ─────

read_abares_vol <- function(path, col_num, label, sheet_name = 1) {
  read_excel(path, sheet = sheet_name, col_names = FALSE, skip = 11) %>%
    select(fy_label = 1, volume = all_of(col_num)) %>%
    mutate(
      volume    = as.numeric(volume),
      year      = as.integer(substr(fy_label, 1, 4)) + 1L,
      commodity = label
    ) %>%
    filter(!is.na(volume), year >= 1990) %>%
    select(year, commodity, volume_kt = volume)
}


# ── 2. Extract each commodity ────────────────────────────────

wheat <- read_abares_vol(
  file.path(mkt_dir, "21_ACS2024_25_WheatTables_v1.0.0 (3).xlsx"),
  col_num = 5, label = "Wheat", sheet_name = "Wheat")

coarse <- read_abares_vol(
  file.path(mkt_dir, "04_ACS2024_25_CoarseGrainsTables_v1.0.0 (1).xlsx"),
  col_num = 6, label = "Coarse grains", sheet_name = "CoarseGrains1")

cotton <- read_abares_vol(
  file.path(mkt_dir, "05_ACS2024_25_CottonTables_v1.0.0.xlsx"),
  col_num = 18, label = "Cotton", sheet_name = "Cotton")

beef <- read_abares_vol(
  file.path(mkt_dir, "13_ACS2024_25_Meat-BeefTables_v1.0.0.xlsx"),
  col_num = 4, label = "Beef", sheet_name = "Beef1")

sugar <- read_abares_vol(
  file.path(mkt_dir, "19_ACS2024_25_SugarTables_v1.0.0.xlsx"),
  col_num = 11, label = "Sugar", sheet_name = "Sugar")

wool <- read_abares_vol(
  file.path(mkt_dir, "20_ACS2024_25_WoolTables_v1.0.0.xlsx"),
  col_num = 55, label = "Wool", sheet_name = "Wool")

lamb <- read_abares_vol(
  file.path(mkt_dir, "15_ACS2024_25_Meat-SheepTables_v1.0.0.xlsx"),
  col_num = 10, label = "Lamb", sheet_name = "Sheep")

mutton <- read_abares_vol(
  file.path(mkt_dir, "15_ACS2024_25_Meat-SheepTables_v1.0.0.xlsx"),
  col_num = 35, label = "Mutton", sheet_name = "Sheep")

dairy <- read_abares_vol(
  file.path(mkt_dir, "06_ACS2024_25_DairyTables_v1.0.0.xlsx"),
  col_num = 100, label = "Dairy", sheet_name = "Dairy1")


# ── 3. Combine and sum to total annual volume ────────────────

all_commodities <- bind_rows(
  wheat, coarse, cotton, beef, sugar, wool, lamb, mutton, dairy
)

total_vol_yr <- all_commodities %>%
  group_by(year) %>%
  summarise(total_vol_kt  = sum(volume_kt, na.rm = TRUE),
            total_vol_mt  = total_vol_kt / 1000,   # million tonnes
            .groups = "drop")

cat("Volume summary:\n")
total_vol_yr %>%
  filter(year %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020, 2024)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  as.data.frame() %>%
  print()


# ── 4. Population ────────────────────────────────────────────

pop_raw <- read_excel(path_pop, sheet = "Data1",
                      col_names = FALSE, skip = 10) %>%
  select(date = 1, pop_000 = 12) %>%
  mutate(
    date    = as.Date(date),
    pop_000 = as.numeric(pop_000),
    month   = as.integer(format(date, "%m")),
    year    = as.integer(format(date, "%Y"))
  ) %>%
  filter(month == 6, year >= 1990, !is.na(pop_000)) %>%
  mutate(population_m = pop_000 / 1000) %>%
  select(year, population_m)


# ── 5. Join ──────────────────────────────────────────────────

analysis_vol <- total_vol_yr %>%
  left_join(pop_raw, by = "year") %>%
  filter(!is.na(population_m))

cat("\nJoined data spot check:\n")
analysis_vol %>%
  filter(year %in% c(1990, 2000, 2010, 2020, 2024)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  as.data.frame() %>%
  print()


# ── 6. Dual-axis scaling ─────────────────────────────────────

exp_min <- 0
exp_max <- max(analysis_vol$total_vol_mt, na.rm = TRUE) * 1.15
pop_min <- 13
pop_max <- 30

scale_pop_to_exp <- function(x) {
  (x - pop_min) / (pop_max - pop_min) * (exp_max - exp_min) + exp_min
}
scale_exp_to_pop <- function(x) {
  (x - exp_min) / (exp_max - exp_min) * (pop_max - pop_min) + pop_min
}


# ── 7. Plot ──────────────────────────────────────────────────

col_exports <- "#2e8b4a"   # green
col_pop     <- "black"
yr_max      <- max(analysis_vol$year)

p_F <- ggplot(analysis_vol, aes(x = year)) +
  
  geom_area(aes(y = total_vol_mt),
            fill = col_exports, alpha = 0.12) +
  geom_line(aes(y = total_vol_mt,
                colour = "Ag export volume"),
            linewidth = 1.1) +
  geom_point(aes(y = total_vol_mt,
                 colour = "Ag export volume"),
             size = 1.5) +
  
  geom_line(aes(y = scale_pop_to_exp(population_m),
                colour = "Population"),
            linewidth = 1.0) +
  geom_point(aes(y = scale_pop_to_exp(population_m),
                 colour = "Population"),
             size = 1.5) +
  
  scale_y_continuous(
    name     = "Agricultural export volume (million tonnes)",
    limits   = c(exp_min, exp_max),
    labels   = label_number(suffix = "Mt", accuracy = 1),
    expand   = expansion(mult = c(0, 0.02)),
    sec.axis = sec_axis(
      transform = scale_exp_to_pop,
      name      = "Australian population (millions)",
      labels    = label_number(suffix = "M", accuracy = 0.1)
    )
  ) +
  scale_x_continuous(
    breaks = c(seq(1990, 2020, by = 5), yr_max),
    labels = c(seq(1990, 2020, by = 5), yr_max),
    expand = expansion(add = 0.5)
  ) +
  scale_colour_manual(
    name   = NULL,
    values = c("Ag export volume" = col_exports,
               "Population"       = col_pop)
  ) +
  
  labs(
    title    = "Australian agricultural export volumes have grown despite population pressure",
    subtitle = "Total export volume (million tonnes) and population, 1990\u20132024",
    x        = NULL,
    caption  = paste0(
      "Sources: ABARES Agricultural Commodity Statistics 2024\u201325; ABS 3101.0 series A2133251W.\n",
      "Commodities: wheat, coarse grains, beef, lamb, mutton, wool, cotton, sugar, dairy. ",
      "Horticulture excluded (no consistent volume series). All series in thousand tonnes."
    )
  ) +
  
  theme_classic() +
  theme(
    plot.title         = element_text(size = 13, face = "bold"),
    plot.subtitle      = element_text(size = 11, colour = "grey40"),
    plot.caption       = element_text(size = 9,  colour = "grey40", hjust = 0),
    axis.title.y.left  = element_text(colour = col_exports, size = 11),
    axis.title.y.right = element_text(colour = col_pop,     size = 11),
    axis.text.y.left   = element_text(colour = col_exports, size = 10),
    axis.text.y.right  = element_text(colour = col_pop,     size = 10),
    axis.text.x        = element_text(size = 10),
    legend.position    = "bottom",
    legend.text        = element_text(size = 10),
    panel.grid.major.y = element_line(colour = "grey92"),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(10, 20, 10, 10)
  )

p_F

ggsave(file.path(pop_dir, "plot_F_export_volume_vs_pop.png"),
       plot = p_F, width = 10, height = 6, dpi = 300)


################################################################################
p_F_v2 <- ggplot(analysis_vol, aes(x = year)) +
  
  geom_area(aes(y = total_vol_mt),
            fill = col_exports, alpha = 0.12) +
  geom_line(aes(y = total_vol_mt,
                colour = "Ag export volume"),
            linewidth = 1.1) +
  geom_point(aes(y = total_vol_mt,
                 colour = "Ag export volume"),
             size = 1.5) +
  
  geom_line(aes(y = scale_pop_to_exp(population_m),
                colour = "Population"),
            linewidth = 1.0) +
  geom_point(aes(y = scale_pop_to_exp(population_m),
                 colour = "Population"),
             size = 1.5) +
  
  scale_y_continuous(
    name     = "Agricultural export volume (million tonnes)",
    limits   = c(exp_min, exp_max),
    labels   = label_number(suffix = "Mt", accuracy = 1),
    expand   = expansion(mult = c(0, 0.02)),
    sec.axis = sec_axis(
      transform = scale_exp_to_pop,
      name      = "Australian population (millions)",
      labels    = label_number(suffix = "M", accuracy = 0.1)
    )
  ) +
  scale_x_continuous(
    breaks = c(seq(1990, 2020, by = 5), yr_max),
    labels = c(seq(1990, 2020, by = 5), yr_max),
    expand = expansion(add = 0.5)
  ) +
  scale_colour_manual(
    name   = NULL,
    values = c("Ag export volume" = col_exports,
               "Population"       = col_pop)
  ) +
  
  labs(title = NULL, subtitle = NULL, caption = NULL, x = NULL) +
  
  theme_classic() +
  theme(
    axis.title.y.left  = element_text(colour = col_exports, size = 11),
    axis.title.y.right = element_text(colour = col_pop,     size = 11),
    axis.text.y.left   = element_text(colour = col_exports, size = 10),
    axis.text.y.right  = element_text(colour = col_pop,     size = 10),
    axis.text.x        = element_text(size = 10),
    legend.position    = "none",
    panel.grid.major.y = element_line(colour = "grey92"),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(10, 20, 10, 10)
  )

p_F_v2

ggsave(file.path(pop_dir, "plot_F_v2_CLEAN.png"),
       plot = p_F_v2, width = 10, height = 6, dpi = 300)

ggsave(file.path(pop_dir, "plot_F_v2_CLEAN_600dpi.png"),
       plot = p_F_v2, width = 10, height = 6, dpi = 600)
