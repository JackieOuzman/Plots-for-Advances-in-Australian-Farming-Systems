# ============================================================
# Plot E (v3) — Agricultural export value vs population
# Dual-axis: real export value (left) + population (right)
# 1980–2024
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)
library(readxl)


# ── 0. Paths ────────────────────────────────────────────────

data_dir      <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets/value vs pop"
path_deflator <- file.path(data_dir, "5206005_Expenditure_Implicit_Price_Deflators (1).xlsx")
path_pop      <- file.path(data_dir, "310101.xlsx")
path_exports  <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets/export_values.rds"


# ── 1. GDP implicit price deflator ──────────────────────────
# ABS 5206.0 Table 5, series A2303730T — GROSS DOMESTIC PRODUCT IPD
# Quarterly seasonally adjusted; June quarter used as annual value
# Rebased to June 2024 = 100

deflator_raw <- read_excel(path_deflator, sheet = "Data1",
                           col_names = FALSE, skip = 10) %>%
  select(date = 1, deflator = 39) %>%
  mutate(
    date     = as.Date(date),
    deflator = as.numeric(deflator),
    month    = as.integer(format(date, "%m")),
    year     = as.integer(format(date, "%Y"))
  ) %>%
  filter(month == 6, year >= 1979, !is.na(deflator))

base_val <- deflator_raw$deflator[deflator_raw$year == 2024]
cat("Deflator base value (June 2024):", base_val, "-> rebasing to 100\n")

gdp_deflator <- deflator_raw %>%
  mutate(deflator_2024 = deflator / base_val * 100) %>%
  select(year, deflator = deflator_2024)

cat("Deflator spot check:\n")
gdp_deflator %>%
  filter(year %in% c(1980, 1990, 2000, 2010, 2020, 2024)) %>%
  print()


# ── 2. Estimated resident population ────────────────────────
# ABS 3101.0 Table 1, series A2133251W — ERP Australia ('000)
# June quarter used as annual value

pop_raw <- read_excel(path_pop, sheet = "Data1",
                      col_names = FALSE, skip = 10) %>%
  select(date = 1, pop_000 = 12) %>%
  mutate(
    date    = as.Date(date),
    pop_000 = as.numeric(pop_000),
    month   = as.integer(format(date, "%m")),
    year    = as.integer(format(date, "%Y"))
  ) %>%
  filter(month == 6, year >= 1979, !is.na(pop_000)) %>%
  mutate(population_m = pop_000 / 1000) %>%
  select(year, population_m)

cat("\nPopulation spot check:\n")
pop_raw %>%
  filter(year %in% c(1981, 1990, 2000, 2010, 2020, 2024)) %>%
  print()


# ── 3. Export values ─────────────────────────────────────────
The gdp_deflator object is missing — the rebasing chunk got lost between edits. Here's the complete corrected script:
r# ============================================================
# Plot E (v3) — Agricultural export value vs population
# Dual-axis: real export value (left) + population (right)
# 1980–2024
# ============================================================

library(dplyr)
library(ggplot2)
library(scales)
library(readxl)


# ── 0. Paths ────────────────────────────────────────────────

data_dir      <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets/value vs pop"
path_deflator <- file.path(data_dir, "5206005_Expenditure_Implicit_Price_Deflators (1).xlsx")
path_pop      <- file.path(data_dir, "310101.xlsx")
path_exports  <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets/export_values.rds"


# ── 1. GDP implicit price deflator ──────────────────────────
# ABS 5206.0 Table 5, series A2303730T — GROSS DOMESTIC PRODUCT IPD
# Quarterly seasonally adjusted; June quarter used as annual value
# Rebased to June 2024 = 100

deflator_raw <- read_excel(path_deflator, sheet = "Data1",
                           col_names = FALSE, skip = 10) %>%
  select(date = 1, deflator = 39) %>%
  mutate(
    date     = as.Date(date),
    deflator = as.numeric(deflator),
    month    = as.integer(format(date, "%m")),
    year     = as.integer(format(date, "%Y"))
  ) %>%
  filter(month == 6, year >= 1979, !is.na(deflator))

base_val <- deflator_raw$deflator[deflator_raw$year == 2024]
cat("Deflator base value (June 2024):", base_val, "-> rebasing to 100\n")

gdp_deflator <- deflator_raw %>%
  mutate(deflator_2024 = deflator / base_val * 100) %>%
  select(year, deflator = deflator_2024)

cat("Deflator spot check:\n")
gdp_deflator %>%
  filter(year %in% c(1980, 1990, 2000, 2010, 2020, 2024)) %>%
  print()


# ── 2. Estimated resident population ────────────────────────
# ABS 3101.0 Table 1, series A2133251W — ERP Australia ('000)
# June quarter used as annual value

pop_raw <- read_excel(path_pop, sheet = "Data1",
                      col_names = FALSE, skip = 10) %>%
  select(date = 1, pop_000 = 12) %>%
  mutate(
    date    = as.Date(date),
    pop_000 = as.numeric(pop_000),
    month   = as.integer(format(date, "%m")),
    year    = as.integer(format(date, "%Y"))
  ) %>%
  filter(month == 6, year >= 1979, !is.na(pop_000)) %>%
  mutate(population_m = pop_000 / 1000) %>%
  select(year, population_m)

cat("\nPopulation spot check:\n")
pop_raw %>%
  filter(year %in% c(1981, 1990, 2000, 2010, 2020, 2024)) %>%
  print()


# ── 3. Export values ─────────────────────────────────────────

export_values <- readRDS(path_exports)
cat("\nExport years available:", min(export_values$year), "-", max(export_values$year), "\n")

total_exp_yr <- export_values %>%
  group_by(year) %>%
  summarise(total_export_m = sum(value, na.rm = TRUE), .groups = "drop")


# ── 4. Join and deflate ──────────────────────────────────────

analysis_data <- total_exp_yr %>%
  left_join(pop_raw,      by = "year") %>%
  left_join(gdp_deflator, by = "year") %>%
  filter(!is.na(population_m), !is.na(deflator), year >= 1980) %>%
  mutate(
    total_export_real_b = (total_export_m * (100 / deflator)) / 1000,
    total_export_nom_b  = total_export_m / 1000
  )

cat("\nAnalysis data summary:\n")
analysis_data %>%
  filter(year %in% c(1980, 1990, 2000, 2010, 2020, 2024)) %>%
  select(year, total_export_nom_b, total_export_real_b, population_m, deflator) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  as.data.frame() %>%
  print()



# ── 5. Dual-axis scaling ─────────────────────────────────────

exp_min <- 0
exp_max <- max(analysis_data$total_export_real_b, na.rm = TRUE) * 1.15
pop_min <- 13
pop_max <- 30

scale_pop_to_exp <- function(x) {
  (x - pop_min) / (pop_max - pop_min) * (exp_max - exp_min) + exp_min
}
scale_exp_to_pop <- function(x) {
  (x - exp_min) / (exp_max - exp_min) * (pop_max - pop_min) + pop_min
}


# ── 6. Plot ──────────────────────────────────────────────────

col_exports <- "#1d6fa4"
col_pop     <- "black"
yr_min      <- 1990
yr_max      <- max(analysis_data$year)

p_E3 <- ggplot(analysis_data %>% filter(year >= 1990), aes(x = year)) +
  
  geom_area(aes(y = total_export_real_b),
            fill = col_exports, alpha = 0.12) +
  geom_line(aes(y = total_export_real_b,
                colour = "Ag export value (real, 2024 A$)"),
            linewidth = 1.1) +
  geom_point(aes(y = total_export_real_b,
                 colour = "Ag export value (real, 2024 A$)"),
             size = 1.5) +
  
  geom_line(aes(y = scale_pop_to_exp(population_m),
                colour = "Population"),
            linewidth = 1.0) +
  geom_point(aes(y = scale_pop_to_exp(population_m),
                 colour = "Population"),
             size = 1.5) +
  
  scale_y_continuous(
    name     = "Agricultural export value (A$ billion, real 2024)",
    limits   = c(exp_min, exp_max),
    labels   = label_dollar(suffix = "b"),
    expand   = expansion(mult = c(0, 0.02)),
    sec.axis = sec_axis(
      transform = scale_exp_to_pop,
      name      = "Australian population (millions)",
      labels    = label_number(suffix = "M", accuracy = 0.1)
    )
  ) +
  scale_x_continuous(
    breaks = c(seq(1990, yr_max, by = 5), 2024),
    expand = expansion(add = 0.5)
  ) +
  scale_colour_manual(
    name   = NULL,
    values = c("Ag export value (real, 2024 A$)" = col_exports,
               "Population"                       = col_pop)
  ) +
  
  labs(
    title    = "Australian agricultural exports have outpaced population growth",
    subtitle = paste0("Real export value (2024 A$) and population, 1990\u20132024"),
    x        = NULL,
    caption  = paste0(
      "Sources: ABARES Agricultural Commodity Statistics 2024\u201325; ",
      "ABS 3101.0 Table 1 series A2133251W (population); ",
      "ABS 5206.0 Table 5 series A2303730T (GDP IPD, rebased to June 2024 = 100).\n",
      "Commodities: wheat, coarse grains, beef, wool, dairy, horticulture, cotton, sugar. ",
      "Nominal values deflated to real 2024 A$."
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

p_E3

ggsave(file.path(data_dir, "plot_E3_dual_axis_real_1990.png"),
       plot = p_E3, width = 10, height = 6, dpi = 300)

### CLEAN

p_E3_v2 <- ggplot(analysis_data %>% filter(year >= 1990), aes(x = year)) +
  
  geom_area(aes(y = total_export_real_b),
            fill = col_exports, alpha = 0.12) +
  geom_line(aes(y = total_export_real_b,
                colour = "Ag export value (real, 2024 A$)"),
            linewidth = 1.1) +
  geom_point(aes(y = total_export_real_b,
                 colour = "Ag export value (real, 2024 A$)"),
             size = 1.5) +
  
  geom_line(aes(y = scale_pop_to_exp(population_m),
                colour = "Population"),
            linewidth = 1.0) +
  geom_point(aes(y = scale_pop_to_exp(population_m),
                 colour = "Population"),
             size = 1.5) +
  
  scale_y_continuous(
    name     = "Agricultural export value (A$ billion, real 2024)",
    limits   = c(exp_min, exp_max),
    labels   = label_dollar(suffix = "b"),
    expand   = expansion(mult = c(0, 0.02)),
    sec.axis = sec_axis(
      transform = scale_exp_to_pop,
      name      = "Australian population (millions)",
      labels    = label_number(suffix = "M", accuracy = 0.1)
    )
  ) +
  scale_x_continuous(
    breaks = c(seq(1990, yr_max, by = 5), 2024),
    expand = expansion(add = 0.5)
  ) +
  scale_colour_manual(
    name   = NULL,
    values = c("Ag export value (real, 2024 A$)" = col_exports,
               "Population"                       = col_pop)
  ) +
  
  labs(title = NULL, subtitle = NULL, caption = NULL, x = NULL) +
  
  theme_classic() +
  theme(
    axis.title.y.left  = element_text(colour = col_exports, size = 12),
    axis.title.y.right = element_text(colour = col_pop,     size = 12),
    axis.text.y.left   = element_text(colour = col_exports, size = 10),
    axis.text.y.right  = element_text(colour = col_pop,     size = 10),
    axis.text.x        = element_text(size = 10),
    legend.position    = "none",
    panel.grid.major.y = element_line(colour = "grey92"),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(10, 20, 10, 10)
  )

p_E3_v2

ggsave(file.path(data_dir, "plot_E3_v2_CLEAN.png"),
       plot = p_E3_v2, width = 10, height = 6, dpi = 300)
ggsave(file.path(data_dir, "plot_E3_v2_CLEAN_60dpi.png"),
       plot = p_E3_v2, width = 10, height = 6, dpi = 600)
