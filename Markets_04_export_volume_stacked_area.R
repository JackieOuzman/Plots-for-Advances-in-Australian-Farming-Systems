# ============================================================
# Markets_04_export_volume_stacked_area.R
# Stacked area chart — agricultural export volume by commodity
# Shows structural shift in what Australia exports over time
#
# Statement supported:
#   "Market dynamics have driven changes in farming systems
#    over time"
#
# Commodities: wheat, coarse grains, beef, cotton, sugar, wool
# Units: all kt (thousand tonnes)
# Production volume used to calculate domestic = production - exports
#
# Sources:
#   ABARES Agricultural Commodity Statistics 2024-25
#   Individual commodity files in data_dir
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readxl)


# ── 0. Paths ────────────────────────────────────────────────

data_dir <- "N:/Advances in Australian Farming Systems Paper/Section 2/Markets"


# ── 1. Helper functions ──────────────────────────────────────

read_vol <- function(path, sheet, col_num, label) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE)
  years  <- as.character(raw[[1]][12:nrow(raw)])
  values <- suppressWarnings(as.numeric(raw[[col_num]][12:nrow(raw)]))
  data.frame(
    year_label = years,
    volume_kt  = values,
    commodity  = label,
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(year_label), !is.na(volume_kt)) %>%
    mutate(year = as.integer(substr(year_label, 1, 4)))
}


# ── 2. Production volumes (kt) ───────────────────────────────

prod_wheat  <- read_vol(
  file.path(data_dir, "21_ACS2024_25_WheatTables_v1.0.0 (3).xlsx"),
  "Wheat", 4, "Wheat")

prod_coarse <- read_vol(
  file.path(data_dir, "04_ACS2024_25_CoarseGrainsTables_v1.0.0 (1).xlsx"),
  "CoarseGrains1", 4, "Coarse grains")

prod_cotton <- read_vol(
  file.path(data_dir, "05_ACS2024_25_CottonTables_v1.0.0.xlsx"),
  "Cotton", 13, "Cotton")

prod_beef   <- read_vol(
  file.path(data_dir, "13_ACS2024_25_Meat-BeefTables_v1.0.0.xlsx"),
  "Beef1", 4, "Beef")

prod_sugar  <- read_vol(
  file.path(data_dir, "19_ACS2024_25_SugarTables_v1.0.0.xlsx"),
  "Sugar", 8, "Sugar")


prod_wool <- read_vol(
  file.path(data_dir, "20_ACS2024_25_WoolTables_v1.0.0.xlsx"),
  "Wool", 4, "Wool")


production <- bind_rows(
  prod_wheat, prod_coarse, prod_cotton,
  prod_beef,  prod_sugar,  prod_wool
) %>%
  select(year, commodity, prod_kt = volume_kt)

cat("Production series years:\n")
production %>%
  group_by(commodity) %>%
  summarise(from = min(year), to = max(year), n = n(), .groups = "drop") %>%
  print()


# ── 3. Export volumes (kt) ───────────────────────────────────
# Wheat:        col 6  AB10352 — total wheat exports kt
# Coarse grains:col 6  AB10362 — total coarse grains exports kt
# Cotton:       col 18 AB218   — cotton lint Australia total kt
# Beef:         col 5  AB23352 — beef and veal exports kt (not col 4 which = production)
# Sugar:        col 11 AB10470 — raw sugar exports kt
# Wool:         col 55 AB10481 — total wool exports kt (all types)

exp_wheat  <- read_vol(
  file.path(data_dir, "21_ACS2024_25_WheatTables_v1.0.0 (3).xlsx"),
  "Wheat", 6, "Wheat")

exp_coarse <- read_vol(
  file.path(data_dir, "04_ACS2024_25_CoarseGrainsTables_v1.0.0 (1).xlsx"),
  "CoarseGrains1", 6, "Coarse grains")

exp_cotton <- read_vol(
  file.path(data_dir, "05_ACS2024_25_CottonTables_v1.0.0.xlsx"),
  "Cotton", 18, "Cotton")

exp_beef <- read_vol(
  file.path(data_dir, "13_ACS2024_25_Meat-BeefTables_v1.0.0.xlsx"),
  "Beef1", 5, "Beef")            # col 5 = AB23352 beef+veal exports; col 4 = production

exp_sugar  <- read_vol(
  file.path(data_dir, "19_ACS2024_25_SugarTables_v1.0.0.xlsx"),
  "Sugar", 11, "Sugar")

exp_wool   <- read_vol(
  file.path(data_dir, "20_ACS2024_25_WoolTables_v1.0.0.xlsx"),
  "Wool", 55, "Wool")

exports <- bind_rows(
  exp_wheat, exp_coarse, exp_cotton,
  exp_beef,  exp_sugar,  exp_wool
) %>%
  select(year, commodity, exp_kt = volume_kt)


# ── 4. Join and calculate domestic ──────────────────────────
# Note: Sugar excluded from domestic calculation — production is raw cane (kt)
#       but exports are refined sugar (kt) — incomparable units.
#       Sugar retained in export stack (G1) but dom_kt set to NA for G2.
# Note: Wool production (col 4 AG635) is total wool all types to match
#       export series (col 55 AB10481) which is also all types.

vol_data <- production %>%
  left_join(exports, by = c("year", "commodity")) %>%
  filter(!is.na(exp_kt), year >= 1990) %>%
  mutate(
    dom_kt = case_when(
      commodity == "Sugar" ~ NA_real_,          # cane vs refined — not comparable
      TRUE ~ pmax(prod_kt - exp_kt, 0)
    ),
    export_pct = exp_kt / prod_kt * 100,
    prod_mt    = prod_kt / 1000,
    exp_mt     = exp_kt  / 1000,
    dom_mt     = dom_kt  / 1000
  )


# ── 5. Commodity order and colours ──────────────────────────
commodity_order <- c("Wheat", "Coarse grains", "Beef",
                     "Wool", "Cotton", "Sugar")

colours_export <- c(
  "Wheat"         = "#1d6fa4",
  "Coarse grains" = "#4a9bc9",
  "Beef"          = "#c0392b",
  "Wool"          = "#7f8c8d",
  "Cotton"        = "#8e44ad",
  "Sugar"         = "#e67e22"
)

vol_data <- vol_data %>%
  mutate(commodity = factor(commodity, levels = commodity_order))



# ── 6. Plot G1 — stacked bar, export volume by commodity ────

yr_max <- max(vol_data$year)

p_G1 <- ggplot(vol_data,
               aes(x = year, y = exp_mt, fill = commodity)) +
  geom_col(width = 0.8, alpha = 0.9) +
  scale_fill_manual(values = colours_export, name = NULL) +
  scale_x_continuous(
    breaks = c(seq(1990, 2020, by = 5), yr_max),
    expand = expansion(add = 0.5)
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "Mt", accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Australian agricultural export volume by commodity",
    subtitle = paste0("Million tonnes, 1990\u2013", yr_max),
    x        = NULL,
    y        = "Export volume (million tonnes)",
    caption  = paste0(
      "Source: ABARES Agricultural Commodity Statistics 2024\u201325.\n",
      "Commodities: wheat, coarse grains, beef and veal, wool, cotton lint, sugar.\n",
      "Dairy, horticulture and sheep meat excluded \u2014 no consistent comparable volume series."
    )
  ) +
  theme_classic() +
  theme(
    plot.title         = element_text(size = 13, face = "bold"),
    plot.subtitle      = element_text(size = 11, colour = "grey40"),
    plot.caption       = element_text(size = 9,  colour = "grey40", hjust = 0),
    axis.title         = element_text(size = 11),
    axis.text          = element_text(size = 10),
    legend.position    = "bottom",
    legend.text        = element_text(size = 10),
    legend.key.size    = unit(0.4, "cm"),
    panel.grid.major.y = element_line(colour = "grey92"),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(10, 15, 10, 10)
  )

p_G1


vol_data %>%
  filter(year >= 2020) %>%
  select(year, commodity, prod_kt, exp_kt, export_pct) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  arrange(year, commodity) %>%
  as.data.frame() %>%
  print()



# ── 7. Plot G2 — faceted domestic vs export by commodity ────
# Sugar excluded (cane production vs refined sugar exports — incomparable units)

vol_long <- vol_data %>%
  filter(commodity != "Sugar", !is.na(dom_mt)) %>%
  select(year, commodity, exp_mt, dom_mt) %>%
  pivot_longer(cols = c(dom_mt, exp_mt),
               names_to  = "segment",
               values_to = "volume_mt") %>%
  mutate(
    segment = recode(segment,
                     "dom_mt" = "Domestic",
                     "exp_mt" = "Exported"),
    segment = factor(segment, levels = c("Domestic", "Exported"))
  )


#####################################
p_G2 <- ggplot(vol_long,
               aes(x = year, y = volume_mt, fill = segment)) +
  geom_col(width = 0.8, alpha = 0.9) +
  facet_wrap(~ commodity, ncol = 3, scales = "free_y") +
  scale_fill_manual(
    values = c("Domestic" = "#a8cfe0", "Exported" = "#1d6fa4"),
    name   = NULL
  ) +
  scale_x_continuous(
    breaks = c(1990, 2000, 2010, yr_max),
    expand = expansion(add = 0.5)
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "Mt", accuracy = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Production split between domestic use and exports",
    subtitle = paste0("Million tonnes, 1990\u2013", yr_max),
    x        = NULL,
    y        = "Volume (million tonnes)",
    caption  = paste0(
      "Source: ABARES Agricultural Commodity Statistics 2024\u201325.\n",
      "Domestic = production minus exports. Sugar excluded (raw cane vs refined sugar ",
      "exports are not directly comparable)."
    )
  ) +
  theme_classic() +
  theme(
    plot.title         = element_text(size = 13, face = "bold"),
    plot.subtitle      = element_text(size = 11, colour = "grey40"),
    plot.caption       = element_text(size = 9,  colour = "grey40", hjust = 0),
    axis.title         = element_text(size = 10),
    axis.text.x        = element_text(size = 9, angle = 45, hjust = 1),
    axis.text.y        = element_text(size = 9),
    strip.text         = element_text(size = 10, face = "bold"),
    strip.background   = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 10),
    panel.grid.major.y = element_line(colour = "grey92"),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(10, 15, 10, 10)
  )

p_G2
ggsave(file.path(data_dir, "plot_G2_domestic_vs_export_facet.png"),
       plot = p_G2, width = 12, height = 8, dpi = 300)
cat("Plot G2 saved\n")


# ── 8. Clean versions (no title/subtitle/caption) ───────────
ggsave(file.path(data_dir, "plot_G1_v2.png"),
       plot = p_G1, width = 10, height = 6, dpi = 300)


p_G1_v2 <- p_G1 +
  labs(title = NULL, subtitle = NULL, caption = NULL) +
  theme(legend.position = "bottom")

p_G2_v2 <- p_G2 +
  labs(title = NULL, subtitle = NULL, caption = NULL) +
  theme(legend.position = "bottom")

ggsave(file.path(data_dir, "plot_G1_v2_clean.png"),
       plot = p_G1_v2, width = 10, height = 6, dpi = 300)
ggsave(file.path(data_dir, "plot_G2_v2_clean.png"),
       plot = p_G2_v2, width = 12, height = 8, dpi = 300)


ggsave(file.path(data_dir, "plot_G1_v2_clean_600dpi.png"),
       plot = p_G1_v2, width = 10, height = 6, dpi = 600)
ggsave(file.path(data_dir, "plot_G2_v2_clean_600dpi.png"),
       plot = p_G2_v2, width = 12, height = 8, dpi = 600)

