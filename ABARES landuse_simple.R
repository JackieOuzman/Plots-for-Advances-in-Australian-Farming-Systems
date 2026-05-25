# ============================================================
# LAND USE CHANGE MAP: 2010-11 vs 2020-21 (NLUM)
# ============================================================

library(terra)
library(tidyverse)
library(ggplot2)
library(tidyterra)   # for geom_spatraster with ggplot2

# ============================================================
# 1. LOAD RASTERS
# ============================================================
path2011 <- "N:/Advances in Australian Farming Systems Paper/Section 1/landuse maps/2010_11/"
path2021 <- "N:/Advances in Australian Farming Systems Paper/Section 1/landuse maps/2020_21/"

lu_2011 <- rast(paste0(path2011, "NLUM_v7_250_ALUMV8_2010_11_alb.tif"))
lu_2021 <- rast(paste0(path2021, "NLUM_v7_250_ALUMV8_2020_21_alb.tif"))

# ============================================================
# 2. LOAD LOOKUP TABLES
# ============================================================
# Assumed columns: VALUE (raster cell value), LU_DESC (description), LU_CLASS (broad class)
# Adjust column names to match your actual CSVs

lookup_2011 <- read.csv(paste0(path2011, "NLUM_v7_250_ALUMV8_2010_11_alb.csv"))
lookup_2021 <- read.csv(paste0(path2021, "NLUM_v7_250_ALUMV8_2020_21_alb.csv"))

# Preview to check column names — comment out once confirmed
head(lookup_2011)
head(lookup_2021)

# ============================================================
# 3. ALIGN RASTERS (resample 2011 to match 2021 if needed)
# ============================================================
if (!compareGeom(lu_2011, lu_2021, stopOnError = FALSE)) {
  message("Rasters don't align — resampling lu_2011 to match lu_2021...")
  lu_2011 <- resample(lu_2011, lu_2021, method = "near")  # nearest neighbour for categorical
}


unique(lookup_2021$AGIND)
unique(lookup_2011$AGIND)


# ============================================================
# 4. BUILD AGIND LOOKUP FROM OFFICIAL TABLE A1.4
# ============================================================
# Value ranges directly from Table A1.4 in the NLUM metadata
# Official ABARES hex colours included

agind_lookup <- tribble(
  ~AGIND,                                ~hex,
  "Grazing native vegetation",           "#D9D6CF",
  "Grazing modified pastures",           "#CDD546",
  "Cropping",                            "#72881A",
  "Horticulture",                        "#E60000",
  "Intensive plant and animal industries","#73DFFF",
  "Not agricultural industry",           "#FFFFFF",
  "No data/offshore",                    NA_character_
)

# AGIND classes to INCLUDE in change analysis (exclude non-ag and no data)
agind_ag <- c(
  "Grazing native vegetation",
  "Grazing modified pastures",
  "Cropping",
  "Horticulture",
  "Intensive plant and animal industries"
)

# ============================================================
# 5. JOIN AGIND TO RASTER VALUES VIA LOOKUP
# ============================================================
# The 'Value' column in the CSV matches raster cell values
# Check your column name — adjust if different (e.g. "VALUE", "Id")

lu2011_key <- lookup_2011 %>%
  select(Value, AGIND) %>%
  distinct()

lu2021_key <- lookup_2021 %>%
  select(Value, AGIND) %>%
  distinct()

# Create integer AGIND code for reclassification
agind_levels <- agind_ag  # only the 5 agricultural classes
agind_code <- tibble(
  AGIND = agind_levels,
  agind_int = seq_along(agind_levels)
)

# Build reclassification matrices: raster Value -> agind_int (NA if not ag)
make_rcl <- function(key) {
  key %>%
    left_join(agind_code, by = "AGIND") %>%
    select(from = Value, to = agind_int) %>%
    mutate(to = replace_na(to, NA_integer_)) %>%
    as.matrix()
}

rcl_2011 <- make_rcl(lu2011_key)
rcl_2021 <- make_rcl(lu2021_key)

# Reclassify rasters to integer AGIND codes (non-ag pixels -> NA)
ag_2011 <- classify(lu_2011, rcl_2011, others = NA)
ag_2021 <- classify(lu_2021, rcl_2021, others = NA)


# ============================================================
# 6. DETECT CHANGE
# ============================================================

# Binary: 1 = AGIND class changed, 0 = no change (NA where either year is non-ag)
changed <- ifel(ag_2011 != ag_2021, 1L, 0L)

# Transition code: from-class * 10 + to-class (e.g. Grazing native -> Cropping = 13)
transition <- ag_2011 * 10L + ag_2021

# ============================================================
# 7. AREA SUMMARY TABLE BY AGIND CLASS
# ============================================================
pixel_area_ha <- (res(lu_2021)[1] * res(lu_2021)[2]) / 10000  # 250m x 250m = 6.25 ha

tabulate_agind <- function(r, key, label) {
  freq(r) %>%
    as_tibble() %>%
    rename(agind_int = value, n_pixels = count) %>%
    left_join(agind_code, by = "agind_int") %>%
    mutate(
      year     = label,
      area_ha  = n_pixels * pixel_area_ha,
      area_Mha = area_ha / 1e6
    )
}

tbl_2011 <- tabulate_agind(ag_2011, lu2011_key, "2010-11")
tbl_2021 <- tabulate_agind(ag_2021, lu2021_key, "2020-21")

change_tbl <- tbl_2011 %>%
  select(AGIND, area_Mha_2011 = area_Mha) %>%
  left_join(tbl_2021 %>% select(AGIND, area_Mha_2021 = area_Mha), by = "AGIND") %>%
  mutate(
    change_Mha = area_Mha_2021 - area_Mha_2011,
    pct_change  = (change_Mha / area_Mha_2011) * 100
  ) %>%
  left_join(agind_lookup, by = "AGIND")

print(change_tbl)

# ============================================================
# 8. BAR CHART: % change by AGIND class (official colours)
# ============================================================
p_bar <- ggplot(change_tbl,
                aes(x = reorder(AGIND, pct_change),
                    y = pct_change,
                    fill = AGIND)) +
  geom_col(width = 0.6, colour = "grey30", linewidth = 0.3) +
  geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey20") +
  geom_text(aes(label = sprintf("%+.1f%%", pct_change),
                hjust = ifelse(pct_change >= 0, -0.15, 1.15)),
            size = 3.5) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = setNames(agind_lookup$hex, agind_lookup$AGIND),
                    guide = "none") +
  scale_y_continuous(labels = scales::label_percent(scale = 1),
                     expand = expansion(mult = c(0.15, 0.2))) +
  labs(
    title    = "Change in agricultural land use area: 2010-11 to 2020-21",
    subtitle = "Australia — NLUM v7 AGIND classification (250m)",
    caption  = "Source: ABARES NLUM v7 (2024). Excludes 'Not agricultural industry' and 'No data/offshore'.",
    x = NULL,
    y = "% change in area"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(p_bar)

# ============================================================
# 9. SPATIAL MAP: where did AGIND class change?
# ============================================================
# Option A — Binary changed/unchanged map
changed_df <- as.data.frame(changed, xy = TRUE) %>%
  rename(changed = TERTV8) %>%
  filter(!is.na(changed))



p_map_binary <- ggplot(changed_df, aes(x = x, y = y, fill = factor(changed))) +
  geom_raster() +
  scale_fill_manual(
    values = c("0" = "grey90", "1" = "#c0392b"),
    labels = c("0" = "No change", "1" = "Changed"),
    name   = NULL,
    na.value = "white"
  ) +
  coord_equal() +
  labs(
    title    = "AGIND land use change: 2010-11 to 2020-21",
    subtitle = "Red = change in agricultural industry class",
    caption  = "Source: ABARES NLUM v7 (2024)",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 11) +
  theme(legend.position = "bottom")

print(p_map_binary)



# ============================================================
# Option B — Transition map (which class changed to which)
# ============================================================




# Build transition label table from all 5x5 = 25 possible combinations
transition_labels <- expand_grid(
  from_int = agind_code$agind_int,
  to_int   = agind_code$agind_int
) %>%
  left_join(agind_code %>% rename(from_int = agind_int, from_class = AGIND), by = "from_int") %>%
  left_join(agind_code %>% rename(to_int   = agind_int, to_class   = AGIND), by = "to_int") %>%
  mutate(
    trans_code = from_int * 10 + to_int,
    label      = paste0(from_class, " \u2192 ", to_class)
  )

# Mask unchanged pixels (keep only actual transitions)
transition_changed <- mask(transition, changed, maskvalues = 0)

transition_df <- as.data.frame(transition_changed, xy = TRUE) %>%
  rename(trans_code = 3) %>%
  filter(!is.na(trans_code)) %>%
  left_join(transition_labels %>% select(trans_code, from_class, to_class, label),
            by = "trans_code")


# Focus on agriculturally interesting transitions (exclude same-to-same, already masked)
p_map_transition <- ggplot(transition_df,
                           aes(x = x, y = y, fill = to_class)) +
  geom_raster() +
  facet_wrap(~from_class, ncol = 3) +
  scale_fill_manual(
    values = setNames(agind_lookup$hex, agind_lookup$AGIND),
    name   = "Changed TO"
  ) +
  coord_equal() +
  labs(
    title    = "Land use transitions by source class: 2010-11 to 2020-21",
    subtitle = "Faceted by 'changed FROM' AGIND class",
    caption  = "Source: ABARES NLUM v7 (2024)",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 10) +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold", size = 8),
    legend.text      = element_text(size = 7)
  )

table(transition_df$label)

print(p_map_transition)



# ============================================================
# 10. SAVE OUTPUTS
# ============================================================
out_dir <- "N:/Advances in Australian Farming Systems Paper/Section 1/landuse maps/change_outputs/"
dir.create(out_dir, showWarnings = FALSE)

ggsave(paste0(out_dir, "AGIND_pct_change_bar.png"),       p_bar,            width = 9,  height = 5,  dpi = 300)
ggsave(paste0(out_dir, "AGIND_change_map_binary.png"),    p_map_binary,     width = 12, height = 10, dpi = 300)
ggsave(paste0(out_dir, "AGIND_change_map_transition.png"),p_map_transition, width = 16, height = 12, dpi = 300)

writeRaster(changed,            paste0(out_dir, "AGIND_changed_binary.tif"),    overwrite = TRUE)
writeRaster(transition_changed, paste0(out_dir, "AGIND_transition_codes.tif"),  overwrite = TRUE)
write.csv(change_tbl,           paste0(out_dir, "AGIND_area_change_summary.csv"), row.names = FALSE)

