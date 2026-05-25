# ============================================================
# LAND USE TRANSITION ANALYSIS: 2010-11 vs 2020-21 (NLUM)
# Tracks ALL transitions including Non-ag <-> Ag
# ============================================================

library(terra)
library(tidyverse)
library(ggplot2)

# ============================================================
# 1. LOAD RASTERS & LOOKUP TABLES
# ============================================================
path2011 <- "N:/Advances in Australian Farming Systems Paper/Section 1/landuse maps/2010_11/"
path2021 <- "N:/Advances in Australian Farming Systems Paper/Section 1/landuse maps/2020_21/"

lu_2011 <- rast(paste0(path2011, "NLUM_v7_250_ALUMV8_2010_11_alb.tif"))
lu_2021 <- rast(paste0(path2021, "NLUM_v7_250_ALUMV8_2020_21_alb.tif"))

lookup_2011 <- read.csv(paste0(path2011, "NLUM_v7_250_ALUMV8_2010_11_alb.csv"))
lookup_2021 <- read.csv(paste0(path2021, "NLUM_v7_250_ALUMV8_2020_21_alb.csv"))

# ============================================================
# 2. ALIGN RASTERS
# ============================================================
if (!compareGeom(lu_2011, lu_2021, stopOnError = FALSE)) {
  message("Resampling lu_2011 to match lu_2021...")
  lu_2011 <- resample(lu_2011, lu_2021, method = "near")
}

# ============================================================
# 3. DEFINE CLASS SCHEME
# 6 meaningful classes: 5 ag sub-classes + 1 Non-ag
# "No data/offshore" pixels will become NA
# ============================================================
class_lookup <- tribble(
  ~AGIND,                                 ~class_int,  ~class_label,         ~hex,
  "Grazing native vegetation",             1L,          "Grazing native",     "#D9D6CF",
  "Grazing modified pastures",             2L,          "Grazing modified",   "#CDD546",
  "Cropping",                              3L,          "Cropping",           "#72881A",
  "Horticulture",                          4L,          "Horticulture",       "#E60000",
  "Intensive plant and animal industries", 5L,          "Intensive",          "#73DFFF",
  "Not agricultural industry",             6L,          "Non-ag",             "#C8A882"
  # "No data/offshore" intentionally excluded -> will be NA
)

# ============================================================
# 4. BUILD RECLASSIFICATION MATRICES
# Value (raster integer) -> class_int
# ============================================================
make_rcl <- function(lookup_df) {
  lookup_df %>%
    select(Value, AGIND) %>%
    distinct() %>%
    left_join(class_lookup %>% select(AGIND, class_int), by = "AGIND") %>%
    filter(!is.na(class_int)) %>%          # drops "No data/offshore"
    select(from = Value, to = class_int) %>%
    as.matrix()
}

rcl_2011 <- make_rcl(lookup_2011)
rcl_2021 <- make_rcl(lookup_2021)

# Reclassify — pixels not in matrix become NA (no data/offshore)
cls_2011 <- classify(lu_2011, rcl_2011, others = NA)
cls_2021 <- classify(lu_2021, rcl_2021, others = NA)

# ============================================================
# 5. BUILD TRANSITION RASTER
# Encode as: from_class * 10 + to_class
# e.g. Non-ag (6) -> Cropping (3) = 63
# Both years must be non-NA for a valid transition
# ============================================================
transition <- cls_2011 * 10L + cls_2021

# ============================================================
# 6. SUMMARISE TRANSITIONS INTO A TABLE
# ============================================================

transition_tbl <- decode_transitions(transition) %>%
  mutate(
    type = case_when(
      from_label == to_label                         ~ "No change",
      from_label == "Non-ag" & to_label != "Non-ag" ~ "Non-ag → Ag",
      from_label != "Non-ag" & to_label == "Non-ag" ~ "Ag → Non-ag",
      from_label != "Non-ag" & to_label != "Non-ag" ~ "Ag → Ag (shift)",
      TRUE                                           ~ "Other"
    )
  ) %>%
  arrange(type, desc(area_Mha))

print(transition_tbl)

# ============================================================
# 7. SUMMARY BY BROAD TRANSITION TYPE
# ============================================================
type_summary <- transition_tbl %>%
  filter(type != "No change") %>%
  group_by(type) %>%
  summarise(area_Mha = sum(area_Mha), .groups = "drop") %>%
  arrange(desc(area_Mha))

print(type_summary)

# ============================================================
# 8. MAP A — Binary: did this pixel change at all?
# (any class change, including Non-ag <-> Ag)
# ============================================================
changed_binary <- ifel(
  is.na(transition),   NA,           # no data
  ifel(cls_2011 == cls_2021, 0L, 1L) # 0 = same, 1 = changed
)

changed_df <- as.data.frame(changed_binary, xy = TRUE) %>%
  setNames(c("x", "y", "changed")) %>%
  filter(!is.na(changed))

p_map_binary <- ggplot(changed_df, aes(x = x, y = y, fill = factor(changed))) +
  geom_raster() +
  scale_fill_manual(
    values = c("0" = "grey90", "1" = "#c0392b"),
    labels = c("0" = "No change", "1" = "Changed"),
    name   = NULL
  ) +
  coord_equal() +
  labs(
    title    = "Land use change 2010-11 to 2020-21",
    subtitle = "Any class change (including Non-ag ↔ Ag)",
    caption  = "Source: ABARES NLUM v7 (2024)",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 11) +
  theme(legend.position = "bottom")

print(p_map_binary)


# ============================================================
# 9. MAP B — 4 transition types
# ============================================================

# Build a 4-category raster from transition codes
from_int <- cls_2011
to_int   <- cls_2021

trans_type <- ifel(
  is.na(from_int) | is.na(to_int), NA,
  ifel(from_int == to_int,          0L,   # no change
       ifel(from_int == 6L & to_int != 6L, 1L, # Non-ag -> Ag
            ifel(from_int != 6L & to_int == 6L, 2L, # Ag -> Non-ag
                 3L  # Ag -> Ag shift
            )))
)

trans_type_df <- as.data.frame(trans_type, xy = TRUE) %>%
  setNames(c("x", "y", "type_code")) %>%
  filter(!is.na(type_code), type_code != 0)

type_colours <- c(
  "1" = "#2ecc71",   # green  — Non-ag -> Ag
  "2" = "#e74c3c",   # red    — Ag -> Non-ag
  "3" = "#3498db"    # blue   — Ag -> Ag shift
)
type_labels <- c(
  "1" = "Non-ag → Ag",
  "2" = "Ag → Non-ag",
  "3" = "Ag → Ag (shift)"
)

p_map_types <- ggplot(trans_type_df, aes(x = x, y = y, fill = factor(type_code))) +
  geom_raster() +
  scale_fill_manual(
    values   = type_colours,
    labels   = type_labels,
    name     = "Transition type"
  ) +
  coord_equal() +
  labs(
    title    = "Land use transition types: 2010-11 to 2020-21",
    subtitle = "Where and what kind of change occurred",
    caption  = "Source: ABARES NLUM v7 (2024). No-change pixels hidden.",
    x = NULL, y = NULL
  ) +
  theme_void(base_size = 11) +
  theme(legend.position = "bottom")

print(p_map_types)



# ============================================================
# 10. SAVE OUTPUTS
# ============================================================
out_path <- "N:/Advances in Australian Farming Systems Paper/Section 1/landuse maps/any_land_use_change_inc_no_ag/"
dir.create(out_path, showWarnings = FALSE)

# --- Rasters ---
writeRaster(changed_binary, paste0(out_path, "changed_binary.tif"),    overwrite = TRUE)
writeRaster(trans_type,     paste0(out_path, "transition_types.tif"),  overwrite = TRUE)
writeRaster(transition,     paste0(out_path, "transition_codes.tif"),  overwrite = TRUE)

# --- CSVs ---
write.csv(transition_tbl, paste0(out_path, "transition_full_matrix.csv"), row.names = FALSE)
write.csv(type_summary,   paste0(out_path, "transition_type_summary.csv"), row.names = FALSE)

# --- Maps ---
ggsave(paste0(out_path, "map_binary_change.png"),   p_map_binary, width = 12, height = 10, dpi = 300)
ggsave(paste0(out_path, "map_transition_types.png"), p_map_types,  width = 12, height = 10, dpi = 300)
