# ============================================================
# ABARES NLUM Land Use Change Sankey: 2010-11 to 2020-21
# ============================================================

# install.packages(c("ggalluvial", "scales"))
library(terra)
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(stringr)
library(scales)

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
lookup_2021 <- read.csv(paste0(path2021, "NLUM_v7_250_ALUMV8_2020_21_alb.csv"))
lookup_2011 <- read.csv(paste0(path2011, "NLUM_v7_250_ALUMV8_2010_11_alb.csv"))

# Build SECV8 -> AGIND lookup
agind_lookup <- lookup_2021 |>
  select(SECV8, AGIND) |>
  distinct() |>
  filter(SECV8 != "No data/offshore",
         AGIND != "No data/offshore")

# Check AGIND categories
unique(agind_lookup$AGIND)

#============================================================
  # 3. SET ACTIVE CATEGORY AND STACK RASTERS
  # ============================================================
# We sample at SECV8 level because that is what links the raster
# pixel values to the CSV lookup table. AGIND labels are then
# attached via left_join in step 6.
activeCat(lu_2011) <- "SECV8"
activeCat(lu_2021) <- "SECV8"

lu_2021_matched <- resample(lu_2021, lu_2011, method = "near")

lu_stack <- c(lu_2011, lu_2021_matched)
names(lu_stack) <- c("lu_2011", "lu_2021")

# ============================================================
# 4. RANDOM SAMPLE
# ============================================================
set.seed(42)
df_sec <- spatSample(lu_stack, size = 500000, method = "random",
                     as.df = TRUE, na.rm = TRUE)

df_sec <- df_sec |>
  rename(from_2011 = lu_2011, to_2021 = lu_2021) |>
  filter(from_2011 != "No data/offshore",
         to_2021   != "No data/offshore")

# ============================================================
# 5. SCALING FACTOR
# (calculated before transitions so it's ready for mutate below)
# ============================================================
total_cells    <- global(lu_2011, fun = "notNA")[[1]]
sampled_cells  <- 500000
scaling_factor <- total_cells / sampled_cells

cat("Total non-NA cells:", total_cells, "\n")
cat("Scaling factor:    ", round(scaling_factor, 1), "\n")



# ============================================================
# 6. BUILD TRANSITION TABLE
# NO threshold filter here - let all transitions through,
# we filter for plotting only
# ============================================================
transitions_agind <- df_sec |>
  count(from_2011, to_2021, name = "n_pixels") |>
  mutate(area_Mha = (n_pixels * 6.25) / 1e6) |>
  left_join(agind_lookup, by = c("from_2011" = "SECV8")) |>
  rename(from_agind = AGIND) |>
  left_join(agind_lookup, by = c("to_2021" = "SECV8")) |>
  rename(to_agind = AGIND)

# # Summarise by AGIND -> AGIND and apply scaling
# transitions_summary <- transitions_agind |>
#   group_by(from_agind, to_agind) |>
#   summarise(area_Mha = sum(area_Mha), .groups = "drop") |>
#   filter(!is.na(from_agind), !is.na(to_agind)) |>
#   mutate(area_Mha_scaled = area_Mha * scaling_factor) |>
#   arrange(desc(area_Mha_scaled))

# # Check all categories present
# cat("AGIND categories in transitions:\n")
# unique(c(transitions_summary$from_agind, transitions_summary$to_agind))
# 
# transitions_summary |> as.data.frame() |> print()

# ============================================================
# REMOVE CATEGORIES - to help with plotting
# ============================================================
# Filter to agricultural categories only
transitions_ag <- transitions_summary |>
  filter(
    !from_agind %in% c("Not agricultural industry"),
    !to_agind   %in% c("Not agricultural industry")
  )


# ============================================================
# REORDER CATEGORIES - largest to smallest, meaningful grouping
# ============================================================
agind_order_ag <- c(
  "Grazing native vegetation",
  "Grazing modified pastures",
  "Cropping",
  "Horticulture",
  "Intensive plant and animal industries"
)

transitions_ag <- transitions_ag |>
  mutate(
    from_agind = factor(from_agind, levels = agind_order_ag),
    to_agind   = factor(to_agind,   levels = agind_order_ag)
  )


# ============================================================
# 7. COLOUR PALETTE
# Matched to official ABARES AGIND colours from the map
# ============================================================
# Updated colours - make Not agricultural industry more distinct

agind_colours_ag <- c(
  "Grazing native vegetation"             = "#b2b2b2",
  "Grazing modified pastures"             = "#c8e86e",
  "Cropping"                              = "#267300",
  "Horticulture"                          = "#e60000",
  "Intensive plant and animal industries" = "#73dfff"
)
# ============================================================
# 8. SANKEY PLOT
# ============================================================
p <- ggplot(transitions_ag,
            aes(axis1 = from_agind,
                axis2 = to_agind,
                y = area_Mha_scaled)) +
  geom_alluvium(aes(fill = from_agind), alpha = 0.8, width = 1/8) +
  geom_stratum(width = 1/4, fill = "white", colour = "grey40", linewidth = 0.4) +
  geom_text(stat = "stratum",
            aes(label = str_wrap(after_stat(stratum), 18)),
            size = 3.2, lineheight = 0.9, fontface = "bold") +
  scale_x_discrete(limits = c("2010\u201311", "2020\u201321"),
                   expand = c(0.3, 0.05)) +
  scale_fill_manual(values = agind_colours_ag) +
  scale_y_continuous(labels = scales::label_number(suffix = " Mha")) +
  labs(
    title    = "Australian Agricultural Land Use Change: 2010\u201311 to 2020\u201321",
    subtitle = paste0("Flow width proportional to estimated area (million hectares)\n",
                      "Scaled from random sample of 500,000 cells (scaling factor: ",
                      round(scaling_factor, 0), "\u00d7) from NLUM v7\n",
                      "Excludes non-agricultural land uses (conservation, water, urban, forestry)"),
    x        = NULL,
    y        = "Area (million hectares)",
    caption  = "Source: ABARES NLUM v7, ALUM Classification v8. CC BY 4.0."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid      = element_blank(),
    axis.text.x     = element_text(size = 13, face = "bold"),
    axis.text.y     = element_text(size = 9),
    plot.title      = element_text(size = 13, face = "bold"),
    plot.subtitle   = element_text(size = 9, colour = "grey40"),
    plot.caption    = element_text(size = 8, colour = "grey50")
  )


print(p)




ggsave("N:/Advances in Australian Farming Systems Paper/Section 1/landuse maps/land_use_sankey_2011_2021_agriculturalcategories.png", 
       p,
       width = 14, height = 8, dpi = 300, bg = "white")





################################################################################
### more filtering to remove grazing veg 
# Focus on actively managed agricultural land uses only
transitions_ag <- transitions_summary |>
  filter(
    !from_agind %in% c("Not agricultural industry", "Grazing native vegetation"),
    !to_agind   %in% c("Not agricultural industry", "Grazing native vegetation")
  )

agind_order_ag <- c(
  "Grazing modified pastures",
  "Cropping",
  "Horticulture",
  "Intensive plant and animal industries"
)

transitions_ag <- transitions_ag |>
  mutate(
    from_agind = factor(from_agind, levels = agind_order_ag),
    to_agind   = factor(to_agind,   levels = agind_order_ag)
  )

agind_colours_ag <- c(
  "Grazing modified pastures"             = "#c8e86e",
  "Cropping"                              = "#267300",
  "Horticulture"                          = "#e60000",
  "Intensive plant and animal industries" = "#73dfff"
)


# ============================================================
# CALCULATE STRATUM TOTALS FOR LABELS
# ============================================================

# ============================================================
# SEPARATE TOTALS FOR EACH TIME POINT
# ============================================================
from_totals <- transitions_ag |>
  group_by(from_agind) |>
  summarise(total_Mha_2011 = sum(area_Mha_scaled), .groups = "drop")

to_totals <- transitions_ag |>
  group_by(to_agind) |>
  summarise(total_Mha_2021 = sum(area_Mha_scaled), .groups = "drop")

# Check - these should show different values
from_totals
to_totals

# Build separate label lookups
label_lookup_2011 <- setNames(
  paste0(from_totals$from_agind, "\n", round(from_totals$total_Mha_2011, 1), " Mha"),
  from_totals$from_agind
)

label_lookup_2021 <- setNames(
  paste0(to_totals$to_agind, "\n", round(to_totals$total_Mha_2021, 1), " Mha"),
  to_totals$to_agind
)


# ============================================================
# PRE-CALCULATE CORRECT LABELS WITH SCALED AREAS
# ============================================================
from_labels <- transitions_ag |>
  group_by(from_agind) |>
  summarise(total = sum(area_Mha_scaled), .groups = "drop") |>
  mutate(label = case_when(
    from_agind %in% c("Grazing modified pastures", "Cropping") ~
      paste0(from_agind, "\n", round(total, 0), " Mha"),
    TRUE ~ ""
  ))

to_labels <- transitions_ag |>
  group_by(to_agind) |>
  summarise(total = sum(area_Mha_scaled), .groups = "drop") |>
  mutate(label = case_when(
    to_agind %in% c("Grazing modified pastures", "Cropping") ~
      paste0(to_agind, "\n", round(total, 0), " Mha"),
    TRUE ~ ""
  ))

# Check these look right before plotting
from_labels
to_labels

# Named vectors for lookup - one per axis
lookup_2011 <- setNames(from_labels$label, from_labels$from_agind)
lookup_2021 <- setNames(to_labels$label,   to_labels$to_agind)

# ============================================================
# PLOT WITH SEPARATE LABELS EACH SIDE
# ============================================================


lookup_2011 <- setNames(from_labels$label, from_labels$from_agind)
lookup_2021 <- setNames(to_labels$label,   to_labels$to_agind)

# Combined lookup that ggalluvial can use - we'll use a trick:
# axis1 strata come first in after_stat, axis2 come second
# so we build one combined lookup with duplicated keys (same name, different label)
# The cleanest approach is two separate geom_text layers filtered by x position

p <- ggplot(transitions_ag,
            aes(axis1 = from_agind,
                axis2 = to_agind,
                y = area_Mha_scaled)) +
  geom_alluvium(aes(fill = from_agind), alpha = 0.8, width = 1/8) +
  geom_stratum(width = 1/4, fill = "white", colour = "grey40", linewidth = 0.4) +
  # Single geom_text using a combined lookup built per-axis
  geom_text(stat = "stratum",
            aes(label = ifelse(after_stat(x) == 1,
                               lookup_2011[after_stat(stratum)],
                               lookup_2021[after_stat(stratum)])),
            size = 3.2, lineheight = 0.9, fontface = "bold") +
  scale_x_discrete(limits = c("2010\u201311", "2020\u201321"),
                   expand = c(0.3, 0.05)) +
  scale_fill_manual(values = agind_colours_ag, name = "Land use category") +
  scale_y_continuous(labels = scales::label_number(suffix = " Mha")) +
  labs(
    title    = "Australian Agricultural Land Use Change: 2010\u201311 to 2020\u201321",
    subtitle = paste0("Flow width proportional to estimated area (million hectares)\n",
                      "Scaled from random sample of 500,000 cells (scaling factor: ",
                      round(scaling_factor, 0), "\u00d7) from NLUM v7\n",
                      "Excludes grazing native vegetation and non-agricultural land uses"),
    x        = NULL,
    y        = "Area (million hectares)",
    caption  = "Source: ABARES NLUM v7, ALUM Classification v8. CC BY 4.0."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 8),
    panel.grid       = element_blank(),
    axis.text.x      = element_text(size = 13, face = "bold"),
    axis.text.y      = element_text(size = 9),
    plot.title       = element_text(size = 13, face = "bold"),
    plot.subtitle    = element_text(size = 9, colour = "grey40"),
    plot.caption     = element_text(size = 8, colour = "grey50")
  )+
  subtitle = paste0("Gross land use transitions — both directions shown simultaneously\n",
                    "Scaled from random sample of 500,000 cells (scaling factor: ",
                    round(scaling_factor, 0), "\u00d7) from NLUM v7\n",
                    "Excludes grazing native vegetation and non-agricultural land uses")


print(p)
ggsave("N:/Advances in Australian Farming Systems Paper/Section 1/landuse maps/land_use_sankey_managed_ag_2011_2021.png", 
       p,
       width = 14, height = 8, dpi = 300, bg = "white")
