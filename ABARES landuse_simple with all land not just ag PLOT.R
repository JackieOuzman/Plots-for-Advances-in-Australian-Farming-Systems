# ============================================================
# LAND USE TRANSITION BAR CHART
# Uses saved outputs from transition analysis
# ============================================================

library(tidyverse)
library(ggplot2)

# ============================================================
# 1. LOAD SAVED DATA
# ============================================================
out_path <- "N:/Advances in Australian Farming Systems Paper/Section 1/landuse maps/any_land_use_change_inc_no_ag/"

transition_tbl <- read.csv(paste0(out_path, "transition_full_matrix.csv"))

# ============================================================
# 2. CALCULATE AREA BY CLASS FOR EACH YEAR
# ============================================================
# Sum area where each class appears as "from" (2010-11)
area_2011 <- transition_tbl %>%
  group_by(class = from_label) %>%
  summarise(area_Mha_2011 = sum(area_Mha), .groups = "drop")

# Sum area where each class appears as "to" (2020-21)
area_2021 <- transition_tbl %>%
  group_by(class = to_label) %>%
  summarise(area_Mha_2021 = sum(area_Mha), .groups = "drop")

# Join and calculate % change
change_tbl <- area_2011 %>%
  left_join(area_2021, by = "class") %>%
  mutate(
    change_Mha = area_Mha_2021 - area_Mha_2011,
    pct_change = (change_Mha / area_Mha_2011) * 100
  )

print(change_tbl)

# ============================================================
# 3. COLOURS FOR ALL 6 CLASSES
# ============================================================
class_colours <- c(
  "Grazing native"   = "#D9D6CF",
  "Grazing modified" = "#CDD546",
  "Cropping"         = "#72881A",
  "Horticulture"     = "#E60000",
  "Intensive"        = "#73DFFF",
  "Non-ag"           = "#C8A882"
)

# ============================================================
# 4. BAR CHART
# ============================================================
p_bar <- ggplot(change_tbl,
                aes(x = reorder(class, pct_change),
                    y = pct_change,
                    fill = class)) +
  geom_col(width = 0.6, colour = "grey30", linewidth = 0.3) +
  geom_hline(yintercept = 0, linewidth = 0.5, colour = "grey20") +
  geom_text(
    aes(label = sprintf("%+.1f%%", pct_change),
        hjust = ifelse(pct_change >= 0, -0.15, 1.15)),
    size = 3.5
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = class_colours, guide = "none") +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0.15, 0.2))
  ) +
  labs(
    title    = "Change in land use area: 2010-11 to 2020-21",
    subtitle = "Australia — NLUM v7 (250m), all classes including Non-ag",
    caption  = "Source: ABARES NLUM v7 (2024). Excludes 'No data/offshore'.",
    x = NULL,
    y = "% change in area"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(p_bar)

# ============================================================
# 5. SAVE
# ============================================================
ggsave(paste0(out_path, "bar_all_class_change.png"),
       p_bar, width = 10, height = 7, dpi = 300)
write.csv(change_tbl, paste0(out_path, "class_area_change.csv"), row.names = FALSE)
