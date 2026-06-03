# ============================================================
# LAND USE TRANSITION BAR CHART
# Version 1: All 6 classes, no table
# Version 2: 5 classes in bar, 6 classes in table, table overlaid
# ============================================================
library(tidyverse)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(grid)

# ============================================================
# 1. LOAD SAVED DATA
# ============================================================
out_path <- "N:/Advances in Australian Farming Systems Paper/Section 1/landuse maps/any_land_use_change_inc_no_ag/"
transition_tbl <- read.csv(paste0(out_path, "transition_full_matrix.csv"))

# ============================================================
# 2. CALCULATE AREA BY CLASS FOR EACH YEAR
# ============================================================
area_2011 <- transition_tbl %>%
  group_by(class = from_label) %>%
  summarise(area_Mha_2011 = sum(area_Mha), .groups = "drop")

area_2021 <- transition_tbl %>%
  group_by(class = to_label) %>%
  summarise(area_Mha_2021 = sum(area_Mha), .groups = "drop")

change_tbl <- area_2011 %>%
  left_join(area_2021, by = "class") %>%
  mutate(
    change_Mha = area_Mha_2021 - area_Mha_2011,
    pct_change = (change_Mha / area_Mha_2011) * 100,
    class = recode(class, "Intensive" = "Intensive plant and animal industries")
  )

# 5 classes for the bar (excludes Intensive)
change_tbl_5 <- change_tbl %>%
  filter(class != "Intensive plant and animal industries")

# ============================================================
# 3. COLOURS
# ============================================================
class_colours_6 <- c(
  "Grazing native"                        = "#D9D6CF",
  "Grazing modified"                      = "#CDD546",
  "Cropping"                              = "#72881A",
  "Horticulture"                          = "#E60000",
  "Intensive plant and animal industries" = "#73DFFF",
  "Non-ag"                                = "#C8A882"
)

class_colours_5 <- class_colours_6[names(class_colours_6) != "Intensive plant and animal industries"]

# ============================================================
# 4. VERSION 1: ALL 6 CLASSES, NO TABLE
# ============================================================
pub_tbl <- change_tbl %>%
  arrange(desc(pct_change)) %>%
  mutate(
    area_Mha_2011 = round(area_Mha_2011, 3),
    area_Mha_2021 = round(area_Mha_2021, 3),
    change_Mha    = round(change_Mha, 3),
    pct_change    = round(pct_change, 1)
  ) %>%
  rename(
    "Land use class"     = class,
    "Area 2010-11 (Mha)" = area_Mha_2011,
    "Area 2020-21 (Mha)" = area_Mha_2021,
    "Change (Mha)"       = change_Mha,
    "Change (%)"         = pct_change
  )

p_bar_v1 <- ggplot(change_tbl,
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
  scale_fill_manual(values = class_colours_6, guide = "none") +
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

print(p_bar_v1)

ggsave(paste0(out_path, "bar_all_class_change.png"),
       p_bar_v1, width = 10, height = 7, dpi = 300)

write.csv(pub_tbl, paste0(out_path, "class_area_change.csv"), row.names = FALSE)

# ============================================================
# 5. VERSION 2: 5 CLASSES IN BAR, 6 IN TABLE, TABLE OVERLAID
# ============================================================

# Smart rounding: no dp for large classes, 3 dp for small classes
tbl_data <- change_tbl %>%
  arrange(desc(pct_change)) %>%
  mutate(
    "Land use class" = class,
    "2010-11\n(Mha)" = ifelse(area_Mha_2011 >= 10,
                              as.character(round(area_Mha_2011, 0)),
                              sprintf("%.3f", area_Mha_2011)),
    "2020-21\n(Mha)" = ifelse(area_Mha_2021 >= 10,
                              as.character(round(area_Mha_2021, 0)),
                              sprintf("%.3f", area_Mha_2021))
  ) %>%
  select("Land use class", "2010-11\n(Mha)", "2020-21\n(Mha)")


p_bar_v2 <- ggplot(change_tbl_5,
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
  scale_fill_manual(values = class_colours_5, guide = "none") +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0.15, 0.2))
  ) +
  labs(
    title   = "Change in land use area: 2010-11 to 2020-21",
    caption = "Source: ABARES NLUM v7 (2024). Excludes 'No data/offshore' and 'Intensive plant and animal industries'.",
    x = NULL,
    y = "% change in area"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

tbl_grob <- tableGrob(
  tbl_data,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 10,
    core      = list(fg_params = list(hjust = 0, x = 0.05)),
    colhead   = list(
      fg_params = list(fontface = "bold", hjust = 0, x = 0.05),
      bg_params = list(fill = "grey92")
    )
  )
)

tbl_grob <- gtable::gtable_add_grob(
  tbl_grob,
  grobs = rectGrob(gp = gpar(fill = NA, col = "grey70", lwd = 1)),
  t = 1, b = nrow(tbl_grob),
  l = 1, r = ncol(tbl_grob)
)

# Overlay table on the right portion of the plot using inset_element
# values are 0-1 proportions of the plot area (left, bottom, right, top)
p_combined <- p_bar_v2 +
  inset_element(
    wrap_elements(tbl_grob),
    left = 0.38, bottom = 0.0, right = 1.0, top = 0.75
  )

print(p_combined)

ggsave(paste0(out_path, "bar_5class_change_with_table.png"),
       p_combined, width = 10, height = 7, dpi = 300)

# ============================================================
# 5b. VERSION 2: 5 CLASSES IN BAR, 6 IN TABLE, TABLE OVERLAID
# ============================================================


p_bar_5b <- ggplot(change_tbl_5,
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
  scale_fill_manual(values = class_colours_5, guide = "none") +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0.15, 0.2))
  ) +
  labs(
    x = NULL,
    y = NULL          # removes x-axis label (y in coord_flip)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )
p_bar_5b

p_combined_5b <- p_bar_5b +
  inset_element(
    wrap_elements(tbl_grob),
    left = 0.42, bottom = 0.0, right = 1.02, top = 0.75
  )
p_combined_5b

ggsave(paste0(out_path, "bar_5bclass_change_with_table_CLEAN.png"),
       p_combined_5b, width = 10, height = 7, dpi = 300)

