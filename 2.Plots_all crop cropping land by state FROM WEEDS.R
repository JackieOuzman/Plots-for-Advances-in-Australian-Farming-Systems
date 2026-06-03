

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(patchwork)
library(ggplot2)


# Load all four datasets
wheat_long <- readRDS("W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/wheat_long.rds")
cg_long    <- readRDS("W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/cg_long.rds")
oil_long   <- readRDS("W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/oil_long.rds")
pulse_long <- readRDS("W:/Economic impact of weeds round 2/Reports and papers/Draft Journal Paper/change is cropping type and area Jackie/pulse_long.rds")

# Combine
all_crops <- bind_rows(wheat_long, cg_long, oil_long, pulse_long)

# Check
cat("Crop groups:", unique(all_crops$crop_group), "\n")
cat("Crops:", unique(all_crops$crop), "\n")
cat("States:", unique(all_crops$state), "\n")
cat("Year range:", min(all_crops$year), "to", max(all_crops$year), "\n")
cat("Rows:", nrow(all_crops), "\n")


all_crops %>%
  group_by(crop_group, crop) %>%
  summarise(
    first_year = min(year[!is.na(area_000ha)]),
    last_year  = max(year[!is.na(area_000ha)]),
    .groups = "drop"
  ) %>%
  print(n = Inf)


# Split Coarse grains into individual crops
cg_split <- all_crops %>%
  filter(crop_group == "Coarse grains",
         crop %in% c("Barley", "Oats", "Triticale", "Sorghum"),
         state == "Australia") %>%
  mutate(crop_group = crop)  # use crop name as its own group

# Combine with existing data
all_crops_expanded <- bind_rows(all_crops, cg_split)

# Check new groups
cat("Crop groups after expansion:", unique(all_crops_expanded$crop_group), "\n")

crops_keep <- c("Wheat", "Barley", "Oats", "Triticale", "Canola", 
                "Lupins", "Field peas", "Chickpeas", "Lentils", 
                "Faba beans", "Sorghum")




################################################################################
### Plot with split of coarse grain
################################################################################
crops_keep_expanded <- c("Wheat", "Barley", "Oats", "Triticale", "Sorghum", "Oilseeds", "Pulses")


# Set factor order - reversed so Wheat plots at bottom of stack
crop_order <- c("Triticale", "Pulses", "Oilseeds", "Sorghum", "Oats", "Barley", "Wheat")

all_crops_expanded %>%
  filter(state == "Australia",
         crop_group %in% crops_keep_expanded,
         year >= 1980) %>%
  group_by(year, crop_group) %>%
  summarise(area_000ha = sum(area_000ha, na.rm = TRUE), .groups = "drop") %>%
  mutate(crop_group = factor(crop_group, levels = crop_order)) %>%
  ggplot(aes(x = year, y = area_000ha, fill = crop_group)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "Wheat"     = "#1a6e8a",
      "Barley"    = "#2d9e6b",
      "Oats"      = "#74c2a8",
      "Sorghum"   = "#1a3a5c",
      "Oilseeds"  = "#5b9fc9",
      "Pulses"    = "#a8c8e8",
      "Triticale" = "#8fbc8f"
    ),
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_x_continuous(breaks = seq(1980, 2024, by = 5)) +
  labs(
    title    = "Australian cropping area by crop group",
    subtitle = "Source: ABARES Agricultural Commodity Statistics 2024-25",
    x        = "Year",
    y        = "Area planted ('000 ha)",
    fill     = NULL,
    caption  = "Oilseeds: Canola | Pulses: Lupins, Field peas, Chickpeas, Lentils, Faba beans"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "right",
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, hjust = 0, colour = "grey40")
  )




################################################################################
### Plot with NO split of coarse grain
################################################################################
names(all_crops)
distinct(all_crops, crop_group)



crop_order_agg <- c("Pulses", "Oilseeds", "Coarse grains", "Wheat")

all_crops %>%
  filter(state == "Australia", crop %in% crops_keep, year >= 1980) %>%
  group_by(year, crop_group) %>%
  summarise(area_000ha = sum(area_000ha, na.rm = TRUE), .groups = "drop") %>%
  mutate(crop_group = factor(crop_group, levels = crop_order_agg)) %>%
  ggplot(aes(x = year, y = area_000ha, fill = crop_group)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "Wheat"         = "#1a6e8a",
      "Coarse grains" = "#2d9e6b",
      "Oilseeds"      = "#74c2a8",
      "Pulses"        = "#5b9fc9"
    ),
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_x_continuous(breaks = seq(1980, 2024, by = 5)) +
  labs(
    x    = "",
    y    = "Area planted ('000 ha)",
    fill = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position  = "bottom",
    #legend.position  = "",
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, hjust = 0, colour = "grey40")
  )










################################################################################
### Facet by state and including all crop types for coarse grain Free scale
################################################################################


# Rebuild cg_split without the state filter
cg_split <- all_crops %>%
  filter(crop_group == "Coarse grains",
         crop %in% c("Barley", "Oats", "Triticale", "Sorghum")) %>%
  mutate(crop_group = crop)

# Rebuild all_crops_expanded
all_crops_expanded <- bind_rows(all_crops, cg_split)


### plotting helpers

crop_order_state <- c("Triticale", "Pulses", "Oilseeds", "Sorghum", "Oats", "Barley", "Wheat")
crops_keep_expanded <- c("Wheat", "Barley", "Oats", "Sorghum", "Oilseeds", "Pulses", "Triticale")
state_order <- c("WA", "SA", "VIC", "NSW", "QLD")

### Plot
all_crops_expanded %>%
  filter(state != "Australia",
         state != "TAS",
         crop_group %in% crops_keep_expanded,
         year >= 1980) %>%
  group_by(year, crop_group, state) %>%
  summarise(area_000ha = sum(area_000ha, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    crop_group = factor(crop_group, levels = crop_order_state),
    state      = factor(state, levels = state_order)
  ) %>%
  ggplot(aes(x = year, y = area_000ha, fill = crop_group)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "Wheat"     = "#1a6e8a",
      "Barley"    = "#2d9e6b",
      "Oats"      = "#74c2a8",
      "Sorghum"   = "#1a3a5c",
      "Oilseeds"  = "#5b9fc9",
      "Pulses"    = "#a8c8e8",
      "Triticale" = "#8fbc8f"
    ),
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_x_continuous(breaks = seq(1980, 2024, by = 10)) +
  facet_wrap(~state, scales = "free_y", ncol = 2) +
  labs(
    # title    = "Australian cropping area by crop group and state",
    # subtitle = "Source: ABARES Agricultural Commodity Statistics 2024-25",
    x        = "",
    y        = "Area planted ('000 ha)",
    fill     = NULL,
    #caption  = "Oilseeds: Canola | Pulses: Lupins, Field peas, Chickpeas, Lentils, Faba beans"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "right",
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, hjust = 0, colour = "grey40")
  )



################################################################################
### Facet by state and including all crop types for coarse grain NO Free scale
################################################################################


### Plot
  all_crops_expanded %>%
  filter(state != "Australia",
         state != "TAS",
         crop_group %in% crops_keep_expanded,
         year >= 1980) %>%
  group_by(year, crop_group, state) %>%
  summarise(area_000ha = sum(area_000ha, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    crop_group = factor(crop_group, levels = crop_order_state),
    state      = factor(state, levels = state_order)
  ) %>%
  ggplot(aes(x = year, y = area_000ha, fill = crop_group)) +
  geom_area(alpha = 0.85, colour = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "Wheat"     = "#1a6e8a",
      "Barley"    = "#2d9e6b",
      "Oats"      = "#74c2a8",
      "Sorghum"   = "#1a3a5c",
      "Oilseeds"  = "#5b9fc9",
      "Pulses"    = "#a8c8e8",
      "Triticale" = "#8fbc8f"
    ),
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_x_continuous(breaks = seq(1980, 2024, by = 5)) +
  facet_wrap(~state, ncol = 2) +
  labs(
    title    = "Australian cropping area by crop group and state",
    subtitle = "Source: ABARES Agricultural Commodity Statistics 2024-25",
    x        = "Year",
    y        = "Area planted ('000 ha)",
    fill     = NULL,
    caption  = "Oilseeds: Canola | Pulses: Lupins, Field peas, Chickpeas, Lentils, Faba beans"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "right",
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 8, hjust = 0, colour = "grey40")
  )

