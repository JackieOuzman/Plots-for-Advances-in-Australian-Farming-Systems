# ============================================================
# Farm consolidation trend – Australian broadacre farms
# Data: ABARES Historical National Estimates via read.abares
# Author: Jackie Ouzman / CSIRO Agriculture & Food
# Run each section (Ctrl+Enter on selection) independently
# ============================================================


# ── Section 1: Load libraries ────────────────────────────────

library(read.abares)
library(tidyverse)
library(scales)


# ── Section 2: Download ABARES data ──────────────────────────
# Fetches the national historical CSV from the ABARES Farm Data Portal
# Requires internet connection on first run; read.abares caches locally

nat <- read_historical_national_estimates()

# Check what variables are available in this dataset
unique(nat$Variable)
grep("area", unique(nat$Variable), value = TRUE, ignore.case = TRUE)
grep("farms", unique(nat$Variable), value = TRUE, ignore.case = TRUE)
grep("number", unique(nat$Variable), value = TRUE, ignore.case = TRUE)

# Check column names
names(nat)

# ── Section 3: Filter to consolidation variables ─────────────
# Keep only the variables needed:

nat_filtered <- nat |>
  filter(Variable %in% c( "Area operated at 30 June (ha)"))

# Check result
unique(nat_filtered$Variable)
nrow(nat_filtered)


# ── Section 4: Check if there is an Industry column to filter on ──
# Some versions of the data include sub-industries (cropping, beef, etc.)
# If so, keep only the national "All broadacre" aggregate

"Industry" %in% names(nat_filtered)   # TRUE = column exists

# nat_filtered <- nat_filtered |>
#   filter(Industry == "All broadacre")  # remove this line if column absent


# ── Section 5: Reshape to one row per year ───────────────────
# Pivot from long to wide so n_farms and area_ha are separate columns

wide <- nat_filtered |>
  select(Variable, Year, Value, Industry) |>
  pivot_wider(names_from = Variable, values_from = Value)

# Rename to tidy column names
names(wide)                            # check names before renaming
wide <- wide |>
  rename(
    area_ha = `Area operated at 30 June (ha)`
  )

head(wide)

# ── Section 6: Clean the year column ─────────────────────────
# Year column is a financial year string e.g. "1977-78" – extract start year

wide <- wide |>
  mutate(year_num = as.integer(substr(Year, 1, 4))) |>
  filter( !is.na(area_ha))

# Quick check
range(wide$year_num)
summary(wide$area_ha)





# ── Section 8: Build the plot ────────────────────────────────

p <- ggplot(wide, aes(x = year_num, y = area_ha, colour = Industry)) +
  
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  
  scale_y_continuous(
    name   = "Area operated per farm (ha)",
    labels = comma
  ) +
  
  scale_x_continuous(breaks = seq(1990, 2024, by = 5)) +
  
  labs(
    title    = "Consolidation of Australian broadacre farms",
    subtitle = "Area operated per farm rising across all industries since 1990",
    x        = NULL,
    colour   = "Industry",
    caption  = "Source: ABARES Historical National Estimates (Farm Data Portal); read.abares package"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "lines"),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(colour = "grey55", size = 8)
  )

print(p)
