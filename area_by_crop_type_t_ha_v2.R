library(tidyverse)
library(readxl)
library(zoo)

# ── paths ─────────────────────────────────────────────────────────────────────
base    <- "N:/Advances in Australian Farming Systems Paper/Section 3/area_by_crop_type_t_ha"
out_dir <- "N:/Advances in Australian Farming Systems Paper/Section 3"

# ── helpers ───────────────────────────────────────────────────────────────────
read_yield <- function(filepath, sheet, keep_commodities, states_only = FALSE) {
  df        <- read_excel(filepath, sheet = sheet, col_names = FALSE)
  unit_row  <- as.character(df[8, ])
  yield_cols <- which(unit_row == "t/ha")
  years_raw <- df[12:nrow(df), 1, drop = TRUE]
  year      <- as.integer(substr(as.character(years_raw), 1, 4))
  
  state_map <- c("New South Wales" = "NSW", "Victoria" = "VIC",
                 "Queensland" = "QLD", "South Australia" = "SA",
                 "Western Australia" = "WA")
  
  out <- list()
  for (col in yield_cols) {
    commodity <- as.character(df[4, col])
    reporter  <- as.character(df[5, col])
    if (!commodity %in% keep_commodities) next
    if (states_only  && !reporter %in% names(state_map)) next
    if (!states_only && reporter != "Australia") next
    vals <- suppressWarnings(as.numeric(as.character(df[12:nrow(df), col, drop = TRUE])))
    state <- if (states_only) unname(state_map[reporter]) else "Australia"
    out[[length(out) + 1]] <- tibble(year, commodity, state, yield_tha = vals)
  }
  bind_rows(out)
}

# ── NEW: area helper ──────────────────────────────────────────────────────────
# Reads '000 ha columns for Australia-level data
read_area <- function(filepath, sheet, keep_commodities) {
  df       <- read_excel(filepath, sheet = sheet, col_names = FALSE)
  unit_row <- as.character(df[8, ])
  area_cols <- which(unit_row == "'000 ha")
  years_raw <- df[12:nrow(df), 1, drop = TRUE]
  year      <- as.integer(substr(as.character(years_raw), 1, 4))
  
  out <- list()
  for (col in area_cols) {
    commodity <- as.character(df[4, col])
    reporter  <- as.character(df[5, col])
    if (!commodity %in% keep_commodities) next
    if (reporter != "Australia") next
    vals <- suppressWarnings(as.numeric(as.character(df[12:nrow(df), col, drop = TRUE])))
    out[[length(out) + 1]] <- tibble(year, commodity, area_kha = vals)
  }
  bind_rows(out)
}

crops      <- c("Wheat", "Barley", "Canola", "Sorghum", "Lentils")
crop_levels <- c("Wheat", "Barley", "Canola", "Sorghum", "Lentils")

files <- list(
  list(path = "21_ACS2024_25_WheatTables_v1.0.0 (1).xlsx", sheet = "Wheat",         keep = "Wheat"),
  list(path = "04_ACS2024_25_CoarseGrainsTables_v1.0.0.xlsx", sheet = "CoarseGrains1", keep = c("Barley", "Sorghum")),
  list(path = "16_ACS2024_25_OilseedsTables_v1.0.0.xlsx",   sheet = "Oilseeds1",    keep = "Canola"),
  list(path = "17_ACS2024_25_PulsesTables_v1.0.0.xlsx", sheet = "Pulses", keep = "Lentils")
)

# ── national yield data ───────────────────────────────────────────────────────
national <- map(files, ~read_yield(file.path(base, .x$path), .x$sheet, .x$keep)) %>%
  bind_rows() %>%
  filter(!is.na(yield_tha)) %>%
  mutate(commodity = factor(commodity, levels = crop_levels)) %>%
  arrange(commodity, year) %>%
  group_by(commodity) %>%
  mutate(roll5 = rollmean(yield_tha, k = 5, fill = NA, align = "right")) %>%
  ungroup()

# ── national area data ────────────────────────────────────────────────────────
national_area <- map(files, ~read_area(file.path(base, .x$path), .x$sheet, .x$keep)) %>%
  bind_rows() %>%
  filter(!is.na(area_kha)) %>%
  mutate(commodity = factor(commodity, levels = crop_levels))

# ── compute per-crop scale factors to map area onto yield axis ────────────────
# Scale factor = (max yield range) / (max area range) per crop
# area_scaled = area_kha / scale_factor  →  plots on yield axis
# sec_axis transformation inverts this so labels show real area ('000 ha)

scale_factors <- national %>%
  group_by(commodity) %>%
  summarise(yield_max = max(yield_tha, na.rm = TRUE),
            yield_min = min(yield_tha, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    national_area %>%
      group_by(commodity) %>%
      summarise(area_max = max(area_kha, na.rm = TRUE),
                area_min = min(area_kha, na.rm = TRUE), .groups = "drop"),
    by = "commodity"
  ) %>%
  mutate(
    # map area range onto 80% of yield range, offset to sit near bottom of panel
    scale_f = (area_max - area_min) / ((yield_max - yield_min) * 0.8),
    offset   = yield_min - (area_min / scale_f)
  )

# join scale factors into area data for use in geom_line
national_area <- national_area %>%
  left_join(scale_factors, by = "commodity") %>%
  mutate(area_scaled = area_kha / scale_f + offset)

# ── shared theme ──────────────────────────────────────────────────────────────
yield_theme <- theme_classic() +
  theme(
    strip.background     = element_blank(),
    strip.text           = element_text(face = "bold", size = 10),
    axis.text.x          = element_text(angle = 45, hjust = 1),
    axis.line            = element_line(colour = "grey40"),
    axis.ticks           = element_line(colour = "grey40"),
    panel.grid.major.y   = element_line(colour = "grey92", linewidth = 0.4),
    panel.grid.minor     = element_blank(),
    legend.position      = "top",
    legend.justification = "left",
    legend.text          = element_text(size = 9),
    plot.caption         = element_text(size = 8, colour = "grey40", hjust = 0),
    plot.margin          = margin(10, 15, 10, 10)
  )


# ── plot 1: national (yield + area) ───────────────────────────────────────────
# NOTE on dual axis in free_y facets:
# ggplot2 applies ONE sec_axis transformation globally. With free_y facets,
# each panel has a different y range, so the secondary axis labels will only
# be numerically correct for one crop unless you use a fixed scale.
# The approach here uses pre-scaled data (area_scaled) so the lines sit
# correctly in each panel. The sec_axis label is therefore indicative;
# add a note in the caption or consider ggh4x::facetted_pos_scales() for
# fully independent secondary axes if exact tick labels are needed.

# Using a representative scale factor for the sec_axis label transform
# (wheat, the dominant crop) — or just label the right axis as "Area planted"
# without numeric ticks, which is cleaner for a free_y faceted plot.

p_national <- ggplot(national, aes(x = year)) +
  # area as shaded ribbon (sits behind yield lines)
  geom_line(data = national_area,
            aes(y = area_scaled, colour = "Area planted ('000 ha)"),
            linewidth = 0.7, linetype = "dashed") +
  # yield lines
  geom_line(aes(y = yield_tha, colour = "Annual"), linewidth = 0.45) +
  geom_line(aes(y = roll5,     colour = "5-year rolling average"),
            linewidth = 1.0, na.rm = TRUE) +
  facet_wrap(~commodity, scales = "free_y", ncol = 2) +
  scale_colour_manual(
    values = c(
      "Annual"                    = "grey30",
      "5-year rolling average"    = "#3A9FD5",
      "Area planted ('000 ha)"    = "#C0392B"
    ),
    breaks = c("5-year rolling average", "Annual", "Area planted ('000 ha)")
  ) +
  scale_x_continuous(breaks = seq(1975, 2025, 10)) +
  scale_y_continuous(
    sec.axis = sec_axis(~ ., name = "Area")
  ) +
  labs(x = "Year", y = expression("Yield (t ha"^{-1}*")"), colour = NULL,
       caption = paste0("Source: ABARES Agricultural Commodity Statistics 2024-25\n",
                        "Right axis (area) is scaled independently per crop panel")) +
  yield_theme +
  theme(
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank()
  )

p_national
ggsave(file.path(out_dir, "yield_national.png"), plot = p_national,
       width = 10, height = 8, dpi = 300)

##############################################################################



#
# ── plot 1b: national grid (yield + area, no rolling av) ─────────────────────
library(patchwork)

read_area <- function(filepath, sheet, keep_commodities) {
  df        <- read_excel(filepath, sheet = sheet, col_names = FALSE)
  unit_row  <- as.character(df[8, ])
  area_cols <- which(unit_row == "'000 ha")
  years_raw <- df[12:nrow(df), 1, drop = TRUE]
  year      <- as.integer(substr(as.character(years_raw), 1, 4))
  
  out <- list()
  for (col in area_cols) {
    commodity <- as.character(df[4, col])
    reporter  <- as.character(df[5, col])
    if (!commodity %in% keep_commodities) next
    if (reporter != "Australia") next
    vals <- suppressWarnings(as.numeric(as.character(df[12:nrow(df), col, drop = TRUE])))
    out[[length(out) + 1]] <- tibble(year, commodity, area_kha = vals)
  }
  bind_rows(out)
}

national_area <- map(files, ~read_area(file.path(base, .x$path), .x$sheet, .x$keep)) %>%
  bind_rows() %>%
  filter(!is.na(area_kha)) %>%
  mutate(commodity = factor(commodity, levels = crop_levels))

# ── per-crop dual-axis plot function ──────────────────────────────────────────
make_crop_plot <- function(crop_name,
                           show_x_axis  = FALSE,
                           show_y_left  = FALSE,
                           show_y_right = FALSE,
                           y_lo         = 0,
                           y_hi,
                           y_breaks) {
  
  yld  <- national      %>% filter(commodity == crop_name)
  area <- national_area %>% filter(commodity == crop_name)
  
  a_min <- min(area$area_kha, na.rm = TRUE)
  a_max <- max(area$area_kha, na.rm = TRUE)
  a_pad <- (a_max - a_min) * 0.10
  a_lo  <- max(0, a_min - a_pad)
  a_hi  <- a_max + a_pad
  
  scale_f <- (y_hi - y_lo) / (a_hi - a_lo)
  offset  <- y_lo - a_lo * scale_f
  
  area <- area %>% mutate(area_scaled = area_kha * scale_f + offset)
  sec_trans <- ~ (. - offset) / scale_f
  
  ggplot(yld, aes(x = year)) +
    geom_line(aes(y = yield_tha), colour = "#3A9FD5", linewidth = 0.7) +
    geom_line(data = area, aes(y = area_scaled), colour = "grey50", linewidth = 0.7) +
    scale_x_continuous(breaks = seq(1975, 2025, 10),
                       limits = c(1973, 2026),
                       expand = c(0, 0)) +
    scale_y_continuous(
      limits = c(y_lo, y_hi),
      breaks = y_breaks,
      sec.axis = sec_axis(
        sec_trans,
        name   = if (show_y_right) "Area" else NULL,
        labels = if (show_y_right) scales::label_comma() else function(x) rep("", length(x))
      )
    ) +
    ggtitle(crop_name) +
    yield_theme +
    theme(
      plot.title         = element_text(face = "bold", size = 10, hjust = 0.5),
      legend.position    = "none",
      axis.text.x        = if (show_x_axis) element_text(angle = 45, hjust = 1) else element_blank(),
      axis.ticks.x       = if (show_x_axis) element_line(colour = "grey40")     else element_blank(),
      axis.title.x       = element_blank(),
      axis.title.y.left  = if (show_y_left)  element_text(size = 9)                    else element_blank(),
      axis.title.y.right = if (show_y_right) element_text(size = 9, colour = "grey30") else element_blank(),
      axis.text.y.right  = if (show_y_right) element_text(size = 7, colour = "grey30") else element_blank(),
      axis.ticks.y.right = if (show_y_right) element_line(colour = "grey40")           else element_blank()
    ) +
    labs(y = if (show_y_left) "Yield" else NULL)
}

# ── build individual panels ───────────────────────────────────────────────────

p_barley  <- make_crop_plot("Barley",  show_x_axis = FALSE, show_y_left = FALSE, show_y_right = TRUE,  y_hi = 4, y_breaks = seq(0, 4, 1))
p_lentils <- make_crop_plot("Lentils", show_x_axis = FALSE, show_y_left = FALSE, show_y_right = TRUE,  y_hi = 2.5, y_breaks = seq(0, 2.5, 0.5))
p_sorghum <- make_crop_plot("Sorghum", show_x_axis = TRUE,  show_y_left = TRUE,  show_y_right = FALSE, y_hi = 6, y_breaks = seq(0, 6, 1))
p_wheat   <- make_crop_plot("Wheat",   show_x_axis = FALSE, show_y_left = TRUE, show_y_right = TRUE, y_hi = 4,   y_breaks = seq(0, 4,   1))
p_canola  <- make_crop_plot("Canola",  show_x_axis = FALSE, show_y_left = TRUE, show_y_right = TRUE, y_hi = 2.5, y_breaks = seq(0, 2.5, 0.5))
p_sorghum <- make_crop_plot("Sorghum", show_x_axis = TRUE,  show_y_left = TRUE, show_y_right = TRUE, y_hi = 6,   y_breaks = seq(0, 6,   1))

# ── shared legend ─────────────────────────────────────────────────────────────
p_legend <- ggplot(
  tibble(year = 2000, yield_tha = 1,
         type = factor(c("Yield (t/ha)", "Area planted ('000 ha)"),
                       levels = c("Yield (t/ha)", "Area planted ('000 ha)"))),
  aes(x = year, y = yield_tha, colour = type)
) +
  geom_line() +
  scale_colour_manual(
    values = c("Yield (t/ha)" = "#3A9FD5", "Area planted ('000 ha)" = "grey50")
  ) +
  theme_void() +
  theme(
    legend.position  = "top",
    legend.direction = "horizontal",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 9),
    legend.key.width = unit(1.2, "cm")
  )

# ── assemble grid ─────────────────────────────────────────────────────────────
p_grid <- (p_wheat | p_barley) /
  (p_canola | p_lentils) /
  (p_sorghum | plot_spacer()) +
  plot_annotation(
    caption = "Source: ABARES Agricultural Commodity Statistics 2024-25",
    theme   = theme(plot.caption = element_text(size = 8, colour = "grey40", hjust = 0))
  )

p_national_area <- p_legend / p_grid +
  plot_layout(heights = c(0.04, 1))

p_national_area

ggsave(file.path(out_dir, "yield_area_national.png"), plot = p_national_area,
       width = 10, height = 10, dpi = 300)
