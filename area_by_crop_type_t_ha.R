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

crops      <- c("Wheat", "Barley", "Canola", "Sorghum", "Lentils")
crop_levels <- c("Wheat", "Barley", "Canola", "Sorghum", "Lentils")

files <- list(
  list(path = "21_ACS2024_25_WheatTables_v1.0.0 (1).xlsx", sheet = "Wheat",         keep = "Wheat"),
  list(path = "04_ACS2024_25_CoarseGrainsTables_v1.0.0.xlsx", sheet = "CoarseGrains1", keep = c("Barley", "Sorghum")),
  list(path = "16_ACS2024_25_OilseedsTables_v1.0.0.xlsx",   sheet = "Oilseeds1",    keep = "Canola"),
  list(path = "17_ACS2024_25_PulsesTables_v1.0.0.xlsx", sheet = "Pulses", keep = "Lentils")
)

# ── national data ─────────────────────────────────────────────────────────────
national <- map(files, ~read_yield(file.path(base, .x$path), .x$sheet, .x$keep)) %>%
  bind_rows() %>%
  filter(!is.na(yield_tha)) %>%
  mutate(commodity = factor(commodity, levels = crop_levels)) %>%
  arrange(commodity, year) %>%
  group_by(commodity) %>%
  mutate(roll5 = rollmean(yield_tha, k = 5, fill = NA, align = "right")) %>%
  ungroup()

# ── state data ────────────────────────────────────────────────────────────────
by_state <- map(files, ~read_yield(file.path(base, .x$path), .x$sheet, .x$keep, states_only = TRUE)) %>%
  bind_rows() %>%
  filter(!is.na(yield_tha)) %>%
  mutate(
    commodity = factor(commodity, levels = crop_levels),
    state     = factor(state, levels = c("WA", "SA", "VIC", "NSW", "QLD"))
  ) %>%
  arrange(commodity, state, year) %>%
  group_by(commodity, state) %>%
  mutate(roll5 = rollmean(yield_tha, k = 5, fill = NA, align = "right")) %>%
  ungroup()

# ── state grouped data ────────────────────────────────────────────────────────
by_region <- by_state %>%
  mutate(region = case_when(
    state == "WA"               ~ "WA",
    state %in% c("SA", "VIC")  ~ "SA & VIC",
    state %in% c("NSW", "QLD") ~ "NSW & QLD"
  )) %>%
  group_by(commodity, region, year) %>%
  summarise(roll5 = mean(roll5, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.nan(roll5)) %>%
  mutate(region = factor(region, levels = c("WA", "SA & VIC", "NSW & QLD")))

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

# ── plot 1: national ──────────────────────────────────────────────────────────
p_national <- ggplot(national, aes(x = year)) +
  geom_line(aes(y = yield_tha, colour = "Annual"), linewidth = 0.45) +
  geom_line(aes(y = roll5,     colour = "5-year rolling average"),
            linewidth = 1.0, na.rm = TRUE) +
  facet_wrap(~commodity, scales = "free_y", ncol = 2) +
  scale_colour_manual(
    values = c("Annual" = "grey30", "5-year rolling average" = "#3A9FD5"),
    breaks = c("5-year rolling average", "Annual")
  ) +
  scale_x_continuous(breaks = seq(1975, 2025, 10)) +
  labs(x = "Year", y = expression("Yield (t ha"^{-1}*")"), colour = NULL,
       caption = "Source: ABARES Agricultural Commodity Statistics 2024-25") +
  yield_theme

# ── plot 2: by state ──────────────────────────────────────────────────────────
state_colours <- c(WA = "#3A9FD5", SA = "#E06C00", VIC = "#2E8B57", NSW = "#8B2FC9", QLD = "#C0392B")

p_state <- ggplot(by_state, aes(x = year, y = roll5, colour = state)) +
  geom_line(linewidth = 1.0, na.rm = TRUE) +
  facet_wrap(~commodity, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = state_colours) +
  scale_x_continuous(breaks = seq(1975, 2025, 10)) +
  labs(x = "Year", y = expression("Yield (t ha"^{-1}*")"), colour = NULL,
       caption = "Source: ABARES Agricultural Commodity Statistics 2024-25\n5-year rolling average shown") +
  yield_theme

# ── plot 3: by region ─────────────────────────────────────────────────────────
region_colours <- c("WA" = "#3A9FD5", "SA & VIC" = "#2E8B57", "NSW & QLD" = "grey60")

p_region <- ggplot(by_region, aes(x = year, y = roll5, colour = region)) +
  geom_line(linewidth = 1.0, na.rm = TRUE) +
  facet_wrap(~commodity, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = region_colours) +
  scale_x_continuous(breaks = seq(1975, 2025, 10)) +
  labs(x = "Year", y = expression("Yield (t ha"^{-1}*")"), colour = NULL,
       caption = "Source: ABARES Agricultural Commodity Statistics 2024-25\n5-year rolling average, grouped state means shown") +
  yield_theme

# ── save plots ────────────────────────────────────────────────────────────────
p_national
p_state
p_region

ggsave(file.path(out_dir, "yield_national.png"),      plot = p_national, width = 10, height = 8, dpi = 300)
ggsave(file.path(out_dir, "yield_by_state.png"),      plot = p_state,    width = 10, height = 8, dpi = 300)
ggsave(file.path(out_dir, "yield_by_region.png"),     plot = p_region,   width = 10, height = 8, dpi = 300)

# ── save data ─────────────────────────────────────────────────────────────────
write_csv(national,  file.path(out_dir, "yield_national.csv"))
write_csv(by_state,  file.path(out_dir, "yield_by_state.csv"))
write_csv(by_region, file.path(out_dir, "yield_by_region.csv"))
