#install.packages("read.abares")
library(read.abares)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(readxl)
library(stringr)

#################################################################################
## National wheat data t/ha using ABARES_Ag_commodities
#################################################################################





path <- "N:/Advances in Australian Farming Systems Paper/Section 1/ABARES_Ag_commodities/21_ACS2024_25_WheatTables_v1.0.0.xlsx"

# Read the Wheat sheet, skipping the 11 header rows
# Col 1 = fiscal year, Col 2 = area sown ('000 ha), Col 4 = production (kt)
abares_hist <- read_excel(
  path,
  sheet = "Wheat",
  skip = 11,
  col_names = FALSE
) %>%
  select(
    fiscal_year = 1,
    area_sown_000ha = 2,   # '000 ha
    production_kt = 4      # kt
  ) %>%
  filter(!is.na(fiscal_year), str_detect(fiscal_year, "\\d{4}")) %>%
  mutate(
    # Extract start year from "1974–75" → 1974, then fiscal year = start + 1
    Year = as.integer(str_extract(fiscal_year, "^\\d{4}")) + 1,
    `Wheat area sown (ha)` = area_sown_000ha * 1000,
    `Wheat produced (t)`   = production_kt * 1000,
    wheat_t_per_wheat_sown_ha = round(`Wheat produced (t)` / `Wheat area sown (ha)`, 2)
  ) %>%
  select(Year, `Wheat area sown (ha)`, `Wheat produced (t)`, wheat_t_per_wheat_sown_ha)


abares_hist <- abares_hist %>%
  mutate(wheat_t_per_ha_5yr_avg = round(zoo::rollmean(wheat_t_per_wheat_sown_ha, 5, fill = NA, align = "right"), 2))


ggplot(abares_hist, aes(x = Year)) +
  geom_line(aes(y = wheat_t_per_wheat_sown_ha, colour = "Annual"), linewidth = 0.7) +
  geom_line(aes(y = wheat_t_per_ha_5yr_avg, colour = "5-year rolling average"), linewidth = 1.2) +
  scale_colour_manual(values = c("Annual" = "grey70", "5-year rolling average" = "steelblue4")) +
  scale_y_continuous(limits = c(0, 3.5), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  labs(
    #title = "Australian wheat yield",
    caption = "Source: ABARES Agricultural Commodity Statistics",
    x = NULL,
    y = "Wheat Yield (t ha⁻¹)",
    colour = NULL
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11),
    plot.title = element_text(size = 12, face = "bold", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, colour = "grey40", hjust = 0, margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.line = element_line(colour = "black", linewidth = 0.4),
    axis.ticks = element_line(colour = "black", linewidth = 0.4),
    legend.position = c(0.15, 0.92),
    legend.background = element_blank(),
    legend.key.width = unit(1.2, "cm"),
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
    plot.margin = margin(10, 15, 10, 10)
  )



###############################################################################
### with captions
##############################################################################

ggplot(abares_hist, aes(x = Year)) +
  geom_line(aes(y = wheat_t_per_wheat_sown_ha, colour = "Annual"), linewidth = 0.7) +
  geom_line(aes(y = wheat_t_per_ha_5yr_avg, colour = "5-year rolling average"), linewidth = 1.2) +
  # Drought arrows pointing UP to low yield years
  annotate("segment", x = 1983, xend = 1983, y = 0.5, yend = 0.75,
           arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  annotate("text", x = 1983, y = 0.3, label = "1982–83\nDrought",
           size = 2.8, family = "serif", colour = "black", hjust = 0.5) +
  
  annotate("segment", x = 2003, xend = 2003, y = 0.5, yend = 0.75,
           arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  annotate("segment", x = 2007, xend = 2007, y = 0.5, yend = 0.75,
           arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  annotate("text", x = 2005, y = 0.3, label = "Millennium\nDrought",
           size = 2.8, family = "serif", colour = "black", hjust = 0.5) +
  
  annotate("segment", x = 2017, xend = 2017, y = 0.5, yend = 0.75,
           arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  annotate("segment", x = 2020, xend = 2020, y = 0.5, yend = 0.75,
           arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  annotate("text", x = 2019, y = 0.3, label = "2017\nDrought",
           size = 2.8, family = "serif", colour = "black", hjust = 0.5) +
  
  # Technology/management annotations down UP
  annotate("segment", x = 1980, xend = 1980, y = 2.5, yend = 2.1,
           arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  annotate("text", x = 1980, y = 1.95, label = "Herbicides, no-till,\nbreak crops, nitrogen",
           size = 2.8, family = "serif", colour = "black", hjust = 0.5) +
  annotate("segment", x = 2017, xend = 2017, y = 3.3, yend = 3.0,
           arrow = arrow(length = unit(0.2, "cm")), colour = "black") +
  annotate("text", x = 2017, y = 2.8, label = "Fallow management, soil\nmanagement, early sowing,\nlogistics",
           size = 2.8, family = "serif", colour = "black", hjust = 0.5) +
  
  scale_colour_manual(values = c("Annual" = "grey70", "5-year rolling average" = "steelblue4")) +
  scale_y_continuous(limits = c(0, 3.5), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  labs(
    title = "Australian wheat yield",
    caption = "Source: ABARES Agricultural Commodity Statistics",
    x = NULL,
    y = "Yield (t ha⁻¹)",
    colour = NULL
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "serif", size = 11),
    plot.title = element_text(size = 12, face = "bold", margin = margin(b = 10)),
    plot.caption = element_text(size = 9, colour = "grey40", hjust = 0, margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.line = element_line(colour = "black", linewidth = 0.4),
    axis.ticks = element_line(colour = "black", linewidth = 0.4),
    legend.position = c(0.15, 0.92),
    legend.background = element_blank(),
    legend.key.width = unit(1.2, "cm"),
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
    plot.margin = margin(10, 15, 10, 10)
  )
