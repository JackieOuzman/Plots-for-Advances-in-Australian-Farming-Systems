#install.packages("read.abares")
library(read.abares)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

#################################################################################
## National wheat data t/ha
#################################################################################

# Pull full national historical estimates dataset
nat_est <- read_historical_national_estimates()

# Inspect what's in there first - check unique industries and variables
unique(nat_est$Industry)
unique(nat_est$Variable)  # or Variable - check snake_case naming

grep("wheat", unique(nat_est$Variable), value = TRUE, ignore.case = TRUE)


Wheat_crop <- nat_est %>%
  filter(
    Industry == "All Broadacre",
    Variable %in% c("Wheat produced (t)", "Wheat area sown (ha)")
  )

Wheat_crop_wide <- Wheat_crop %>%
  select(-RSE) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(wheat_t_per_wheat_sown_ha = round(`Wheat produced (t)` / `Wheat area sown (ha)`, 2))

glimpse(Wheat_crop_wide)


Wheat_crop_wide <- Wheat_crop %>%
  select(-RSE) %>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(
    wheat_t_per_wheat_sown_ha = round(`Wheat produced (t)` / `Wheat area sown (ha)`, 2),
    wheat_t_per_ha_5yr_avg = round(zoo::rollmean(wheat_t_per_wheat_sown_ha, k = 5, fill = NA, align = "right"), 2)
  )



ggplot(Wheat_crop_wide, aes(x = Year)) +
  geom_line(aes(y = wheat_t_per_wheat_sown_ha, colour = "Annual"), linewidth = 0.7) +
  geom_line(aes(y = wheat_t_per_ha_5yr_avg, colour = "5-year rolling average"), linewidth = 1.2) +
  scale_colour_manual(values = c("Annual" = "grey70", "5-year rolling average" = "steelblue4")) +
  scale_y_continuous(limits = c(0, 3.5), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  labs(
    title = "Australian wheat yield",
    caption = "Wheat on all broadacre farms",
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
###############################################################################

#Agricultural commodity statistics 
# I have downloaded already
