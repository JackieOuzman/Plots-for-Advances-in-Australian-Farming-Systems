#install.packages("read.abares")
#install.packages("ggforce")
library(ggforce)
library(read.abares)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)
library(readxl)
library(stringr)

#################################################################################
## National wheat data t/ha using ABARES_Ag_commodities and crop report
#################################################################################

abares_hist <- read.csv("N:/Advances in Australian Farming Systems Paper/Section 1/ABARES_Ag_commodities/wheat_ABARES.csv")
wheat_est   <- read.csv("N:/Advances in Australian Farming Systems Paper/Section 1/ABARES_crop_reports/wheat_ABARES_est.csv")

unique(abares_hist$Year)
unique(wheat_est$Year)

names(abares_hist)
names(wheat_est)


wheat_combined <- abares_hist %>%
  bind_rows(
    wheat_est %>% 
      filter(Year == 2026) %>%
      rename(
        `Wheat area sown (ha)` = `Wheat.area.sown..ha.`,
        `Wheat produced (t)`   = `Wheat.produced..t.`
      )
  ) %>%
  filter(!is.na(Year)) %>%
  arrange(Year) %>%
  select(-wheat_t_per_ha_5yr_avg) %>%
  mutate(
    wheat_t_per_ha_5yr_avg = zoo::rollmean(wheat_t_per_wheat_sown_ha, k = 5,
                                           fill = NA, align = "right")
  )




###############################################################################
### with captions
##############################################################################



##################################################################################


ggplot(wheat_combined, aes(x = Year)) +
  geom_line(aes(y = wheat_t_per_wheat_sown_ha, colour = "Annual"), linewidth = 1) +
  geom_line(aes(y = wheat_t_per_ha_5yr_avg, colour = "5-year rolling average"), linewidth = 2) +
  
  # 1982-83 Drought
  annotate("text", x = 1982.5, y = 0.35, label = "Drought",
           size = 4, family = "serif", colour = "black", hjust = 0.5) +
  annotate("segment", x = 1982, xend = 1983, y = 0.15, yend = 0.15,
           colour = "black", linewidth = 0.6) +
  
  # Millennium Drought
  annotate("text", x = 2005, y = 0.35, label = "Drought",
           size = 4, family = "serif", colour = "black", hjust = 0.5) +
  annotate("segment", x = 2003, xend = 2007, y = 0.15, yend = 0.15,
           colour = "black", linewidth = 0.6) +
  
  # 2017-2020 Drought
  annotate("text", x = 2018.5, y = 0.35, label = "Drought",
           size = 4, family = "serif", colour = "black", hjust = 0.5) +
  annotate("segment", x = 2017, xend = 2020, y = 0.15, yend = 0.15,
           colour = "black", linewidth = 0.6) +
  
  # Technology/management label boxes
  annotate("label", x = 1982, y = 2.3,
           label = "Herbicides, no-till,\nbreak crops, nitrogen",
           size = 4, family = "serif", colour = "black",
           fill = "white", label.r = unit(0.2, "lines"),
           label.padding = unit(0.3, "lines"),
           label.size = 0.3, hjust = 0.5) +
  annotate("label", x = 2016, y = 3.1,
           label = "Fallow management,\nearly sowing,\nlogistics",
           size = 4, family = "serif", colour = "black",
           fill = "white", label.r = unit(0.2, "lines"),
           label.padding = unit(0.3, "lines"),
           label.size = 0.3, hjust = 0.5) +
  
  scale_colour_manual(values = c("Annual" = "grey30", "5-year rolling average" = "dodgerblue")) +
  scale_y_continuous(limits = c(0, 3.5), expand = c(0, 0)) +
  scale_x_continuous(
    expand = c(0.01, 0),
    limits = c(1975, 2027),
    breaks = c(seq(1980, 2020, by = 10), 2026),
    minor_breaks = seq(1975, 2026, by = 5),
    labels = c(seq(1980, 2020, by = 10), "2026"),
    guide = guide_axis(minor.ticks = TRUE)
  ) +
  labs(
    caption = "Source: ABARES Agricultural Commodity Statistics",
    x       = NULL,
    y       = "Wheat Yield (t ha⁻¹)",
    colour  = NULL
  ) +
  theme_classic() +
  theme(
    text             = element_text(family = "serif", size = 11),
    plot.caption     = element_text(size = 12, colour = "grey30", hjust = 0, margin = margin(t = 8)),
    axis.text        = element_text(size = 13),
    axis.title.y     = element_text(size = 13, margin = margin(r = 8)),
    axis.line        = element_line(colour = "black", linewidth = 0.4),
    axis.ticks       = element_line(colour = "black", linewidth = 0.4),
    axis.ticks.length            = unit(0.15, "cm"),
    axis.minor.ticks.length.x    = rel(0.8),
    axis.minor.ticks.x.bottom    = element_line(colour = "black", linewidth = 0.4),
    legend.position              = c(0.15, 0.92),
    legend.background            = element_blank(),
    legend.key.width             = unit(1.2, "cm"),
    panel.grid.major.y           = element_line(colour = "grey92", linewidth = 0.3),
    plot.margin                  = margin(10, 15, 10, 10)
    
    
  )


### No legend
no_legend <- ggplot(wheat_combined, aes(x = Year)) +
  geom_line(aes(y = wheat_t_per_wheat_sown_ha, colour = "Annual"), linewidth = 1) +
  geom_line(aes(y = wheat_t_per_ha_5yr_avg, colour = "5-year rolling average"), linewidth = 2) +
  
  # 1982-83 Drought
  annotate("text", x = 1982.5, y = 0.35, label = "Drought",
           size = 4, family = "serif", colour = "black", hjust = 0.5) +
  annotate("segment", x = 1982, xend = 1983, y = 0.15, yend = 0.15,
           colour = "black", linewidth = 0.6) +
  
  # Millennium Drought
  annotate("text", x = 2005, y = 0.35, label = "Drought",
           size = 4, family = "serif", colour = "black", hjust = 0.5) +
  annotate("segment", x = 2003, xend = 2007, y = 0.15, yend = 0.15,
           colour = "black", linewidth = 0.6) +
  
  # 2017-2020 Drought
  annotate("text", x = 2018.5, y = 0.35, label = "Drought",
           size = 4, family = "serif", colour = "black", hjust = 0.5) +
  annotate("segment", x = 2017, xend = 2020, y = 0.15, yend = 0.15,
           colour = "black", linewidth = 0.6) +
  
  # Technology/management label boxes
  annotate("label", x = 1982, y = 2.3,
           label = "Herbicides, no-till,\nbreak crops, nitrogen",
           size = 4, family = "serif", colour = "black",
           fill = "white", label.r = unit(0.2, "lines"),
           label.padding = unit(0.3, "lines"),
           label.size = 0.3, hjust = 0.5) +
  annotate("label", x = 2016, y = 3.1,
           label = "Fallow management,\nearly sowing,\nlogistics",
           size = 4, family = "serif", colour = "black",
           fill = "white", label.r = unit(0.2, "lines"),
           label.padding = unit(0.3, "lines"),
           label.size = 0.3, hjust = 0.5) +
  
  scale_colour_manual(values = c("Annual" = "grey30", "5-year rolling average" = "dodgerblue")) +
  scale_y_continuous(limits = c(0, 3.5), expand = c(0, 0)) +
  scale_x_continuous(
    expand = c(0.01, 0),
    limits = c(1975, 2027),
    breaks = c(seq(1980, 2020, by = 10), 2026),
    minor_breaks = seq(1975, 2026, by = 5),
    labels = c(seq(1980, 2020, by = 10), "2026"),
    guide = guide_axis(minor.ticks = TRUE)
  ) +
  labs(
    caption = "Source: ABARES Agricultural Commodity Statistics",
    x       = NULL,
    y       = "Wheat Yield (t ha⁻¹)",
    colour  = NULL
  ) +
  theme_classic() +
  theme(
    text              = element_text(family = "serif", size = 11),
    plot.caption      = element_text(size = 12, colour = "grey30", hjust = 0, margin = margin(t = 8)),
    axis.text         = element_text(size = 13),
    axis.title.y      = element_text(size = 13, margin = margin(r = 8)),
    axis.line         = element_line(colour = "black", linewidth = 0.4),
    axis.ticks        = element_line(colour = "black", linewidth = 0.4),
    axis.ticks.length           = unit(0.15, "cm"),
    axis.minor.ticks.length.x   = rel(0.8),
    axis.minor.ticks.x.bottom   = element_line(colour = "black", linewidth = 0.4),
    legend.position             = "none",
    panel.grid.major.y          = element_line(colour = "grey92", linewidth = 0.3),
    plot.margin                 = margin(10, 15, 10, 10)
  )



# Save plot
ggsave(
  "N:/Advances in Australian Farming Systems Paper/Section 1/Wheat_t_ha_plot/wheat_yield_plot.png",
  width = 18, height = 12, units = "cm", dpi = 300
)

ggsave(
  plot     = no_legend,
  filename = "N:/Advances in Australian Farming Systems Paper/Section 1/Wheat_t_ha_plot/wheat_yield_plot_no_legend.png",
  width    = 18, height = 12, units = "cm", dpi = 300
)

# Save data
write.csv(
  wheat_combined,
  "N:/Advances in Australian Farming Systems Paper/Section 1/Wheat_t_ha_plot/wheat_combined.csv",
  row.names = FALSE
)
