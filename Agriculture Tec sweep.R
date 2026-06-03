library(tidyverse)
library(readxl)
df <- read_excel("N:/Advances in Australian Farming Systems Paper/Section3b/Ag2050_TechSweep_Main_Table_Reconstructed.xlsx", sheet = "Products")

# legend mapping
sector_labels <- c(
  "1" = "Broadacre Crops",
  "2" = "Horticulture",
  "3" = "Protected Cropping",
  "4" = "Animal Husbandry",
  "5" = "Aquaculture"
)

sector_counts <- df %>%
  select(`Sector(s)`) %>%
  mutate(row = row_number()) %>%
  separate_rows(`Sector(s)`, sep = ",\\s*") %>%
  filter(`Sector(s)` %in% names(sector_labels)) %>%
  mutate(sector = sector_labels[`Sector(s)`]) %>%
  count(sector) %>%
  mutate(
    pct     = n / nrow(df) * 100,
    sector  = fct_reorder(sector, n),
    highlight = FALSE
  )

p_sector <- ggplot(sector_counts, aes(x = pct, y = sector, fill = highlight)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 0), "%")),
            hjust = -0.2, size = 3.2, colour = "grey30") +
  scale_fill_manual(values = c("FALSE" = "#3A9FD5")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08)),
                     labels = function(x) paste0(x, "%")) +
  labs(x = "Share of products (%)", y = NULL,
       caption = "Source: Ag2050 Technology Sweep (n = 192 products)\nNote: products may be counted in multiple sectors") +
  theme_classic() +
  theme(
    legend.position    = "none",
    axis.line.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.text.y        = element_text(size = 9),
    axis.text.x        = element_text(size = 8),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.4),
    plot.caption       = element_text(size = 8, colour = "grey40", hjust = 0)
  )

p_sector
out_dir <- "N:/Advances in Australian Farming Systems Paper/Section3b"
ggsave(file.path(out_dir, "agtech_by_sector.png"), plot = p_sector, width = 7, height = 4, dpi = 300)




sector_labels <- c(
  "1" = "Broadacre Crops",
  "2" = "Horticulture",
  "3" = "Protected Cropping",
  "4" = "Animal Husbandry",
  "5" = "Aquaculture"
)

sector_cat_counts <- df %>%
  select(`Agtech Category`, `Sector(s)`) %>%
  separate_rows(`Sector(s)`, sep = ",\\s*") %>%
  filter(`Sector(s)` %in% names(sector_labels)) %>%
  mutate(sector = sector_labels[`Sector(s)`]) %>%
  count(sector, `Agtech Category`) %>%
  group_by(sector) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(`Agtech Category` = fct_reorder(`Agtech Category`, pct, .fun = mean))

p_sector_facet <- ggplot(sector_cat_counts,
                         aes(x = pct, y = `Agtech Category`)) +
  geom_col(width = 0.7, fill = "#1a3a5c") +
  geom_text(aes(label = paste0(round(pct, 0), "%")),
            hjust = -0.2, size = 2.8, colour = "grey30") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = function(x) paste0(x, "%")) +
  facet_wrap(~sector, ncol = 3) +
  labs(x = "Share of products within sector (%)", y = NULL,
       caption = "Source: Ag2050 Technology Sweep\nNote: products may appear in multiple sectors") +
  theme_classic() +
  theme(
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold", size = 9),
    axis.line.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.text.y        = element_text(size = 8),
    axis.text.x        = element_text(size = 7),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.4),
    plot.caption       = element_text(size = 8, colour = "grey40", hjust = 0)
  )

p_sector_facet
ggsave(file.path(out_dir, "agtech_by_sector_facet.png"), plot = p_sector_facet,
       width = 12, height = 7, dpi = 300)
