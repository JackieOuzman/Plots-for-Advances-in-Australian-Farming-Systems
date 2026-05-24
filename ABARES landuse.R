#============================================================
  # ABARES NLUM Land Use Change Sankey: 2010-11 to 2020-21
  # ============================================================
install.packages("ggalluvial")
library(terra)
library(read.abares)   # install.packages("read.abares")
library(dplyr)
library(ggplot2)
library(ggalluvial)    # install.packages("ggalluvial")
library(stringr)


# ============================================================
# 1. LOAD RASTERS
# ============================================================



# --- Option B (if you've already downloaded the zips) ---
path2011 <- "N:/Advances in Australian Farming Systems Paper/Section 1/landuse maps/2010_11/"
path2021 <- "N:/Advances in Australian Farming Systems Paper/Section 1/landuse maps/2020_21/"

lu_2011 <- rast(paste0(path2011, "NLUM_v7_250_ALUMV8_2010_11_alb.tif"))
lu_2021 <- rast(paste0(path2021, "NLUM_v7_250_ALUMV8_2020_21_alb.tif"))                        

# ============================================================
# 2. LOAD LOOKUP TABLES
# ============================================================
lookup_2021 <- read.csv(paste0(path2021, "NLUM_v7_250_ALUMV8_2020_21_alb.csv"))
lookup_2011 <- read.csv(paste0(path2011, "NLUM_v7_250_ALUMV8_2010_11_alb.csv"))

# Build SECV8 -> AGIND lookup (use 2021 lookup; structure identical)
agind_lookup <- lookup_2021 |>
  select(SECV8, AGIND) |>
  distinct() |>
  filter(SECV8 != "No data/offshore",
         AGIND != "No data/offshore")

# Check AGIND categories
unique(agind_lookup$AGIND)

# ============================================================
# 3. SET ACTIVE CATEGORY AND STACK RASTERS
# ============================================================
activeCat(lu_2011) <- "SECV8"
activeCat(lu_2021) <- "SECV8"

# Resample 2021 to match 2011 grid (should already match but just in case)
lu_2021_matched <- resample(lu_2021, lu_2011, method = "near")

# Stack into two-layer raster
lu_stack <- c(lu_2011, lu_2021_matched)
names(lu_stack) <- c("lu_2011", "lu_2021")

# ============================================================
# 4. RANDOM SAMPLE
# ~500k cells from 290M total (~0.17%) - sufficient for
# proportional transition analysis at national scale
# ============================================================
set.seed(42)
df_sec <- spatSample(lu_stack, size = 500000, method = "random",
                     as.df = TRUE, na.rm = TRUE)
names(df_sec) <- c("from_2011", "to_2021")
# Check first, then rename and filter in one go
head(df_sec)
names(df_sec)

df_sec <- df_sec |>
  rename(from_2011 = lu_2011, to_2021 = lu_2021) |>
  filter(from_2011 != "No data/offshore",
         to_2021   != "No data/offshore")

head(df_sec)


transitions_agind <- df_sec |>
  count(from_2011, to_2021, name = "n_pixels") |>
  mutate(area_Mha = (n_pixels * 6.25) / 1e6) |>
  filter(area_Mha > 0.005) |>
  left_join(agind_lookup, by = c("from_2011" = "SECV8")) |>
  rename(from_agind = AGIND) |>
  left_join(agind_lookup, by = c("to_2021" = "SECV8")) |>
  rename(to_agind = AGIND)

transitions_summary <- transitions_agind |>
  group_by(from_agind, to_agind) |>
  summarise(area_Mha = sum(area_Mha), .groups = "drop") |>
  filter(!is.na(from_agind), !is.na(to_agind)) |>
  arrange(desc(area_Mha))

transitions_summary |> as.data.frame() |> print()

# ============================================================
# SCALING FACTOR
# ============================================================

# Get total number of non-NA cells in the 2011 raster
total_cells <- global(lu_2011, fun = "notNA")[[1]]
sampled_cells <- 500000

scaling_factor <- total_cells / sampled_cells

cat("Total non-NA cells:", total_cells, "\n")
cat("Scaling factor:    ", round(scaling_factor, 1), "\n")

# Apply scaling factor to transitions_summary
transitions_summary <- transitions_summary |>
  mutate(area_Mha_scaled = area_Mha * scaling_factor)

# Check - these should now be plausible national Mha figures
transitions_summary |> as.data.frame() |> print()


# Colours matched to your actual AGIND categories
agind_colours <- c(
  "Not agricultural industry" = "#267300",
  "Grazing native vegetation" = "#70a800",
  "Grazing modified pastures" = "#c8e86e",
  "Cropping"                  = "#f5c518"
)


p <- ggplot(transitions_summary,
            aes(axis1 = from_agind,
                axis2 = to_agind,
                y = area_Mha_scaled)) +        # <-- scaled
  geom_alluvium(aes(fill = from_agind), alpha = 0.8, width = 1/8) +
  geom_stratum(width = 1/4, fill = "white", colour = "grey40", linewidth = 0.4) +
  geom_text(stat = "stratum",
            aes(label = str_wrap(after_stat(stratum), 18)),
            size = 3.2, lineheight = 0.9, fontface = "bold") +
  scale_x_discrete(limits = c("2010–11", "2020–21"),
                   expand = c(0.3, 0.05)) +
  scale_fill_manual(values = agind_colours) +
  scale_y_continuous(labels = label_number(suffix = " Mha")) +
  labs(
    title    = "Australian Land Use Change: 2010–11 to 2020–21",
    subtitle = paste0("Flow width proportional to estimated area (million hectares)\n",
                      "Scaled from random sample of 500,000 cells (scaling factor: ",
                      round(scaling_factor, 0), "×) from NLUM v7"),
    x        = NULL,
    y        = "Area (million hectares)",
    caption  = "Source: ABARES NLUM v7, ALUM Classification v8. CC BY 4.0."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid      = element_blank(),
    axis.text.x     = element_text(size = 13, face = "bold"),
    axis.text.y     = element_text(size = 9),
    plot.title      = element_text(size = 13, face = "bold"),
    plot.subtitle   = element_text(size = 9, colour = "grey40"),
    plot.caption    = element_text(size = 8, colour = "grey50")
  )

print(p)

ggsave("land_use_sankey_2011_2021.png", p,
       width = 14, height = 10, dpi = 300, bg = "white")
