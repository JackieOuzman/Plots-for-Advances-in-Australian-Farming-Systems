# =============================================================================
# Farm Consolidation in Australia: ABS Agricultural Commodities Data
# =============================================================================
#
# PURPOSE:
#   Read and display ABS data on the number of agricultural businesses
#   by commodity type for two time points (2010-11 and 2021-22), as a
#   first step toward building a plot showing the long-run trend toward
#   farm consolidation (fewer, larger farms) in Australian grain and
#   mixed livestock systems.
#
# DATA SOURCES:
#   1. ABS Agricultural Commodities, Australia 2010-11 (cat. 7121.0)
#      Format: Wide — rows are commodity/land-use categories; columns are
#              Total + states, each with Estimate and No. of businesses.
#      Downloaded from:
#      https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/7121.02010-11?OpenDocument
#      File: N:/Advances in Australian Farming Systems Paper/Section 1/
#            Farm size and Consolidation/1_71210do001_201011.xls
#
#   2. ABS Agricultural Commodities, Australia 2021-22 (cat. 7121.0)
#      Format: Long — rows are commodity codes; columns include Estimate
#              and Number of agricultural businesses.
#      Downloaded from:
#      https://www.abs.gov.au/statistics/industry/agriculture/
#      agricultural-commodities-australia/2021-22#data-downloads
#      File: N:/Advances in Australian Farming Systems Paper/Section 1/
#            Farm size and Consolidation/2_AGCDCNAT_STATE202122.xlsx
#
# NOTE ON DATA STRUCTURE:
#   These files report business counts attached to commodity rows (e.g.
#   wheat area, sheep numbers), NOT by ANZSIC industry classification.
#   The approach below extracts business counts from comparable commodity
#   rows across the two years to allow approximate cross-year comparison.
#   For a full industry-type time series, ABARES farm survey data (via
#   the abares R package) is recommended as a complement.
#
# AUTHOR: [Jackie]
# DATE:   May 2026
# =============================================================================

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(read.abares)


# -----------------------------------------------------------------------------
# FILE PATHS — update if files are moved
# -----------------------------------------------------------------------------
path_2010 <- "N:/Advances in Australian Farming Systems Paper/Section 1/Farm size and Consolidation/1_71210do001_201011.xls"
path_2021 <- "N:/Advances in Australian Farming Systems Paper/Section 1/Farm size and Consolidation/2_AGCDCNAT_STATE202122.xlsx"

# -----------------------------------------------------------------------------
# SECTION 1: Read 2010-11 data
# -----------------------------------------------------------------------------
# Structure (from screenshot):
#   Row 5: column headers (wide format)
#   Row 6: sub-headers ("Estimate", "Number of agricultural businesses", repeated per state)
#   Row 7: units ("no.", "no.", ...)
#   Data starts row 8
#   Columns: A = row label | D = Total Estimate | E = Total No. of businesses
#            (then repeated per state from col F onward)
#
# We want: row label (col A) + Total No. of agricultural businesses (col E)

cat("\n=== Reading 2010-11 data ===\n")


raw_2010 <- read_xls(
  path_2010,
  sheet     = "Table_1",
  col_names = FALSE,
  skip      = 4   # skip rows 1-4 (title, release date, blank, table heading)
)

# Preview the first 10 rows to confirm structure
cat("\nFirst 10 rows of raw 2010-11 file (cols 1-6):\n")
print(raw_2010[1:10, 1:6])

# Col 1 = row label, Col 5 = Total "Number of agricultural businesses"
# Adjust column index below if preview shows differently

data_2010 <- raw_2010 %>%
  slice(-(1:3)) %>%                        # drop header rows
  select(row_label = 1, n_businesses = 3) %>% #clm c 
  mutate(
    row_label    = as.character(row_label),
    n_businesses = suppressWarnings(as.numeric(n_businesses)),
    year         = "2010-11"
  ) %>%
  filter(!is.na(row_label), !is.na(n_businesses))

cat("\nAll rows with business counts in 2010-11:\n")
print(data_2010, n = Inf)



# -----------------------------------------------------------------------------
# SECTION 2: Read 2021-22 data
# -----------------------------------------------------------------------------
# Structure (from screenshot):
#   Row 7: column headers
#   Col A = Region code | Col B = Region label | Col C = Commodity code
#   Col D = Commodity description | Col E = Estimate | Col F = RSE (%)
#   Col G = Number of agricultural businesses | Col H = RSE for businesses (%)
#
# We want: Australia-level rows only (Region code = 0), cols D, E, G

cat("\n\n=== Reading 2021-22 data ===\n")

raw_2021 <- read_xlsx(
  path_2021,
  sheet     = "Table 1",
  col_names = FALSE,
  skip      = 6   # skip rows 1-6 (title block); row 7 = headers
)

# Preview first 10 rows
cat("\nFirst 10 rows of raw 2021-22 file (cols 1-8):\n")
print(raw_2021[1:10, 1:8])

data_2021 <- raw_2021 %>%
  slice(-1) %>%                             # drop the header row
  select(
    region_label          = 2,
    commodity_description = 4,
    estimate              = 5,
    n_businesses          = 7
  ) %>%
  mutate(
    region_label  = as.character(region_label),   # keep as text
    estimate      = suppressWarnings(as.numeric(estimate)),
    n_businesses  = suppressWarnings(as.numeric(n_businesses)),
    year          = "2021-22"
  ) %>%
  filter(region_label == "Australia", !is.na(n_businesses))      # Australia total only

cat("\nAll Australia-level rows with business counts in 2021-22:\n")
print(data_2021 %>% select(commodity_description, estimate, n_businesses, year), n = Inf)


# -----------------------------------------------------------------------------
# SECTION 3: Identify comparable rows across years
# -----------------------------------------------------------------------------


# See all commodity descriptions in 2021-22
data_2021 %>% 
  select(commodity_description, n_businesses) %>% 
  arrange(desc(n_businesses)) %>% 
  print(n = Inf)
# See all row labels in 2010-11
data_2010 %>% 
  select(row_label, n_businesses) %>% 
  arrange(desc(n_businesses)) %>% 
  print(n = Inf)



# -----------------------------------------------------------------------------
# Build comparable farm business count data frame across 2010-11 and 2021-22
# -----------------------------------------------------------------------------

comparable_farms <- bind_rows(
  
  # --- 2010-11 rows ---
  data_2010 %>%
    filter(row_label %in% c(
      "Area of holding - Total area of holding (ha)",
      "Broadacre crops - Cereal crops - Wheat for grain - Area (ha)",
      "Broadacre crops - Cereal crops - Barley for grain - Area (ha)",
      "Broadacre crops - Non-cereal crops - Oilseeds - Canola - Area (ha)",
      "Livestock - Sheep - Total Sheep (no.)",
      "Livestock - Meat cattle - Total (no.)",
      "Livestock - Dairy cattle - Total (no.)"
    )) %>%
    mutate(
      farm_type = case_when(
        str_detect(row_label, "Total area of holding")  ~ "All farms",
        str_detect(row_label, "Wheat")                  ~ "Wheat growers",
        str_detect(row_label, "Barley")                 ~ "Barley growers",
        str_detect(row_label, "Canola")                 ~ "Canola growers",
        str_detect(row_label, "Sheep")                  ~ "Sheep farmers",
        str_detect(row_label, "Meat cattle")            ~ "Beef farmers",
        str_detect(row_label, "Dairy")                  ~ "Dairy farmers"
      )
    ),
  
  # --- 2021-22 rows ---
  data_2021 %>%
    filter(commodity_description %in% c(
      "Area of holding - Total area (ha) (a)",
      "Cereal crops - Wheat for grain - Area (ha)",
      "Cereal crops - Barley for grain - Area (ha)",
      "Other crops - Oilseeds - Canola - Area (ha)",
      "Livestock - Sheep and lambs - Total (no.)",
      "Livestock - Meat cattle - Total (no.)",
      "Livestock - Dairy cattle - Total (no.)"
    )) %>%
    rename(row_label = commodity_description) %>%
    mutate(
      farm_type = case_when(
        str_detect(row_label, "Total area")   ~ "All farms",
        str_detect(row_label, "Wheat")        ~ "Wheat growers",
        str_detect(row_label, "Barley")       ~ "Barley growers",
        str_detect(row_label, "Canola")       ~ "Canola growers",
        str_detect(row_label, "Sheep")        ~ "Sheep farmers",
        str_detect(row_label, "Meat cattle")  ~ "Beef farmers",
        str_detect(row_label, "Dairy")        ~ "Dairy farmers"
      )
    )
  
) %>%
  select(farm_type, year, n_businesses) %>%
  mutate(n_businesses = round(n_businesses))

# Display
print(comparable_farms, n = Inf)



library(ggplot2)
library(dplyr)

# -----------------------------------------------------------------------------
# Prepare data for plotting
# -----------------------------------------------------------------------------

plot_df <- comparable_farms %>%
  filter(farm_type != "All farms") %>%          # plot separately or exclude
  mutate(
    farm_type = factor(farm_type, levels = c(
      "Wheat growers", "Barley growers", "Canola growers",
      "Sheep farmers", "Beef farmers", "Dairy farmers"
    )),
    year = factor(year, levels = c("2010-11", "2021-22"))
  )

# -----------------------------------------------------------------------------
# Plot
# -----------------------------------------------------------------------------

p1 <- ggplot(plot_df, aes(x = farm_type, y = n_businesses, fill = year)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(
    aes(label = scales::comma(n_businesses)),
    position = position_dodge(width = 0.7),
    vjust = -0.5, size = 3, colour = "grey30"
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.12))   # headroom for labels
  ) +
  scale_fill_manual(values = c("2010-11" = "#378ADD", "2021-22" = "#1D9E75")) +
  labs(
    title    = "Decline in farm businesses by commodity type, Australia",
    subtitle = "Number of agricultural businesses reporting each commodity, 2010–11 vs 2021–22",
    x        = NULL,
    y        = "Number of agricultural businesses",
    fill     = NULL,
    caption  = "Source: ABS Agricultural Commodities, Australia (cat. 7121.0), 2010-11 and 2021-22"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(colour = "grey40", size = 10, margin = margin(b = 10)),
    plot.caption     = element_text(colour = "grey50", size = 8, hjust = 0),
    legend.position  = "top",
    legend.justification = "left",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x      = element_text(size = 10)
  )

p1


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
#### with all farms as a insert
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Main plot data (excluding All farms)
# -----------------------------------------------------------------------------

plot_df <- comparable_farms %>%
  filter(farm_type != "All farms") %>%
  mutate(
    farm_type = factor(farm_type, levels = c(
      "Beef farmers", "Sheep farmers", "Wheat growers",
      "Barley growers", "Canola growers", "Dairy farmers"
    )),
    year = factor(year, levels = c("2010-11", "2021-22"))
  )
# -----------------------------------------------------------------------------
# Inset plot data (All farms only)
# -----------------------------------------------------------------------------
inset_df <- comparable_farms %>%
  filter(farm_type == "All farms") %>%
  mutate(year = factor(year, levels = c("2010-11", "2021-22"))) %>%
  arrange(year)   # ensure rows are in factor order

# -----------------------------------------------------------------------------
# Inset plot
# -----------------------------------------------------------------------------
p_inset <- ggplot(inset_df, aes(x = year, y = n_businesses, fill = year)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = scales::comma(n_businesses)),
    vjust = -0.5, size = 2.8, colour = "grey30"
  ) +
  scale_x_discrete(limits = c("2010-11", "2021-22")) +  # explicitly lock x order
  scale_y_continuous(
    limits = c(0, 155000),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = c("2010-11" = "#378ADD", "2021-22" = "#1D9E75")) +
  labs(title = "All farms", x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title         = element_text(face = "bold", size = 9, hjust = 0.5),
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_blank(),
    axis.text.y        = element_blank(),
    plot.background    = element_rect(fill = "grey96", colour = "grey80", linewidth = 0.4)
  )

# -----------------------------------------------------------------------------
# Combine — note left must be LESS than right
# -----------------------------------------------------------------------------
farm_bus <- p_main + inset_element(p_inset, left = 0.70, bottom = 0.55, right = 0.98, top = 1.0)




###############################################################################
### using farm survey data 

# Pull historical national estimates
hist_nat <- read_historical_national_estimates()
read_estimates_by_size()
# See what's in it
str(hist_nat)
names(hist_nat)
unique(hist_nat$Industry)   # check industry type categories


# Check if there is an Item/Variable column with row-level descriptions
if("Item" %in% names(hist_nat)) {
  grep("farm|number|size|count|area|hectare|ha",
       unique(hist_nat$Item),
       value = TRUE,
       ignore.case = TRUE)
}

# Same for a Variable column
if("Variable" %in% names(hist_nat)) {
  grep("farm|number|size|count|area|hectare|ha",
       unique(hist_nat$Variable),
       value = TRUE,
       ignore.case = TRUE)
}



ls("package:read.abares")

# Filter to Area operated and see what's available
area_operated <- hist_nat %>%
  filter(Variable == "Area operated at 30 June (ha)")   

# Check the structure
str(area_operated)
names(area_operated)

# See what industry categories and years are available
unique(area_operated$Industry)
unique(area_operated$Year)

# Look at the data
area_operated %>%
  select(Year, Industry, Value) %>%
  arrange(Industry, Year) %>%
  View()


area_operated_per_farm <- area_operated %>%
  select(Year, Industry, Value) %>%
  arrange(Industry, Year) %>%
  ggplot(aes(x = Year, y = Value, colour = Industry)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_brewer(palette = "Set2") +
  labs(
    title    = "Average area operated by farm industry type, Australia",
    subtitle = "Area operated at 30 June (ha), by ABARES industry classification",
    x        = NULL,
    y        = "Average area operated (ha)",
    colour   = NULL,
    caption  = "Source: ABARES Historical National Estimates (AAGIS)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title           = element_text(face = "bold", size = 13),
    plot.subtitle        = element_text(colour = "grey40", size = 10, margin = margin(b = 10)),
    plot.caption         = element_text(colour = "grey50", size = 8, hjust = 0),
    legend.position      = "bottom",
    legend.justification = "left",
    panel.grid.minor     = element_blank()
  )
area_operated_per_farm



Total_area_cropped <- hist_nat %>%
  filter(Variable == "Total area cropped (ha)") %>%
  select(Year, Industry, Value) %>%
  arrange(Industry, Year) %>%
  
  ggplot(aes(x = Year, y = Value, colour = Industry)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_brewer(palette = "Set2") +
  labs(
    title    = "Average total area cropped by farm industry type, Australia",
    subtitle = "Total area cropped (ha), by ABARES industry classification",
    x        = NULL,
    y        = "Average total area cropped (ha)",
    colour   = NULL,
    caption  = "Source: ABARES Historical National Estimates (AAGIS)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title           = element_text(face = "bold", size = 13),
    plot.subtitle        = element_text(colour = "grey40", size = 10, margin = margin(b = 10)),
    plot.caption         = element_text(colour = "grey50", size = 8, hjust = 0),
    legend.position      = "bottom",
    legend.justification = "left",
    panel.grid.minor     = element_blank()
  )
Total_area_cropped


Total_area_cropped_cropping <- hist_nat %>%
  filter(Variable == "Total area cropped (ha)",
         Industry == "Cropping") %>%
  select(Year, Industry, Value) %>%
  arrange(Year) %>%
  mutate(Year = as.integer(Year)) %>%
  ggplot(aes(x = Year, y = Value)) +
  geom_line(colour = "#185FA5", linewidth = 0.9) +
  geom_point(colour = "#185FA5", size = 1.8) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  labs(
    title    = "Average area cropped per farm, cropping specialists, Australia",
    #subtitle = "Mean total area cropped (ha) per farm — ABARES cropping industry classification",
    x        = NULL,
    y        = "Average area cropped per farm (ha)",
    caption  = "Source: ABARES Historical National Estimates (AAGIS)\nNote: values are per-farm averages for broadacre cropping specialist farms"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", size = 13),
    plot.subtitle     = element_text(colour = "grey40", size = 10, margin = margin(b = 10)),
    plot.caption      = element_text(colour = "grey50", size = 8, hjust = 0),
    panel.grid.minor  = element_blank()
  )
Total_area_cropped_cropping
max(hist_nat$Year)

#################################################################################

# Save panel A - farm numbers bar chart
farm_bus
ggsave(
  filename = "N:/Advances in Australian Farming Systems Paper/Section 1/Farm size and Consolidation/fig_farm_business_numbers_ABS.png",
  plot     = p_numbers,
  width    = 10,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)


ggsave(
  filename = "N:/Advances in Australian Farming Systems Paper/Section 1/Farm size and Consolidation/fig_farm_business_numbers_ABS.png",
  plot     = farm_bus,
  width    = 10,
  height   = 6,
  dpi      = 300,
  bg       = "white"
)

# Save panel B - average area cropped per farm Total_area_cropped_cropping
Total_area_cropped_cropping
ggsave(
  filename = "N:/Advances in Australian Farming Systems Paper/Section 1/Farm size and Consolidation/Total_area_cropped_cropping_ABARES.png",
  plot     = p_size,
  width    = 8,
  height   = 5,
  dpi      = 300,
  bg       = "white"
)

