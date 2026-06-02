# ============================================================
# EXPORT RASTERS FOR ESRI/ARCGIS
# Run after main land use change script
# ============================================================

# ============================================================
# 1. CHECK CRS
# ============================================================
message("CRS for changed raster:")
print(crs(changed))

message("CRS for transition raster:")
print(crs(transition_changed))

# ============================================================
# 2. SET OUTPUT DIRECTORY
# ============================================================
out_dir <- "N:/Advances in Australian Farming Systems Paper/Section 1/landuse maps/change_outputs/"
dir.create(out_dir, showWarnings = FALSE)

# ============================================================
# 3. WRITE RASTERS WITH CORRECT DATA TYPES
# ============================================================

# Binary changed/unchanged (0 = no change, 1 = changed)
writeRaster(changed,
            paste0(out_dir, "AGIND_changed_binary.tif"),
            datatype  = "INT1U",
            overwrite = TRUE)

# Transition codes (two-digit: from-class * 10 + to-class, e.g. 13 = Grazing native -> Cropping)
writeRaster(transition_changed,
            paste0(out_dir, "AGIND_transition_codes.tif"),
            datatype  = "INT2U",
            overwrite = TRUE)

# AGIND-coded rasters for 2011 and 2021 (1-5, one value per class)
writeRaster(ag_2011,
            paste0(out_dir, "AGIND_coded_2011.tif"),
            datatype  = "INT1U",
            overwrite = TRUE)

writeRaster(ag_2021,
            paste0(out_dir, "AGIND_coded_2021.tif"),
            datatype  = "INT1U",
            overwrite = TRUE)

# ============================================================
# 4. WRITE LEGEND / SIDECAR CSVs FOR ARCGIS JOIN
# ============================================================

# Binary raster legend
write.csv(
  data.frame(Value = c(0, 1), Label = c("No change", "Changed")),
  paste0(out_dir, "AGIND_changed_binary_legend.csv"),
  row.names = FALSE
)

# Transition raster legend — join to raster in ArcGIS on Value field
transition_legend <- transition_labels %>%
  select(Value = trans_code, from_class, to_class, label) %>%
  filter(Value %in% unique(values(transition_changed, na.rm = TRUE)))

write.csv(transition_legend,
          paste0(out_dir, "AGIND_transition_codes_legend.csv"),
          row.names = FALSE)

# AGIND coded raster legend (applies to both 2011 and 2021)
agind_export_legend <- agind_code %>%
  left_join(agind_lookup, by = "AGIND") %>%
  rename(Value = agind_int, Class = AGIND, Hex_colour = hex)

write.csv(agind_export_legend,
          paste0(out_dir, "AGIND_coded_legend.csv"),
          row.names = FALSE)

# ============================================================
# 5. CONFIRM FILES WRITTEN
# ============================================================
message("Files written to: ", out_dir)
list.files(out_dir)



# Force integer and rebuild for ArcGIS compatibility
transition_int <- as.int(transition_changed)

writeRaster(transition_int,
            paste0(out_dir, "AGIND_transition_codes_int.tif"),
            datatype  = "INT2U",
            overwrite = TRUE)


# ============================================================
# CREATE 3-CLASS TRANSITION TYPE RASTER FOR ARCMAP
# ============================================================

# Non-ag AGIND integer code — anything NOT in agind_code is non-ag
# In our coding: 1-5 = ag classes, NA = non-ag
# So we need to track where original rasters had NA (non-ag)

# Create non-ag masks for each year (TRUE where pixel was non-ag)
nonag_2011 <- is.na(ag_2011)
nonag_2021 <- is.na(ag_2021)

# ag in both years
ag_both <- (!nonag_2011) & (!nonag_2021)

# Classify into 3 transition types:
# 1 = Non-ag -> Ag  (was NA in 2011, has value in 2021)
# 2 = Ag -> Non-ag  (had value in 2011, NA in 2021)
# 3 = Ag -> Ag different class (ag in both years but class changed)

trans_type <- ifel(nonag_2011 & !nonag_2021,  1L,       # Non-ag -> Ag
                   ifel(!nonag_2011 & nonag_2021,    2L,       # Ag -> Non-ag
                        ifel(ag_both & (ag_2011 != ag_2021), 3L,   # Ag -> Ag different
                             NA_integer_)))                               # no change or no data

# ============================================================
# WRITE RASTER
# ============================================================
writeRaster(trans_type,
            paste0(out_dir, "AGIND_transition_type.tif"),
            datatype  = "INT1U",
            overwrite = TRUE)

# ============================================================
# WRITE LEGEND CSV
# ============================================================
write.csv(
  data.frame(
    Value  = c(1, 2, 3),
    Label  = c("Non-ag to Ag", "Ag to Non-ag", "Ag to Ag (different class)"),
    Colour = c("#2ECC71",      "#E74C3C",       "#3498DB")
  ),
  paste0(out_dir, "AGIND_transition_type_legend.csv"),
  row.names = FALSE
)

message("Done — load AGIND_transition_type.tif into ArcMap")
message("Values: 1 = Non-ag to Ag, 2 = Ag to Non-ag, 3 = Ag to Ag different class")
