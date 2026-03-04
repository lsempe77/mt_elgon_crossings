# =============================================================================
# P2_generate_crossings.R
# PHASE 2 — Generate the master river crossing point dataset
#
# BLOCKING: All Phase 3 scripts wait for this output.
# One person runs this after all Phase 1 scripts are complete.
#
# Requires:
#   data/raw/dem/dem_mt_elgon.tif          (from P1A)
#   data/raw/rivers/rivers_osm.gpkg        (from P1A)
#   data/raw/roads/paths_osm.gpkg          (from P1B)
#
# Outputs:
#   data/processed/hydro/dem_filled.tif
#   data/processed/hydro/flow_dir.tif
#   data/processed/hydro/flow_acc.tif
#   data/processed/hydro/streams_derived.tif
#   data/processed/crossings/crossings.gpkg   ← MASTER SPINE
#
# Output contract (crossings.gpkg, layer "crossings"):
#   crossing_id     character  unique, format XNG-00001
#   river_osm_id    character  OSM way ID of river
#   road_osm_id     character  OSM way ID of road/path
#   road_type       character  highway tag value
#   is_pedestrian   logical    TRUE if path/footway/track
#   elev_m          numeric    elevation from DEM
#   geometry        POINT      CRS = EPSG:32636
#
# Runtime: ~10–20 minutes (DEM hydrological conditioning is the slow step).
# =============================================================================

library(here)
library(sf)
library(terra)
library(dplyr)
library(whitebox)

source(here("R/utils/constants.R"))
source(here("R/utils/io_helpers.R"))
source(here("R/utils/spatial_helpers.R"))

message("=== P2: Generating river crossing points ===")

# Check all inputs present
.check_input(PATH_DEM)
.check_input(PATH_RIVERS_OSM)
.check_input(PATH_ROADS_OSM)

# Check WhiteboxTools is available
if (!whitebox::wbt_init()) {
  message("[P2] WhiteboxTools not found. Installing...")
  whitebox::install_whitebox()
  whitebox::wbt_init()
}

# -----------------------------------------------------------------------------
# STEP 1 — Hydrological conditioning of DEM
# Fills sinks, computes flow direction and accumulation.
# This is needed for stream order and catchment area extraction in Phase 3A.
# -----------------------------------------------------------------------------
message("\n[Step 1/4] Conditioning DEM (filling sinks)...")

wbt_fill_depressions(
  dem    = here(PATH_DEM),
  output = here(PATH_DEM_FILLED)
)

message("[Step 1/4] Computing D8 flow direction...")
wbt_d8_pointer(
  dem    = here(PATH_DEM_FILLED),
  output = here(PATH_FLOW_DIR)
)

message("[Step 1/4] Computing flow accumulation...")
wbt_d8_flow_accumulation(
  input  = here(PATH_DEM_FILLED),
  output = here(PATH_FLOW_ACC),
  out_type = "cells"
)

message("[Step 1/4] Extracting stream network (threshold = ",
        FLOW_ACC_THRESHOLD, " cells)...")
wbt_extract_streams(
  flow_accum = here(PATH_FLOW_ACC),
  output     = here(PATH_STREAMS),
  threshold  = FLOW_ACC_THRESHOLD
)

# -----------------------------------------------------------------------------
# STEP 2 — Load OSM layers and reproject to CRS_PROJ
# -----------------------------------------------------------------------------
message("\n[Step 2/4] Loading OSM layers...")

rivers <- sf::st_read(here(PATH_RIVERS_OSM), layer = "rivers", quiet = TRUE) |>
  sf::st_transform(CRS_PROJ) |>
  sf::st_cast("LINESTRING")   # ensure no MULTILINESTRING

roads  <- sf::st_read(here(PATH_ROADS_OSM), layer = "roads", quiet = TRUE) |>
  sf::st_transform(CRS_PROJ) |>
  sf::st_cast("LINESTRING")

message("  Rivers: ", nrow(rivers), " features")
message("  Roads : ", nrow(roads),  " features")

# -----------------------------------------------------------------------------
# STEP 3 — Find river × road intersections
# Each intersection point = one candidate crossing.
# -----------------------------------------------------------------------------
message("\n[Step 3/4] Finding river × road intersections...")

# Snap tolerance: points within 5m of each other are considered the same crossing
SNAP_TOLERANCE_M <- 5

# Add row indices for attribute join after intersection
rivers <- rivers |> dplyr::mutate(.river_row = dplyr::row_number())
roads  <- roads  |> dplyr::mutate(.road_row  = dplyr::row_number())

# Spatial intersection — this is the slow step
crossings_raw <- suppressWarnings(
  sf::st_intersection(rivers, roads)
) |>
  sf::st_cast("POINT")

# Remove empty geometries using subsetting (avoids geometry column name issues)
crossings_raw <- crossings_raw[!sf::st_is_empty(crossings_raw), ]

if (nrow(crossings_raw) == 0) {
  stop("[P2] No intersections found between rivers and roads. ",
       "Check that both layers are in CRS_PROJ and cover the same area.",
       call. = FALSE)
}

message("  Raw intersections: ", nrow(crossings_raw))

# -----------------------------------------------------------------------------
# STEP 4 — Clean and attribute crossings
# -----------------------------------------------------------------------------
message("\n[Step 4/4] Cleaning and attributing crossings...")

crossings <- crossings_raw |>
  dplyr::transmute(
    crossing_id   = CROSSING_ID_FORMAT(dplyr::row_number()),
    river_osm_id  = as.character(osm_id),
    river_name    = river_name,
    road_osm_id   = as.character(osm_id.1),
    road_type     = road_type,
    is_pedestrian = is_pedestrian
  )

# Attach elevation from DEM
dem <- terra::rast(here(PATH_DEM))
dem_proj <- terra::project(dem, CRS_PROJ)
crossings$elev_m <- terra::extract(
  dem_proj,
  terra::vect(crossings)
)[, 2]

# Remove duplicate points within snap tolerance (same physical crossing
# represented by multiple OSM line segments)
crossings_dedup <- crossings |>
  dplyr::group_by(crossing_id) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

# Final row count
n_total       <- nrow(crossings_dedup)
n_pedestrian  <- sum(crossings_dedup$is_pedestrian, na.rm = TRUE)

# Write master spine
write_crossings(crossings_dedup, script = "P2_generate_crossings.R")

# Also write as CSV for quick inspection (no geometry)
readr::write_csv(
  sf::st_drop_geometry(crossings_dedup),
  here("data/processed/crossings/crossings_attributes.csv")
)

# -----------------------------------------------------------------------------
# Summary and visual check
# -----------------------------------------------------------------------------
message(
  "\nCrossing generation complete:",
  "\n  Total crossings       : ", n_total,
  "\n  Pedestrian crossings  : ", n_pedestrian,
  "\n  Road crossings        : ", n_total - n_pedestrian,
  "\n  Elevation range       : ",
  round(min(crossings_dedup$elev_m, na.rm=TRUE), 0), " – ",
  round(max(crossings_dedup$elev_m, na.rm=TRUE), 0), " m",
  "\n  Output                : ", PATH_CROSSINGS
)

# Quick map
aoi_sf <- sf::st_read(here(PATH_AOI_GPKG), layer = "aoi", quiet = TRUE) |>
  sf::st_transform(CRS_PROJ)

plot(sf::st_geometry(aoi_sf),
     main = paste0("Mt. Elgon river crossings (n=", n_total, ")"),
     col = NA, border = "grey40")
plot(sf::st_geometry(
  rivers[sf::st_intersects(rivers, aoi_sf, sparse = FALSE)[,1], ]
     ), col = "steelblue", add = TRUE, lwd = 0.5)
plot(sf::st_geometry(crossings_dedup),
     col   = ifelse(crossings_dedup$is_pedestrian, "orange", "darkgreen"),
     pch   = 20, cex = 0.8, add = TRUE)
legend("bottomright",
       legend = c("Pedestrian crossing", "Road crossing"),
       col    = c("orange", "darkgreen"), pch = 20, cex = 0.8)

message("\n=== P2 complete. Phase 3A and 3B can now run in parallel. ===\n")
