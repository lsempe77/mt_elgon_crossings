# =============================================================================
# P3A_hydro_covariates.R
# PHASE 3A — Extract hydrological difficulty proxies at each crossing
#
# WORKER A runs this after P2 is complete.
# Run in parallel with P3B_socioec_outcomes.R.
#
# Requires:
#   data/processed/crossings/crossings.gpkg    (from P2 — master spine)
#   data/raw/dem/dem_mt_elgon.tif              (from P1A)
#   data/processed/hydro/dem_filled.tif        (from P2)
#   data/processed/hydro/flow_dir.tif          (from P2)
#   data/processed/hydro/flow_acc.tif          (from P2)
#   data/processed/hydro/streams_derived.tif   (from P2)
#
# Outputs:
#   outputs/covariates/hydro_covariates.csv
#
# Output columns (see io_helpers.R::HYDRO_REQUIRED_COLS for contract):
#   crossing_id, slope_deg, catchment_area_km2,
#   stream_order, flood_Q10, flood_Q50
#   plus: slope_pct, flow_acc_cells, mean_precip_mm
#
# Runtime: ~15–30 minutes (watershed delineation is slow for many points).
# =============================================================================

library(here)
library(sf)
library(terra)
library(dplyr)
library(whitebox)

source(here("R/utils/constants.R"))
source(here("R/utils/io_helpers.R"))
source(here("R/utils/spatial_helpers.R"))

message("=== P3A: Extracting hydrological covariates ===")

.check_input(PATH_CROSSINGS)
.check_input(PATH_DEM)
.check_input(PATH_DEM_FILLED)
.check_input(PATH_FLOW_DIR)
.check_input(PATH_FLOW_ACC)
.check_input(PATH_STREAMS)

if (!whitebox::wbt_init()) whitebox::install_whitebox()

# -----------------------------------------------------------------------------
# Load master spine
# -----------------------------------------------------------------------------
crossings <- read_crossings() |>
  sf::st_transform(CRS_PROJ)

n <- nrow(crossings)
message("Processing ", n, " crossings...")

# Load rasters once (avoid repeated I/O)
dem_proj   <- terra::project(terra::rast(here(PATH_DEM)), CRS_PROJ)
flow_acc_r <- terra::rast(here(PATH_FLOW_ACC))
strahler_r <- NULL   # computed below

# Buffer for raster extraction: OSM crossing points can be offset from DEM
# stream cells by up to 5–10 cells (DEM res ~19–30 m → use 500 m buffer).
# Taking the MAX within the buffer captures the main stream channel even for
# upland crossings where the DEM stream network is displaced from the OSM line.
SNAP_BUF_M <- 500
crossings_buf <- sf::st_buffer(crossings, dist = SNAP_BUF_M)

# -----------------------------------------------------------------------------
# STEP 1 — Slope at crossing point
# Use mean within 100 m buffer (smooths road cut artefacts in DEM)
# -----------------------------------------------------------------------------
message("\n[Step 1/5] Computing slope at crossings...")

slope_r     <- terra::terrain(dem_proj, "slope", unit = "degrees")
slope_rad_r <- terra::terrain(dem_proj, "slope", unit = "radians")
slope_pct_r <- tan(slope_rad_r) * 100   # terra 1.8 lacks "tangent" unit

slope_buf <- sf::st_buffer(crossings, dist = 100)
crossings$slope_deg <- terra::extract(slope_r, terra::vect(slope_buf),
                                       fun = "mean", na.rm = TRUE)[, 2]
crossings$slope_pct <- terra::extract(slope_pct_r, terra::vect(slope_buf),
                                       fun = "mean", na.rm = TRUE)[, 2]

# -----------------------------------------------------------------------------
# STEP 2 — Flow accumulation → catchment area
# Use MAX within 300 m buffer to snap to the nearest stream cell
# -----------------------------------------------------------------------------
message("[Step 2/5] Extracting flow accumulation → catchment area...")

# Flow accumulation raster is in units of upstream cells.
# Convert: catchment_area_km2 = n_cells × cell_area_m2 / 1e6
flow_acc_proj <- terra::project(flow_acc_r, CRS_PROJ)
cell_area_m2  <- prod(terra::res(flow_acc_proj))

crossings$flow_acc_cells <- terra::extract(flow_acc_proj,
                                            terra::vect(crossings_buf),
                                            fun = "max", na.rm = TRUE)[, 2]
crossings$catchment_area_km2 <- (crossings$flow_acc_cells * cell_area_m2) / 1e6

# -----------------------------------------------------------------------------
# STEP 3 — Strahler stream order
# Use MAX within 500 m buffer for the same snapping reason
# -----------------------------------------------------------------------------
message("[Step 3/5] Computing Strahler stream order...")

strahler_path <- here(PATH_STRAHLER)

whitebox::wbt_strahler_stream_order(
  d8_pntr = here(PATH_FLOW_DIR),
  streams = here(PATH_STREAMS),
  output  = strahler_path
)

strahler_r <- terra::project(terra::rast(strahler_path), CRS_PROJ)
crossings$stream_order <- as.integer(
  terra::extract(strahler_r, terra::vect(crossings_buf),
                 fun = "max", na.rm = TRUE)[, 2]
)

# -----------------------------------------------------------------------------
# STEP 4 — Mean annual precipitation (for flood frequency estimation)
# Uses WorldClim 2.1 (1970–2000 average) as climate baseline.
# Falls back to a uniform 1200 mm/yr estimate if download fails.
# -----------------------------------------------------------------------------
message("[Step 4/5] Extracting mean annual precipitation...")

precip_path <- here("data/raw/landuse/chirps_mean_annual_precip.tif")

if (!file.exists(precip_path)) {
  message("  Downloading WorldClim precipitation (Uganda tiles)...")
  prec_tiles <- tryCatch({
    tiles <- lapply(c(33, 34, 35), function(lon) {
      geodata::worldclim_tile(var = "prec", lon = lon, lat = 1,
                              res = "0.5", path = here("data/raw/landuse/"))
    })
    terra::mosaic(terra::sprc(Filter(Negate(is.null), tiles)))
  }, error = function(e) {
    warning("[P3A] WorldClim download failed: ", conditionMessage(e),
            "\nUsing uniform 1200 mm/yr fallback.")
    NULL
  })

  if (!is.null(prec_tiles)) {
    # WorldClim prec is monthly (12 layers); sum to annual
    prec_annual <- sum(prec_tiles)
    prec_crop   <- terra::crop(prec_annual,
                               terra::ext(AOI_BBOX_DL["xmin"], AOI_BBOX_DL["xmax"],
                                          AOI_BBOX_DL["ymin"], AOI_BBOX_DL["ymax"]))
    terra::writeRaster(prec_crop, precip_path, overwrite=TRUE,
                       gdal="COMPRESS=LZW")
  }
}

if (file.exists(precip_path)) {
  prec_proj <- terra::project(terra::rast(precip_path), CRS_PROJ)
  crossings$mean_precip_mm <- terra::extract(prec_proj,
                                              terra::vect(crossings))[, 2]
} else {
  message("  Using uniform precip fallback: 1200 mm/yr")
  crossings$mean_precip_mm <- 1200
}

# Replace any remaining NAs in precip with median
median_prec <- median(crossings$mean_precip_mm, na.rm = TRUE)
crossings$mean_precip_mm[is.na(crossings$mean_precip_mm)] <- median_prec

# -----------------------------------------------------------------------------
# STEP 5 — Flood frequency (regional regression, Uganda)
#
# Uses the Flood Estimation Handbook Sub-Saharan Africa (FEHSSA) approach:
#   Q_T = C * A^alpha * R^beta * T^gamma
# Parameters below are indicative for Uganda highlands. Calibrate against
# gauged catchments before finalising (see docs/methodology.md).
#
# A = catchment area (km²)
# R = mean annual precipitation (mm)
# T = return period (years)
# -----------------------------------------------------------------------------
message("[Step 5/5] Estimating flood frequency (Q10, Q50)...")

# Indicative FEHSSA parameters for Uganda (adjust with literature)
C     <- 0.015
alpha <- 0.73
beta  <- 0.85
gamma <- 0.35

crossings <- crossings |>
  dplyr::mutate(
    catchment_area_km2 = dplyr::if_else(catchment_area_km2 <= 0 | is.na(catchment_area_km2),
                                        0.01, catchment_area_km2),
    flood_Q10 = C * catchment_area_km2^alpha * mean_precip_mm^beta * 10^gamma,
    flood_Q50 = C * catchment_area_km2^alpha * mean_precip_mm^beta * 50^gamma
  )

# -----------------------------------------------------------------------------
# Write output
# -----------------------------------------------------------------------------
hydro_out <- sf::st_drop_geometry(crossings) |>
  dplyr::select(
    crossing_id,
    slope_deg,
    slope_pct,
    catchment_area_km2,
    flow_acc_cells,
    stream_order,
    mean_precip_mm,
    flood_Q10,
    flood_Q50
  )

write_hydro_covariates(hydro_out, script = "P3A_hydro_covariates.R")

# -----------------------------------------------------------------------------
# Summary statistics
# -----------------------------------------------------------------------------
message(
  "\nHydro covariates summary:",
  "\n  slope_deg          : ", round(mean(hydro_out$slope_deg, na.rm=T),1),
  " ± ", round(sd(hydro_out$slope_deg, na.rm=T),1), " (mean ± SD)",
  "\n  catchment_area_km2 : ", round(median(hydro_out$catchment_area_km2, na.rm=T),1),
  " (median), range ",
  round(min(hydro_out$catchment_area_km2, na.rm=T),1), "–",
  round(max(hydro_out$catchment_area_km2, na.rm=T),0),
  "\n  stream_order       : ", paste(sort(unique(hydro_out$stream_order)), collapse=", "),
  "\n  flood_Q50 (m³/s)   : ", round(median(hydro_out$flood_Q50, na.rm=T), 1),
  " (median)",
  "\n  NAs in slope       : ", sum(is.na(hydro_out$slope_deg)),
  "\n  Output             : ", PATH_HYDRO_COV
)

message("\n=== P3A complete ===\n")
