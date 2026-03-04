# =============================================================================
# P1D_ingest_gaez.R
# PHASE 1D — Prepare FAO GAEZ v4 agricultural suitability raster
#
# WORKER D runs this (alongside P1D_ingest_travel_time.R — independent).
#
# IMPORTANT: FAO GAEZ v4 requires manual download. Instructions below.
# This script prepares the manually downloaded file (crops and reprojects).
# If the file already exists at data/raw/landuse/gaez_raw/, it proceeds.
#
# Manual download steps:
#   1. Go to https://gaez.fao.org/pages/data-access-download
#   2. Theme: "Suitability and Attainable Yield" (Theme 6)
#   3. Sub-theme: "Suitability class - Rainfed crops"
#   4. Time period: Baseline (1981–2010), Input level: Intermediate
#   5. Download as GeoTIFF
#   6. Save to: data/raw/landuse/gaez_raw/
#
# Fallback: If GAEZ is unavailable, we use ESA WorldCover 10m land cover
# to proxy agricultural land (cropland class = 40).
#
# Outputs:
#   data/raw/landuse/gaez_rainfed_suit.tif
#
# =============================================================================

library(here)
library(terra)
library(sf)

source(here("R/utils/constants.R"))
source(here("R/utils/io_helpers.R"))

message("=== P1D: Preparing agricultural suitability ===")

if (.skip_if_done(PATH_GAEZ)) stop("Skipping.", call. = FALSE)

aoi_ext <- terra::ext(
  AOI_BBOX_DL["xmin"], AOI_BBOX_DL["xmax"],
  AOI_BBOX_DL["ymin"], AOI_BBOX_DL["ymax"]
)

# -----------------------------------------------------------------------------
# Option A: Use manually downloaded GAEZ v4 file
# -----------------------------------------------------------------------------
gaez_raw_dir  <- here("data/raw/landuse/gaez_raw")
gaez_raw_files <- list.files(gaez_raw_dir, pattern = "\\.tif$",
                              full.names = TRUE, recursive = TRUE)

if (length(gaez_raw_files) > 0) {
  message("Found GAEZ raw file(s): ", paste(basename(gaez_raw_files), collapse=", "))

  gaez_raw  <- terra::rast(gaez_raw_files[1])
  gaez_crop <- terra::crop(gaez_raw, aoi_ext)

  # GAEZ suitability classes: 1=Very Suitable, 2=Suitable, 3=Moderately Suitable,
  # 4=Marginally Suitable, 5=Not Suitable, 7=Water, 8=Urban/Built-up, 9=No data
  # Recode to numeric index 0–100 for continuous regression (invert: 1→100, 5→20)
  rcl <- matrix(
    c(1, 100,
      2,  80,
      3,  60,
      4,  40,
      5,  20,
      7,  NA,
      8,  NA,
      9,  NA),
    ncol = 2, byrow = TRUE
  )
  gaez_suit <- terra::classify(gaez_crop, rcl, others = NA)
  names(gaez_suit) <- "agri_suitability_0_100"

  terra::writeRaster(gaez_suit, here(PATH_GAEZ), overwrite = TRUE,
                     gdal = c("COMPRESS=LZW"))

  message("GAEZ suitability raster saved: ", PATH_GAEZ)

} else {
  # -------------------------------------------------------------------------
  # Option B: ESA WorldCover fallback (cropland share as suitability proxy)
  # -------------------------------------------------------------------------
  message(
    "[P1D] No GAEZ file found in ", gaez_raw_dir,
    "\nFalling back to ESA WorldCover cropland fraction proxy.",
    "\n  Download GAEZ manually from https://gaez.fao.org for better results."
  )

  # ESA WorldCover v200 (2021) can be downloaded tile by tile.
  # Uganda tile: N00E033, N00E034, N01E033, N01E034
  # Download from: https://worldcover2021.esa.int/download
  # Save tiles to: data/raw/landuse/worldcover/
  worldcover_dir  <- here("data/raw/landuse/worldcover")
  wc_files        <- list.files(worldcover_dir, pattern = "\\.tif$",
                                full.names = TRUE, recursive = TRUE)

  if (length(wc_files) > 0) {
    message("Using ESA WorldCover tiles: ", paste(basename(wc_files), collapse=", "))
    wc_mosaic <- if (length(wc_files) == 1) terra::rast(wc_files[1]) else
      terra::mosaic(terra::sprc(lapply(wc_files, terra::rast)))

    wc_crop   <- terra::crop(wc_mosaic, aoi_ext)

    # Class 40 = Cropland in ESA WorldCover
    # Aggregate to ~90m to match other rasters and compute cropland fraction
    wc_crop_agg   <- terra::aggregate(wc_crop, fact = 9, fun = "modal")
    cropland_bin  <- terra::ifel(wc_crop_agg == 40, 1, 0)
    names(cropland_bin) <- "agri_suitability_0_100"
    cropland_score <- cropland_bin * 100  # binary: 0 or 100

    terra::writeRaster(cropland_score, here(PATH_GAEZ), overwrite = TRUE,
                       gdal = c("COMPRESS=LZW"))
    message("ESA WorldCover cropland proxy saved: ", PATH_GAEZ)

  } else {
    stop(
      "[P1D] Neither GAEZ nor ESA WorldCover files found.\n",
      "Please download one of:\n",
      "  GAEZ: https://gaez.fao.org/pages/data-access-download\n",
      "  ESA WorldCover: https://worldcover2021.esa.int/download\n",
      "See script comments for instructions.",
      call. = FALSE
    )
  }
}

message("\n=== P1D: Agricultural suitability complete ===\n")
