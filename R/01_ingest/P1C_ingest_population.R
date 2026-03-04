# =============================================================================
# P1C_ingest_population.R
# PHASE 1C — Download WorldPop 2015 population density for Mt. Elgon
#
# WORKER C runs this (alongside P1C_ingest_ntl.R — independent).
# Requires: outputs/aoi/mt_elgon_bbox.gpkg  (from P0)
#
# Outputs:
#   data/raw/population/worldpop_mt_elgon.tif   (persons per pixel, WGS84)
#
# Runtime: ~2–5 minutes.
# =============================================================================

library(here)
library(geodata)
library(terra)
library(sf)

source(here("R/utils/constants.R"))
source(here("R/utils/io_helpers.R"))

message("=== P1C: Downloading WorldPop population ===")

if (.skip_if_done(PATH_POP)) stop("Skipping.", call. = FALSE)

# -----------------------------------------------------------------------------
# 1. Download Uganda 2015 WorldPop 100m (geodata wraps WorldPop REST API)
# -----------------------------------------------------------------------------
message("Downloading WorldPop 2015 for Uganda...")

pop_dl_dir <- here("data/raw/population/tiles")
dir.create(pop_dl_dir, showWarnings = FALSE, recursive = TRUE)

pop_raw <- tryCatch(
  geodata::population(year = 2015, res = 0.5, path = pop_dl_dir),
  error = function(e) {
    stop("[P1C pop] geodata::population() failed: ", conditionMessage(e),
         call. = FALSE)
  }
)

# geodata::population() returns global raster; crop to download AOI
aoi_ext <- terra::ext(
  AOI_BBOX_DL["xmin"], AOI_BBOX_DL["xmax"],
  AOI_BBOX_DL["ymin"], AOI_BBOX_DL["ymax"]
)
pop_crop <- terra::crop(pop_raw, aoi_ext)
names(pop_crop) <- "pop_density_2015"

# -----------------------------------------------------------------------------
# 2. Write output
# -----------------------------------------------------------------------------
terra::writeRaster(pop_crop, here(PATH_POP), overwrite = TRUE,
                   gdal = c("COMPRESS=LZW"))

total_pop <- round(terra::global(pop_crop, "sum", na.rm = TRUE)[[1]], 0)

message(
  "\nPopulation raster saved:",
  "\n  Total pop in AOI : ~", format(total_pop, big.mark=","),
  "\n  Resolution       : ", paste(round(terra::res(pop_crop) * 111000, 0), collapse=" x "), " m (approx)",
  "\n  Output           : ", PATH_POP
)

message("\n=== P1C: Population complete ===\n")
