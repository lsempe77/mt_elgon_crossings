# =============================================================================
# P1D_ingest_travel_time.R
# PHASE 1D — Download Weiss et al. 2020 travel time to cities
#
# WORKER D runs this.
# Requires: outputs/aoi/mt_elgon_bbox.gpkg  (from P0)
#
# Source: Weiss et al. (2020) Nature. "Global maps of travel time to
#         healthcare facilities." 1 km resolution, ~2015 vintage.
#         Distributed via the Malaria Atlas Project.
#
# Outputs:
#   data/raw/travel_time/accessibility_2015.tif  (minutes to city ≥50k pop)
#
# Runtime: ~3–10 minutes.
# =============================================================================

library(here)
library(malariaAtlas)
library(terra)
library(sf)

source(here("R/utils/constants.R"))
source(here("R/utils/io_helpers.R"))

message("=== P1D: Downloading travel time to cities ===")

if (.skip_if_done(PATH_TRAVEL_TIME)) stop("Skipping.", call. = FALSE)

# -----------------------------------------------------------------------------
# 1. Load AOI for spatial filter
# -----------------------------------------------------------------------------
aoi_sf <- sf::st_read(here(PATH_AOI_GPKG), layer = "aoi_download", quiet = TRUE)

# -----------------------------------------------------------------------------
# 2. Download via malariaAtlas
#    Surface name: "A global map of travel time to cities to assess inequalities
#    in accessibility in 2015" (Weiss et al. 2020)
# -----------------------------------------------------------------------------
message("Querying Malaria Atlas Project raster catalogue...")

# List available rasters to confirm surface name
available <- tryCatch(
  malariaAtlas::listRaster(),
  error = function(e) {
    warning("[P1D] Could not list rasters: ", conditionMessage(e))
    NULL
  }
)

# Try to find the correct surface name
if (!is.null(available)) {
  tt_surfaces <- available[grepl("travel|access|walking", available$Title,
                                 ignore.case = TRUE), ]
  if (nrow(tt_surfaces) > 0) {
    message("Available travel time surfaces:")
    print(tt_surfaces[, c("Title", "Abstract")])
  }
}

message("Downloading travel time raster...")

tt_rast <- tryCatch(
  malariaAtlas::getRaster(
    surface = "A global map of travel time to cities to assess inequalities in accessibility in 2015",
    shp     = aoi_sf
  ),
  error = function(e) {
    # Fallback: try geodata::access() which wraps the same dataset
    message("[P1D] malariaAtlas failed, trying geodata::access() fallback...")
    tryCatch(
      {
        acc <- geodata::access(res = 1, path = here("data/raw/travel_time/"))
        aoi_ext <- terra::ext(AOI_BBOX_DL["xmin"], AOI_BBOX_DL["xmax"],
                              AOI_BBOX_DL["ymin"], AOI_BBOX_DL["ymax"])
        terra::crop(acc, aoi_ext)
      },
      error = function(e2) {
        stop("[P1D] Both malariaAtlas and geodata::access() failed.\n",
             "Error 1: ", conditionMessage(e), "\n",
             "Error 2: ", conditionMessage(e2), call. = FALSE)
      }
    )
  }
)

# Convert to terra SpatRaster if needed
if (inherits(tt_rast, "RasterLayer") || inherits(tt_rast, "RasterStack")) {
  tt_rast <- terra::rast(tt_rast)
}

names(tt_rast) <- "travel_time_min_to_city"

# -----------------------------------------------------------------------------
# 3. Write output
# -----------------------------------------------------------------------------
terra::writeRaster(tt_rast, here(PATH_TRAVEL_TIME), overwrite = TRUE,
                   gdal = c("COMPRESS=LZW"))

message(
  "\nTravel time raster saved:",
  "\n  Min / Median / Max : ",
  round(terra::global(tt_rast, "min",  na.rm=TRUE)[[1]], 0), " / ",
  round(terra::global(tt_rast, "mean", na.rm=TRUE)[[1]], 0), " / ",
  round(terra::global(tt_rast, "max",  na.rm=TRUE)[[1]], 0), " minutes",
  "\n  Resolution         : ", paste(round(terra::res(tt_rast) * 111000, 0),
                                     collapse=" x "), " m (approx)",
  "\n  Output             : ", PATH_TRAVEL_TIME
)

message("\n=== P1D: Travel time complete ===\n")
