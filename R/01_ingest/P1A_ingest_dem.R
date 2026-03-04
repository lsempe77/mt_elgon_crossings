# =============================================================================
# P1A_ingest_dem.R
# PHASE 1A — Download ~30 m elevation tiles for Mt. Elgon (AWS/Mapzen)
#
# WORKER A runs this.
# Requires: outputs/aoi/mt_elgon_bbox.gpkg  (from P0)
#           Internet connection
#           R package: elevatr
#
# Outputs:
#   data/raw/dem/dem_mt_elgon.tif   (cropped DEM ~30 m, WGS84)
#
# Source: AWS Open Terrain / Mapzen (resampled from SRTM 1-arc-sec + other
#         sources). Zoom level z=12 → ~38 m native resolution, cropped to AOI.
#         Substantially better stream-order delineation than SRTM 3-arc-sec
#         without the memory/runtime cost of z=13 (~10m, 500 MB+).
#
# Runtime: ~5–10 minutes depending on connection speed.
# =============================================================================

library(here)
library(elevatr)
library(terra)
library(sf)

source(here("R/utils/constants.R"))
source(here("R/utils/io_helpers.R"))

message("=== P1A: Downloading DEM (30 m, elevatr/AWS) ===")

if (.skip_if_done(PATH_DEM)) stop("Skipping — set FORCE_RERUN <- TRUE to re-run.",
                                   call. = FALSE)

dir.create(here("data/raw/dem"), showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. Build AOI polygon and download tiles at z = 13 (~19 m at equator)
#    elevatr::get_elev_raster() fetches Mapzen terrain tiles and mosaics
#    them automatically; output CRS matches input locations (WGS84 here).
# -----------------------------------------------------------------------------
xmin <- as.numeric(AOI_BBOX_DL["xmin"])
xmax <- as.numeric(AOI_BBOX_DL["xmax"])
ymin <- as.numeric(AOI_BBOX_DL["ymin"])
ymax <- as.numeric(AOI_BBOX_DL["ymax"])

aoi_bbox_sf <- sf::st_sf(
  geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(xmin, ymin,
        xmax, ymin,
        xmax, ymax,
        xmin, ymax,
        xmin, ymin),
      ncol = 2, byrow = TRUE
    ))),
    crs = 4326
  )
)

message("Downloading elevation tiles (z = 12, ~38 m, AWS)…")
message("  AOI: lon ", AOI_BBOX_DL["xmin"], "–", AOI_BBOX_DL["xmax"],
        "  lat ", AOI_BBOX_DL["ymin"], "–", AOI_BBOX_DL["ymax"])

dem_raw <- tryCatch(
  elevatr::get_elev_raster(
    locations  = aoi_bbox_sf,
    z          = 12,
    src        = "aws",
    clip       = "bbox",
    neg_to_na  = FALSE
  ),
  error = function(e) stop("[P1A] elevatr download failed: ",
                            conditionMessage(e), call. = FALSE)
)

# elevatr returns a RasterLayer (raster pkg) — convert to terra SpatRaster
dem_crop <- terra::rast(dem_raw)

# -----------------------------------------------------------------------------
# 2. Assign CRS and layer name
# elevatr returns WGS84 but the proj4 string may not carry an EPSG code that
# terra's describe() can identify. Force WGS84 (EPSG:4326) explicitly.
# -----------------------------------------------------------------------------
terra::crs(dem_crop) <- CRS_GEO
names(dem_crop) <- "elevation_m"

# -----------------------------------------------------------------------------
# 3. Write output
# -----------------------------------------------------------------------------
terra::writeRaster(dem_crop, here(PATH_DEM), overwrite = TRUE,
                   gdal = c("COMPRESS=LZW"))

message(
  "\nDEM saved:",
  "\n  Resolution : ", paste(round(terra::res(dem_crop) * 111000, 0), collapse = " x "), " m (approx)",
  "\n  Dimensions : ", nrow(dem_crop), " rows × ", ncol(dem_crop), " cols",
  "\n  Extent     : ", paste(round(as.vector(terra::ext(dem_crop)), 3), collapse = ", "),
  "\n  CRS        : ", terra::crs(dem_crop, describe = TRUE)$code,
  "\n  Output     : ", PATH_DEM
)

message("\n=== P1A: DEM complete ===\n")
