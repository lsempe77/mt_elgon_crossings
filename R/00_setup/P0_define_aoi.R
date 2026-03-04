# =============================================================================
# P0_define_aoi.R
# PHASE 0 — Study Area Definition
#
# Run this FIRST. Outputs the AOI polygon used by all downstream scripts.
# Takes ~5 seconds.
#
# Outputs:
#   outputs/aoi/mt_elgon_bbox.gpkg  (two layers: 'aoi' and 'aoi_download')
#   outputs/aoi/mt_elgon_bbox.rds
# =============================================================================

library(here)
library(sf)
library(terra)

source(here("R/utils/constants.R"))
source(here("R/utils/spatial_helpers.R"))
source(here("R/utils/io_helpers.R"))

message("=== P0: Defining Area of Interest ===")

# -----------------------------------------------------------------------------
# 1. Core AOI polygon (WGS84)
# -----------------------------------------------------------------------------
aoi_wgs84 <- bbox_to_sf(AOI_BBOX, crs = CRS_GEO)

# -----------------------------------------------------------------------------
# 2. Download AOI — padded by BUFFER_DL_M for edge-effect prevention
# -----------------------------------------------------------------------------
aoi_proj        <- sf::st_transform(aoi_wgs84, CRS_PROJ)
aoi_dl_proj     <- sf::st_buffer(aoi_proj, dist = BUFFER_DL_M)
aoi_dl_wgs84    <- sf::st_transform(aoi_dl_proj, CRS_GEO)
aoi_dl_wgs84$id <- "aoi_download"

# -----------------------------------------------------------------------------
# 3. Write both layers to GeoPackage
# -----------------------------------------------------------------------------
out_gpkg <- here(PATH_AOI_GPKG)

sf::st_write(aoi_wgs84,   out_gpkg, layer = "aoi",          delete_dsn = TRUE,  quiet = TRUE)
sf::st_write(aoi_dl_wgs84, out_gpkg, layer = "aoi_download", delete_layer = TRUE, quiet = TRUE)
saveRDS(list(aoi = aoi_wgs84, aoi_download = aoi_dl_wgs84), here(PATH_AOI_RDS))

# -----------------------------------------------------------------------------
# 4. Print summary for verification
# -----------------------------------------------------------------------------
aoi_proj_check <- sf::st_transform(aoi_wgs84, CRS_PROJ)
area_km2       <- as.numeric(sf::st_area(aoi_proj_check)) / 1e6

message(
  "\nAOI defined successfully:",
  "\n  Bounding box (WGS84): ",
  "xmin=", AOI_BBOX["xmin"], "  xmax=", AOI_BBOX["xmax"],
  "  ymin=", AOI_BBOX["ymin"], "  ymax=", AOI_BBOX["ymax"],
  "\n  Area: ", round(area_km2, 0), " km²",
  "\n  Projection for analysis: ", CRS_PROJ,
  "\n  Output: ", PATH_AOI_GPKG
)

# -----------------------------------------------------------------------------
# 5. Quick visual check (opens plot window — close to continue)
# -----------------------------------------------------------------------------
plot(sf::st_geometry(aoi_wgs84),
     main = "Mt. Elgon AOI — core (black) and download buffer (grey)",
     col  = NA, border = "black", lwd = 2)
plot(sf::st_geometry(aoi_dl_wgs84),
     col = NA, border = "grey60", lwd = 1, lty = 2, add = TRUE)

message("\n=== P0 complete ===\n")
