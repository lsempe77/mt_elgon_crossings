# =============================================================================
# P1A_ingest_rivers.R
# PHASE 1A — Download OSM waterway network for Mt. Elgon AOI
#
# WORKER A runs this (alongside P1A_ingest_dem.R — independent).
# Requires: outputs/aoi/mt_elgon_bbox.gpkg  (from P0)
#           Internet connection
#
# Outputs:
#   data/raw/rivers/rivers_osm.gpkg   (all OSM waterway lines)
#
# Runtime: ~2–5 minutes (Overpass API query).
# =============================================================================

library(here)
library(osmdata)
library(sf)
library(dplyr)

source(here("R/utils/constants.R"))
source(here("R/utils/io_helpers.R"))

message("=== P1A: Downloading OSM waterways ===")

if (.skip_if_done(PATH_RIVERS_OSM)) stop("Skipping.", call. = FALSE)

# -----------------------------------------------------------------------------
# 1. Build Overpass query
#    Use the download bbox (padded) so we capture rivers crossing the AOI edge.
# -----------------------------------------------------------------------------
bbox_vec <- c(
  AOI_BBOX_DL["ymin"], AOI_BBOX_DL["xmin"],
  AOI_BBOX_DL["ymax"], AOI_BBOX_DL["xmax"]
)  # osmdata expects c(south, west, north, east)

message("Querying Overpass API for waterways...")
message("  bbox: ", paste(round(bbox_vec, 3), collapse=", "))

q <- osmdata::opq(
  bbox    = bbox_vec,
  timeout = 120    # seconds — increase if query times out
) |>
  osmdata::add_osm_feature(
    key   = "waterway",
    value = c("river", "stream", "canal", "drain")
  )

osm_raw <- tryCatch(
  osmdata::osmdata_sf(q),
  error = function(e) {
    stop("[P1A rivers] Overpass query failed: ", conditionMessage(e),
         "\nTry again later or increase timeout.", call. = FALSE)
  }
)

# -----------------------------------------------------------------------------
# 2. Extract lines, keep useful attributes, reproject to WGS84 for storage
# -----------------------------------------------------------------------------
rivers <- osm_raw$osm_lines

if (is.null(rivers) || nrow(rivers) == 0) {
  stop("[P1A rivers] No waterway lines returned. Check bbox or Overpass status.",
       call. = FALSE)
}

rivers <- rivers |>
  dplyr::select(
    osm_id,
    name,
    waterway,
    geometry
  ) |>
  dplyr::rename(river_name = name, waterway_type = waterway)

# Ensure valid geometries
rivers <- sf::st_make_valid(rivers)
rivers <- rivers[!sf::st_is_empty(rivers), ]

# FIX: osmdata returns coordinates in EPSG:4326 authority order (lat as X, lon as Y).
# sf uses GIS convention (X=lon, Y=lat), so physically swap the coordinates.
geoms_fixed <- lapply(sf::st_geometry(rivers), function(g) {
  co <- sf::st_coordinates(g)  # col1=lat, col2=lon
  if (ncol(co) >= 3 && length(unique(co[,3])) > 1) {
    parts <- lapply(sort(unique(co[,3])), function(l)
      co[co[,3]==l, c(2,1), drop=FALSE])
    sf::st_multilinestring(parts)
  } else {
    sf::st_linestring(co[, c(2,1)])
  }
})
rivers <- sf::st_set_geometry(rivers,
  sf::st_sfc(geoms_fixed, crs = sf::st_crs(4326)))

# Project to UTM 36N for storage (unambiguous axis order)
rivers <- sf::st_transform(rivers, CRS_PROJ)

# -----------------------------------------------------------------------------
# 3. Write output
# -----------------------------------------------------------------------------
sf::st_write(rivers, here(PATH_RIVERS_OSM),
             layer = "rivers", delete_dsn = TRUE, quiet = TRUE)

message(
  "\nWaterways saved:",
  "\n  Features   : ", nrow(rivers),
  "\n  Types      : ", paste(sort(unique(rivers$waterway_type)), collapse=", "),
  "\n  CRS        : ", sf::st_crs(rivers)$epsg,
  "\n  Output     : ", PATH_RIVERS_OSM
)

message("\n=== P1A: Rivers complete ===\n")
