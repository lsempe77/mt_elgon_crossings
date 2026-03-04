# =============================================================================
# P1B_ingest_roads.R
# PHASE 1B — Download OSM pedestrian and road network for Mt. Elgon AOI
#
# WORKER B runs this.
# Requires: outputs/aoi/mt_elgon_bbox.gpkg  (from P0)
#           Internet connection
#
# Outputs:
#   data/raw/roads/paths_osm.gpkg   (footpaths, tracks, and roads)
#
# Note: We download a broad set of highway types to capture all possible
# pedestrian river crossings. The crossing detection in Phase 2 will filter
# to walkable routes only.
#
# Runtime: ~3–8 minutes (large Overpass query).
# =============================================================================

library(here)
library(osmdata)
library(sf)
library(dplyr)

source(here("R/utils/constants.R"))
source(here("R/utils/io_helpers.R"))

message("=== P1B: Downloading OSM road/path network ===")

if (.skip_if_done(PATH_ROADS_OSM)) stop("Skipping.", call. = FALSE)

# -----------------------------------------------------------------------------
# 1. Overpass query — pedestrian-accessible highway types
#    Ordered from most to least pedestrian-relevant:
#      path / footway / track  →  primary pedestrian routes
#      unclassified / tertiary →  low-grade roads also used on foot
#      secondary               →  included to catch bridges on named roads
# -----------------------------------------------------------------------------
bbox_vec <- c(
  AOI_BBOX_DL["ymin"], AOI_BBOX_DL["xmin"],
  AOI_BBOX_DL["ymax"], AOI_BBOX_DL["xmax"]
)

highway_types <- c(
  "path", "footway", "track",
  "unclassified", "tertiary", "secondary", "primary"
)

message("Querying Overpass API for road/path network...")
message("  highway types: ", paste(highway_types, collapse=", "))

q <- osmdata::opq(
  bbox    = bbox_vec,
  timeout = 180
) |>
  osmdata::add_osm_feature(
    key   = "highway",
    value = highway_types
  )

osm_raw <- tryCatch(
  osmdata::osmdata_sf(q),
  error = function(e) {
    stop("[P1B] Overpass query failed: ", conditionMessage(e),
         "\nTry again later or increase timeout.", call. = FALSE)
  }
)

# -----------------------------------------------------------------------------
# 2. Extract and clean
# -----------------------------------------------------------------------------
roads <- osm_raw$osm_lines

if (is.null(roads) || nrow(roads) == 0) {
  stop("[P1B] No road/path features returned. Check bbox or Overpass status.",
       call. = FALSE)
}

roads <- roads |>
  dplyr::select(
    osm_id,
    name,
    highway,
    surface,
    geometry
  ) |>
  dplyr::rename(road_name = name, road_type = highway)

# Tag pedestrian-priority routes for Phase 2 crossing classification
roads <- roads |>
  dplyr::mutate(
    is_pedestrian = road_type %in% c("path", "footway", "track")
  )

roads <- sf::st_make_valid(roads)
roads <- roads[!sf::st_is_empty(roads), ]

# FIX: osmdata returns coordinates in EPSG:4326 authority order (lat as X, lon as Y).
# sf uses GIS convention (X=lon, Y=lat), so physically swap the coordinates.
geoms_fixed <- lapply(sf::st_geometry(roads), function(g) {
  co <- sf::st_coordinates(g)  # col1=lat, col2=lon
  if (ncol(co) >= 3 && length(unique(co[,3])) > 1) {
    parts <- lapply(sort(unique(co[,3])), function(l)
      co[co[,3]==l, c(2,1), drop=FALSE])
    sf::st_multilinestring(parts)
  } else {
    sf::st_linestring(co[, c(2,1)])
  }
})
roads <- sf::st_set_geometry(roads,
  sf::st_sfc(geoms_fixed, crs = sf::st_crs(4326)))

# Project to UTM 36N for storage (unambiguous axis order)
roads <- sf::st_transform(roads, CRS_PROJ)

# -----------------------------------------------------------------------------
# 3. Write output
# -----------------------------------------------------------------------------
sf::st_write(roads, here(PATH_ROADS_OSM),
             layer = "roads", delete_dsn = TRUE, quiet = TRUE)

type_counts <- roads |>
  sf::st_drop_geometry() |>
  dplyr::count(road_type, sort = TRUE)

message(
  "\nRoads/paths saved:",
  "\n  Total features : ", nrow(roads),
  "\n  Pedestrian     : ", sum(roads$is_pedestrian),
  "\n  By type:"
)
print(type_counts)
message("  CRS    : ", sf::st_crs(roads)$epsg)
message("  Output : ", PATH_ROADS_OSM)

message("\n=== P1B: Roads complete ===\n")
