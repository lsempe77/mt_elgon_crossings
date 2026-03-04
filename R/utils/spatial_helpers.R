# =============================================================================
# spatial_helpers.R
# Reusable spatial utility functions used across Phase 3A, 3B, and 4.
# All functions assume inputs are already in CRS_PROJ (EPSG:32636) unless
# noted otherwise.
# =============================================================================

library(sf)
library(terra)
library(dplyr)

# -----------------------------------------------------------------------------
# Buffer extraction helpers
# -----------------------------------------------------------------------------

#' Extract mean raster value within circular buffers around points
#'
#' @param rast_path  Path to raster file (character)
#' @param points_sf  sf POINT object in CRS_PROJ
#' @param radius_m   Buffer radius in metres
#' @param fun        Summary function: "mean", "sum", "modal" (default "mean")
#' @param col_name   Name for the output column
#' @return Named numeric vector, same length as nrow(points_sf)
extract_buffer_stat <- function(rast_path, points_sf, radius_m,
                                fun = "mean", col_name = NULL) {
  stopifnot(file.exists(here::here(rast_path)))
  r    <- terra::rast(here::here(rast_path))
  bufs <- sf::st_buffer(points_sf, dist = radius_m)
  vals <- terra::extract(r, terra::vect(bufs), fun = fun, na.rm = TRUE)
  out  <- vals[, 2]
  if (!is.null(col_name)) names(out) <- rep(col_name, length(out))
  out
}

#' Extract raster value at point locations (no buffer)
#'
#' @param rast_path  Path to raster file
#' @param points_sf  sf POINT object
#' @return Numeric vector
extract_at_point <- function(rast_path, points_sf) {
  stopifnot(file.exists(here::here(rast_path)))
  r    <- terra::rast(here::here(rast_path))
  vals <- terra::extract(r, terra::vect(points_sf))
  vals[, 2]
}

# -----------------------------------------------------------------------------
# Road density helper
# -----------------------------------------------------------------------------

#' Compute road/path density (km of road per km²) within buffers
#'
#' @param roads_sf   sf LINESTRING object of roads/paths in CRS_PROJ
#' @param points_sf  sf POINT object in CRS_PROJ
#' @param radius_m   Buffer radius in metres
#' @return Numeric vector of road density values (km / km²)
compute_road_density <- function(roads_sf, points_sf, radius_m) {
  bufs      <- sf::st_buffer(points_sf, dist = radius_m)
  buf_area  <- as.numeric(sf::st_area(bufs)) / 1e6  # km²

  road_len <- vapply(seq_len(nrow(bufs)), function(i) {
    clipped <- suppressWarnings(sf::st_intersection(roads_sf, bufs[i, ]))
    if (nrow(clipped) == 0) return(0)
    sum(as.numeric(sf::st_length(clipped))) / 1000  # km
  }, numeric(1))

  road_len / buf_area
}

# -----------------------------------------------------------------------------
# Bearing and orthogonality helpers (used in Phase 2)
# -----------------------------------------------------------------------------

#' Compute bearing of a linestring at a given point (degrees, 0–360 from north)
#'
#' Extracts the nearest line segment within `snap_m` metres of `point`,
#' fits a direction, and returns the bearing.
#'
#' @param line_sf   sf LINESTRING (single feature)
#' @param point_sf  sf POINT (single feature)
#' @param snap_m    Search radius in metres for local segment extraction
#' @return Bearing in degrees (0–360), or NA if no segment found
bearing_at_point <- function(line_sf, point_sf, snap_m = 100) {
  # Extract coordinates of line segment nearest to the crossing point
  coords <- sf::st_coordinates(line_sf)[, 1:2]
  if (nrow(coords) < 2) return(NA_real_)

  pt   <- sf::st_coordinates(point_sf)[1, ]
  dsts <- sqrt((coords[, 1] - pt[1])^2 + (coords[, 2] - pt[2])^2)
  idx  <- which.min(dsts)

  # Use surrounding vertices (±1) to get local direction
  i1 <- max(1, idx - 1)
  i2 <- min(nrow(coords), idx + 1)
  dx <- coords[i2, 1] - coords[i1, 1]
  dy <- coords[i2, 2] - coords[i1, 2]

  bearing <- (atan2(dx, dy) * 180 / pi) %% 360
  bearing
}

#' Compute acute angle (0–90°) between two bearings
#'
#' @param b1  Bearing 1 in degrees
#' @param b2  Bearing 2 in degrees
#' @return Acute angle in degrees
acute_angle <- function(b1, b2) {
  diff <- abs(b1 - b2) %% 180
  ifelse(diff > 90, 180 - diff, diff)
}

# -----------------------------------------------------------------------------
# Spatial weights helpers (used in Phase 4)
# -----------------------------------------------------------------------------

#' Build k-nearest-neighbour spatial weights list
#'
#' @param points_sf  sf POINT object
#' @param k          Number of neighbours
#' @return spdep listw object
build_knn_weights <- function(points_sf, k = KNN_NEIGHBORS) {
  coords <- sf::st_coordinates(points_sf)
  nb     <- spdep::knn2nb(spdep::knearneigh(coords, k = k))
  spdep::nb2listw(nb, style = "W")
}

# -----------------------------------------------------------------------------
# AOI polygon helper
# -----------------------------------------------------------------------------

#' Create sf polygon from a named bbox vector
#'
#' @param bbox  Named numeric: c(xmin, xmax, ymin, ymax) in WGS84
#' @param crs   CRS string (default CRS_GEO)
#' @return sf data frame with one POLYGON row
bbox_to_sf <- function(bbox, crs = CRS_GEO) {
  bb <- structure(
    c(xmin = as.numeric(bbox["xmin"]),
      ymin = as.numeric(bbox["ymin"]),
      xmax = as.numeric(bbox["xmax"]),
      ymax = as.numeric(bbox["ymax"])),
    class = "bbox",
    crs   = sf::st_crs(crs)
  )
  poly <- sf::st_as_sfc(bb)
  sf::st_sf(id = "aoi", geometry = poly)
}
