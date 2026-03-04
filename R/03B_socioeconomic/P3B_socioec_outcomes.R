# =============================================================================
# P3B_socioec_outcomes.R
# PHASE 3B — Extract socioeconomic outcome proxies at each crossing
#
# WORKER B runs this after P2 is complete.
# Run in parallel with P3A_hydro_covariates.R.
#
# Requires:
#   data/processed/crossings/crossings.gpkg    (from P2 — master spine)
#   data/raw/ntl/ntl_2014.tif                  (from P1C)
#   data/raw/population/worldpop_mt_elgon.tif  (from P1C)
#   data/raw/roads/paths_osm.gpkg              (from P1B)
#   data/raw/travel_time/accessibility_2015.tif (from P1D)
#   data/raw/landuse/gaez_rainfed_suit.tif     (from P1D)
#   data/raw/wealth/rwi_uganda.tif             (from P1E — Meta RWI raster)
#   data/raw/wealth/worldpop_u15.tif           (from P1E — under-15 population)
#
# Outputs:
#   outputs/covariates/socioeconomic_outcomes.csv
#
# Output columns (see io_helpers.R::SOCIO_REQUIRED_COLS for contract):
#   crossing_id, ntl_500m, ntl_2km, pop_500m, pop_2km,
#   travel_time_min, agri_suit_2km, road_dens_2km,
#   rwi_2km, dep_ratio_2km
#
# Runtime: ~20–40 minutes (road density computation is the slow step).
# =============================================================================

library(here)
library(sf)
library(terra)
library(dplyr)
library(purrr)

source(here("R/utils/constants.R"))
source(here("R/utils/io_helpers.R"))
source(here("R/utils/spatial_helpers.R"))

message("=== P3B: Extracting socioeconomic outcome proxies ===")

.check_input(PATH_CROSSINGS)
.check_input(PATH_NTL)
.check_input(PATH_POP)
.check_input(PATH_ROADS_OSM)
.check_input(PATH_TRAVEL_TIME)
.check_input(PATH_GAEZ)
.check_input(PATH_RWI)
.check_input(PATH_WORLDPOP_U15)

# -----------------------------------------------------------------------------
# Load master spine and pre-compute buffers
# Doing this once avoids repeating st_buffer() for each variable.
# -----------------------------------------------------------------------------
crossings <- read_crossings() |>
  sf::st_transform(CRS_PROJ)

n <- nrow(crossings)
message("Processing ", n, " crossings...")
message("Pre-computing buffers...")

buf_small <- sf::st_buffer(crossings, dist = BUFFER_SMALL_M)
buf_large <- sf::st_buffer(crossings, dist = BUFFER_LARGE_M)

# Helper: project raster to CRS_PROJ, extract buffer mean
extract_buf_mean <- function(rast_path, buffers, label) {
  message("  Extracting: ", label, "...")
  r    <- terra::project(terra::rast(here(rast_path)), CRS_PROJ)
  vals <- terra::extract(r, terra::vect(buffers), fun = "mean", na.rm = TRUE)
  vals[, 2]
}

# -----------------------------------------------------------------------------
# STEP 1 — Nighttime lights
# Baseline year (NTL_BASELINE_YEAR = 2014) = pre-Fika Uganda programme.
# Using log1p transform for the analysis, but store raw values here.
# -----------------------------------------------------------------------------
message("\n[Step 1/7] Nighttime lights (", NTL_BASELINE_YEAR, " baseline)...")

ntl_500m <- extract_buf_mean(PATH_NTL, buf_small, "NTL 500m")
ntl_2km  <- extract_buf_mean(PATH_NTL, buf_large, "NTL 2km")

# Coefficient of variation within 2km buffer (spatial heterogeneity proxy)
ntl_r      <- terra::project(terra::rast(here(PATH_NTL)), CRS_PROJ)
ntl_sd_2km <- terra::extract(ntl_r, terra::vect(buf_large),
                              fun = "sd", na.rm = TRUE)[, 2]
ntl_cv_2km <- ntl_sd_2km / (ntl_2km + 1e-6)   # avoid division by zero

# -----------------------------------------------------------------------------
# STEP 2 — Population density
# WorldPop stores persons per pixel; we want persons per km².
# Pixel area varies slightly with latitude — use terra::cellSize() to correct.
# -----------------------------------------------------------------------------
message("\n[Step 2/7] Population density...")

pop_r     <- terra::project(terra::rast(here(PATH_POP)), CRS_PROJ)
cell_area_km2 <- terra::cellSize(pop_r, unit = "km") # km² per cell

# Per-pixel density = pop_count / cell_area_km2 (already persons/km2 in WorldPop
# at 100m, but geodata returns coarser resolution — normalise to be safe)
pop_dens_r <- pop_r / cell_area_km2
names(pop_dens_r) <- "pop_per_km2"

pop_500m <- terra::extract(pop_dens_r, terra::vect(buf_small),
                            fun = "mean", na.rm = TRUE)[, 2]
pop_2km  <- terra::extract(pop_dens_r, terra::vect(buf_large),
                            fun = "mean", na.rm = TRUE)[, 2]

pop_total_2km <- terra::extract(pop_r, terra::vect(buf_large),
                                 fun = "sum", na.rm = TRUE)[, 2]

# -----------------------------------------------------------------------------
# STEP 3 — Road/path density within 2km buffer
# Measured as total km of road per km² of buffer area.
# This is the slowest step — iterates over each crossing.
# -----------------------------------------------------------------------------
message("\n[Step 3/7] Road density within ", BUFFER_LARGE_M/1000, "km buffer...")
message("  (This may take 10–20 minutes for large crossing sets)")

roads <- sf::st_read(here(PATH_ROADS_OSM), layer = "roads", quiet = TRUE) |>
  sf::st_transform(CRS_PROJ) |>
  sf::st_make_valid()

# Use spatial index for speed
roads_idx <- sf::st_sfc(roads$geometry)

road_dens_2km <- vapply(seq_len(nrow(buf_large)), function(i) {
  if (i %% 100 == 0) message("    ... ", i, "/", n)
  buf_i   <- buf_large[i, ]
  clipped <- suppressWarnings(sf::st_intersection(roads, buf_i))
  if (nrow(clipped) == 0) return(0)
  len_km  <- sum(as.numeric(sf::st_length(clipped))) / 1000
  area_km2 <- as.numeric(sf::st_area(buf_i)) / 1e6
  len_km / area_km2
}, numeric(1))

# Count of distinct road segments within buffer
road_count_2km <- vapply(seq_len(nrow(buf_large)), function(i) {
  buf_i <- buf_large[i, ]
  nrow(suppressWarnings(sf::st_intersection(roads, buf_i)))
}, integer(1))

# -----------------------------------------------------------------------------
# STEP 4 — Travel time to city (point extract — from Weiss et al. 2020)
# -----------------------------------------------------------------------------
message("\n[Step 4/7] Travel time to city...")

travel_time_min <- extract_buf_mean(PATH_TRAVEL_TIME, crossings,
                                    "travel time (point)")
# Also get 2km buffer mean (in case point falls in nodata)
tt_r        <- terra::project(terra::rast(here(PATH_TRAVEL_TIME)), CRS_PROJ)
tt_2km_mean <- terra::extract(tt_r, terra::vect(buf_large),
                               fun = "mean", na.rm = TRUE)[, 2]

# Fill point NAs with buffer mean
travel_time_min <- dplyr::if_else(is.na(travel_time_min), tt_2km_mean,
                                   travel_time_min)

# -----------------------------------------------------------------------------
# STEP 5 — Agricultural suitability
# -----------------------------------------------------------------------------
message("\n[Step 5/7] Agricultural suitability...")

agri_suit_2km <- extract_buf_mean(PATH_GAEZ, buf_large, "agri suitability 2km")
agri_suit_500m <- extract_buf_mean(PATH_GAEZ, buf_small, "agri suitability 500m")

# -----------------------------------------------------------------------------
# STEP 6 — Relative Wealth Index (Meta / Data For Good)
# Nearest-feature match from RWI tile centroids (CSV).
# Each RWI tile is ~2.4 km — matching by nearest centroid is the natural unit.
# Buffer-based raster extraction fails (too few tiles per 2km buffer).
# Reference: Chi et al. (2022) Science.
# -----------------------------------------------------------------------------
message("\n[Step 6/7] Relative Wealth Index (Meta RWI — nearest tile)...")

rwi_csv <- readr::read_csv(here(PATH_RWI_CSV), show_col_types = FALSE)

# Use "latitude"/"longitude" columns; rename if needed
if (!"latitude"  %in% names(rwi_csv)) rwi_csv <- dplyr::rename(rwi_csv, latitude  = lat)
if (!"longitude" %in% names(rwi_csv)) rwi_csv <- dplyr::rename(rwi_csv, longitude = lon)

rwi_pts <- sf::st_as_sf(
  rwi_csv[!is.na(rwi_csv$rwi), ],
  coords = c("longitude", "latitude"), crs = 4326
) |> sf::st_transform(CRS_PROJ)

# Nearest RWI tile centroid for each crossing
nn_idx      <- sf::st_nearest_feature(crossings, rwi_pts)
nn_dist_m   <- as.numeric(sf::st_distance(crossings, rwi_pts[nn_idx, ], by_element = TRUE))

# Assign nearest RWI tile value. All crossings are within Uganda; the nearest
# tile is always within the country and represents regional wealth.
# Record distance so analysts can see how far the tile is from the crossing.
rwi_2km      <- rwi_pts$rwi[nn_idx]
rwi_dist_km  <- nn_dist_m / 1000

message("  RWI non-NA: ", sum(!is.na(rwi_2km)), "/", length(rwi_2km),
        "  |  median nearest dist: ",
        round(median(nn_dist_m[!is.na(rwi_2km)]) / 1000, 1), " km")

# -----------------------------------------------------------------------------
# STEP 7 — Child dependency ratio (WorldPop age structure)
# dep_ratio = under-15 pop / total pop within 2km buffer.
# Proxy for demographic structure: high ratio → young, potentially poorer area.
# Under-15 raster produced by P1E (sum of WorldPop 0-1, 1-4, 5-9, 10-14 bands).
# Total population from PATH_POP (already loaded above as pop_r / pop_total_2km).
# -----------------------------------------------------------------------------
message("\n[Step 7/7] Child dependency ratio...")

u15_r   <- terra::project(terra::rast(here(PATH_WORLDPOP_U15)), CRS_PROJ)
u15_raw <- terra::extract(u15_r, terra::vect(buf_large),
                          fun = "sum", na.rm = TRUE)[, 2]

# WorldPop age-structure rasters use NA masking in forest / no-data areas.
# Treat NA under-15 as 0 where the total pop raster shows no population;
# keep NA only when total pop is unknown.
# Rule: if u15_raw is NA but pop_total_2km > 0, set u15 = 0 (forest masking artefact).
#       if u15_raw is NA and pop_total_2km is also NA/0, dep_ratio = NA.
u15_2km <- dplyr::case_when(
  !is.na(u15_raw)                              ~ u15_raw,         # normal
  is.na(u15_raw) & !is.na(pop_total_2km)       ~ 0,               # age mask, pop exists
  TRUE                                          ~ NA_real_         # genuinely unknown
)

dep_ratio_2km <- dplyr::if_else(
  !is.na(pop_total_2km) & pop_total_2km > 0,
  u15_2km / pop_total_2km,
  NA_real_
)

message("  dep_ratio non-NA: ", sum(!is.na(dep_ratio_2km)), "/", length(dep_ratio_2km))

# -----------------------------------------------------------------------------
# Assemble and write output
# -----------------------------------------------------------------------------
message("\nAssembling output...")

socio_out <- dplyr::tibble(
  crossing_id    = crossings$crossing_id,
  ntl_500m       = ntl_500m,
  ntl_2km        = ntl_2km,
  ntl_cv_2km     = ntl_cv_2km,
  pop_500m       = pop_500m,
  pop_2km        = pop_2km,
  pop_total_2km  = pop_total_2km,
  travel_time_min = travel_time_min,
  agri_suit_500m  = agri_suit_500m,
  agri_suit_2km   = agri_suit_2km,
  road_dens_2km   = road_dens_2km,
  road_count_2km  = road_count_2km,
  rwi_2km        = rwi_2km,
  rwi_dist_km    = rwi_dist_km,   # distance to nearest RWI tile centroid
  dep_ratio_2km  = dep_ratio_2km
)

write_socio_outcomes(socio_out, script = "P3B_socioec_outcomes.R")

# -----------------------------------------------------------------------------
# Summary statistics
# -----------------------------------------------------------------------------
na_counts <- colSums(is.na(socio_out))

message(
  "\nSocioeconomic outcomes summary:",
  "\n  ntl_2km (mean)       : ", round(mean(socio_out$ntl_2km, na.rm=T), 3),
  " nW/cm²/sr",
  "\n  pop_2km (median)     : ", round(median(socio_out$pop_2km, na.rm=T), 0),
  " persons/km²",
  "\n  travel_time (median) : ", round(median(socio_out$travel_time_min, na.rm=T), 0),
  " minutes",
  "\n  road_dens (median)   : ", round(median(socio_out$road_dens_2km, na.rm=T), 2),
  " km/km²",
  "\n  agri_suit (mean)     : ", round(mean(socio_out$agri_suit_2km, na.rm=T), 1),
  "\n  rwi_2km (median)     : ", round(median(socio_out$rwi_2km, na.rm=T), 3),
  "\n  dep_ratio_2km (mean) : ", round(mean(socio_out$dep_ratio_2km, na.rm=T), 3),
  "\n  NAs per column:"
)
print(na_counts[na_counts > 0])

message("\n=== P3B complete ===\n")
