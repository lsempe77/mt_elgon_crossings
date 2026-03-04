# =============================================================================
# io_helpers.R
# Standardised read / write wrappers with:
#   - Input existence checks (fail early with informative messages)
#   - Output column contract validation before writing
#   - Build log entry on every write (outputs/.build_log.csv)
#   - Skip-if-exists logic (set FORCE_RERUN = TRUE to override)
# =============================================================================

library(readr)
library(dplyr)

# Set FORCE_RERUN <- TRUE in your script to re-run even if output exists
if (!exists("FORCE_RERUN")) FORCE_RERUN <- FALSE

# -----------------------------------------------------------------------------
# Internal helpers
# -----------------------------------------------------------------------------

.log_write <- function(script, output_path, nrow = NA_integer_) {
  log_path <- here::here(PATH_BUILD_LOG)
  entry <- dplyr::tibble(
    timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    script      = script,
    output      = output_path,
    nrow        = nrow
  )
  if (file.exists(log_path)) {
    existing <- readr::read_csv(log_path, show_col_types = FALSE, col_types = readr::cols(timestamp = readr::col_character()))
    readr::write_csv(dplyr::bind_rows(existing, entry), log_path)
  } else {
    readr::write_csv(entry, log_path)
  }
  message("[build log] ", output_path, " (", nrow, " rows)")
}

.check_input <- function(path) {
  full <- here::here(path)
  if (!file.exists(full)) {
    stop(
      "\n[io_helpers] Required input not found:\n  ", full,
      "\nRun the upstream script that produces this file first.\n",
      call. = FALSE
    )
  }
  invisible(full)
}

.skip_if_done <- function(path) {
  full <- here::here(path)
  if (file.exists(full) && !FORCE_RERUN) {
    message("[io_helpers] Output already exists, skipping: ", path,
            "\n             Set FORCE_RERUN <- TRUE to re-run.")
    return(TRUE)
  }
  FALSE
}

.validate_cols <- function(df, required_cols, context) {
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(
      "\n[io_helpers] Column contract violated in: ", context,
      "\nMissing columns: ", paste(missing, collapse = ", "), "\n",
      call. = FALSE
    )
  }
  invisible(df)
}

# -----------------------------------------------------------------------------
# AOI
# -----------------------------------------------------------------------------

read_aoi <- function() {
  .check_input(PATH_AOI_GPKG)
  sf::st_read(here::here(PATH_AOI_GPKG), layer = "aoi", quiet = TRUE)
}

write_aoi <- function(aoi_sf, script = "P0_define_aoi.R") {
  sf::st_write(aoi_sf, here::here(PATH_AOI_GPKG),
               layer = "aoi", delete_dsn = TRUE, quiet = TRUE)
  saveRDS(aoi_sf, here::here(PATH_AOI_RDS))
  .log_write(script, PATH_AOI_GPKG, nrow(aoi_sf))
}

# -----------------------------------------------------------------------------
# Crossings spine (master join key — Phase 2 output)
# -----------------------------------------------------------------------------

CROSSINGS_REQUIRED_COLS <- c(
  "crossing_id", "elev_m"
)

read_crossings <- function() {
  .check_input(PATH_CROSSINGS)
  sf::st_read(here::here(PATH_CROSSINGS), layer = "crossings", quiet = TRUE)
}

write_crossings <- function(crossings_sf, script = "P2_generate_crossings.R") {
  .validate_cols(crossings_sf, CROSSINGS_REQUIRED_COLS, "crossings.gpkg")
  dups <- sum(duplicated(crossings_sf$crossing_id))
  if (dups > 0) stop("[io_helpers] ", dups, " duplicate crossing_id values found.",
                     call. = FALSE)
  sf::st_write(crossings_sf, here::here(PATH_CROSSINGS),
               layer = "crossings", delete_dsn = TRUE, quiet = TRUE)
  .log_write(script, PATH_CROSSINGS, nrow(crossings_sf))
}

# -----------------------------------------------------------------------------
# Hydro covariates (Phase 3A output)
# -----------------------------------------------------------------------------

HYDRO_REQUIRED_COLS <- c(
  "crossing_id", "slope_deg", "catchment_area_km2",
  "stream_order", "flood_Q10", "flood_Q50"
)

read_hydro_covariates <- function() {
  .check_input(PATH_HYDRO_COV)
  readr::read_csv(here::here(PATH_HYDRO_COV), show_col_types = FALSE)
}

write_hydro_covariates <- function(df, script = "P3A_hydro_covariates.R") {
  .validate_cols(df, HYDRO_REQUIRED_COLS, "hydro_covariates.csv")
  readr::write_csv(df, here::here(PATH_HYDRO_COV))
  .log_write(script, PATH_HYDRO_COV, nrow(df))
}

# -----------------------------------------------------------------------------
# Socioeconomic outcomes (Phase 3B output)
# -----------------------------------------------------------------------------

SOCIO_REQUIRED_COLS <- c(
  "crossing_id",
  "ntl_500m", "ntl_2km",
  "pop_500m", "pop_2km",
  "travel_time_min",
  "agri_suit_2km",
  "road_dens_2km",
  "rwi_2km",
  "dep_ratio_2km"
)

read_socio_outcomes <- function() {
  .check_input(PATH_SOCIO_COV)
  readr::read_csv(here::here(PATH_SOCIO_COV), show_col_types = FALSE)
}

write_socio_outcomes <- function(df, script = "P3B_socioec_outcomes.R") {
  .validate_cols(df, SOCIO_REQUIRED_COLS, "socioeconomic_outcomes.csv")
  readr::write_csv(df, here::here(PATH_SOCIO_COV))
  .log_write(script, PATH_SOCIO_COV, nrow(df))
}

# -----------------------------------------------------------------------------
# Analysis dataset (Phase 4 input)
# -----------------------------------------------------------------------------

read_analysis_dataset <- function() {
  .check_input(PATH_ANALYSIS_DATA)
  readr::read_csv(here::here(PATH_ANALYSIS_DATA), show_col_types = FALSE)
}

write_analysis_dataset <- function(df, script = "P4_build_dataset.R") {
  readr::write_csv(df, here::here(PATH_ANALYSIS_DATA))
  .log_write(script, PATH_ANALYSIS_DATA, nrow(df))
}
