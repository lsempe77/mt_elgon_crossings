# =============================================================================
# P1E_ingest_wealth_proxies.R
# PHASE 1E — Download household wealth and demographic proxy rasters
#
# Downloads:
#   1. Meta / Data For Good Relative Wealth Index (RWI) for Uganda
#      Source: Humanitarian Data Exchange (HDX), Meta AI Research.
#      Format: CSV with lat/lon + rwi score (~2.4 km resolution tiles).
#      Reference: Chi et al. (2022) Science.
#
#   2. WorldPop age-structured population rasters for Uganda (2020, 1km)
#      Source: worldpop.org (UN-adjusted, unconstrained).
#      Age groups used: 0–1, 1–4, 5–9, 10–14 (males + females)
#      Summed to create an "under-15 population" raster.
#      Combined with PATH_POP (total population) in P3B to compute
#      child dependency ratio = pop_u15 / pop_total within each buffer.
#
# Requires:
#   outputs/aoi/mt_elgon_bbox.gpkg     (from P0 — for crop extent)
#   R/utils/constants.R                (AOI_BBOX_DL, CRS_GEO, paths)
#   R package: httr2
#
# Outputs:
#   data/raw/wealth/uga_rwi.csv                Raw RWI point CSV
#   data/raw/wealth/rwi_uganda.tif             RWI rasterised (~2.4 km grid)
#   data/raw/wealth/worldpop_u15.tif           Under-15 pop summed (1km, WGS84)
#
# Runtime: ~5–15 minutes depending on connection speed.
#
# Run order: after P0; before P3B.
# =============================================================================

library(here)
library(terra)
library(sf)
library(dplyr)
library(httr2)
library(readr)

source(here("R/utils/constants.R"))
source(here("R/utils/io_helpers.R"))

if (!exists("FORCE_RERUN")) FORCE_RERUN <- FALSE

message("=== P1E: Ingesting wealth proxy data ===\n")

wealth_dir <- here("data/raw/wealth")
dir.create(wealth_dir, showWarnings = FALSE, recursive = TRUE)

# ─────────────────────────────────────────────────────────────────────────────
# PART 1 — Meta Relative Wealth Index (RWI)
# ─────────────────────────────────────────────────────────────────────────────

message("[Part 1/2] Meta Relative Wealth Index...")

rwi_csv_dest <- here(PATH_RWI_CSV)
rwi_tif_dest <- here(PATH_RWI)

if (file.exists(rwi_tif_dest) && !FORCE_RERUN) {
  message("  RWI raster already exists: ", PATH_RWI, " — skipping.")
} else {

  # ── 1a. Find the Uganda CSV via the HDX CKAN API ──────────────────────────
  # The Meta RWI dataset on HDX lists a CSV resource per country.
  # We search for the resource whose name/description includes "UGA" or "Uganda".
  if (!file.exists(rwi_csv_dest) || FORCE_RERUN) {
    message("  Looking up Uganda RWI on HDX CKAN API...")

    hdx_api <- paste0(
      "https://data.humdata.org/api/3/action/package_show",
      "?id=relative-wealth-index"
    )

    hdx_resp <- tryCatch(
      httr2::request(hdx_api) |>
        httr2::req_timeout(60) |>
        httr2::req_perform(),
      error = function(e) stop("[P1E] HDX API request failed: ",
                                conditionMessage(e), call. = FALSE)
    )

    pkg <- httr2::resp_body_json(hdx_resp)
    resources <- pkg$result$resources

    if (length(resources) == 0)
      stop("[P1E] HDX returned zero resources for relative-wealth-index.",
           call. = FALSE)

    # Filter: resource name or URL contains "UGA" or "Uganda"
    uga_res <- Filter(function(r) {
      nm  <- tolower(r$name  %||% "")
      url <- tolower(r$url   %||% "")
      grepl("uga|uganda", nm) || grepl("uga", url)
    }, resources)

    if (length(uga_res) == 0) {
      # Fallback: search descriptions
      uga_res <- Filter(function(r) {
        desc <- tolower(r$description %||% "")
        grepl("uga|uganda", desc)
      }, resources)
    }

    if (length(uga_res) == 0)
      stop("[P1E] Could not find Uganda resource in Meta RWI HDX dataset.\n",
           "Available resources: ",
           paste(sapply(resources, function(r) r$name %||% "?"),
                 collapse = ", "),
           call. = FALSE)

    rwi_dl_url <- uga_res[[1]]$url
    message("  Found Uganda RWI resource: ", uga_res[[1]]$name)
    message("  Download URL: ", rwi_dl_url)

    # ── 1b. Download CSV ─────────────────────────────────────────────────────
    message("  Downloading (~20–50 MB)...")
    tryCatch(
      httr2::request(rwi_dl_url) |>
        httr2::req_timeout(600) |>
        httr2::req_perform(path = rwi_csv_dest),
      error = function(e) stop("[P1E] RWI CSV download failed: ",
                                conditionMessage(e), call. = FALSE)
    )
    message("  Saved: ", PATH_RWI_CSV,
            " (", round(file.size(rwi_csv_dest) / 1e6, 1), " MB)")
  } else {
    message("  CSV already downloaded: ", PATH_RWI_CSV)
  }

  # ── 1c. Read CSV and inspect ──────────────────────────────────────────────
  message("  Reading RWI CSV...")
  rwi_df <- readr::read_csv(rwi_csv_dest, show_col_types = FALSE)

  # Expected columns: latitude, longitude, rwi, error
  needed <- c("latitude", "longitude", "rwi")
  missing_cols <- setdiff(needed, names(rwi_df))
  if (length(missing_cols) > 0) {
    # Try alternative names (e.g., "lat"/"lon", "x"/"y")
    name_map <- c(lat = "latitude", lon = "longitude",
                  x   = "longitude", y   = "latitude")
    for (old in names(name_map)) {
      if (old %in% names(rwi_df)) rwi_df <- dplyr::rename(rwi_df, !!name_map[old] := !!old)
    }
    missing_cols <- setdiff(needed, names(rwi_df))
    if (length(missing_cols) > 0)
      stop("[P1E] RWI CSV missing columns: ", paste(missing_cols, collapse=", "),
           "\nActual columns: ", paste(names(rwi_df), collapse=", "), call.=FALSE)
  }

  message("  RWI rows: ", nrow(rwi_df),
          " | Rwanda range: ",
          round(min(rwi_df$rwi, na.rm=TRUE), 2), " to ",
          round(max(rwi_df$rwi, na.rm=TRUE), 2))

  # ── 1d. Crop to Uganda/AOI and rasterise ─────────────────────────────────
  # Keep only points within expanded AOI bbox (reduces memory)
  rwi_aoi <- rwi_df |>
    dplyr::filter(
      longitude >= AOI_BBOX_DL["xmin"], longitude <= AOI_BBOX_DL["xmax"],
      latitude  >= AOI_BBOX_DL["ymin"], latitude  <= AOI_BBOX_DL["ymax"],
      !is.na(rwi)
    )

  message("  Points in AOI: ", nrow(rwi_aoi))

  if (nrow(rwi_aoi) < 5)
    stop("[P1E] Fewer than 5 RWI points found within AOI_BBOX_DL.\n",
         "Check that the Uganda CSV covers Mt. Elgon (lon 33.55-35.45, lat 0.17-2.23).",
         call. = FALSE)

  # Rasterise onto a ~0.022° (~2.4 km) grid matching RWI native resolution
  rwi_sf  <- sf::st_as_sf(rwi_aoi, coords = c("longitude", "latitude"),
                           crs = 4326)
  rwi_v   <- terra::vect(rwi_sf)

  # Template raster at RWI native resolution over AOI
  aoi_ext  <- terra::ext(AOI_BBOX_DL["xmin"], AOI_BBOX_DL["xmax"],
                          AOI_BBOX_DL["ymin"], AOI_BBOX_DL["ymax"])
  template <- terra::rast(aoi_ext, res = 0.022, crs = CRS_GEO)

  rwi_rast <- terra::rasterize(rwi_v, template, field = "rwi", fun = "mean")
  names(rwi_rast) <- "rwi"

  terra::writeRaster(rwi_rast, rwi_tif_dest, overwrite = TRUE,
                     gdal = c("COMPRESS=LZW"))

  message("  RWI raster saved: ", PATH_RWI,
          "\n  Resolution (approx): ",
          paste(round(terra::res(rwi_rast) * 111000, 0), collapse = " x "), " m",
          "\n  Non-NA cells: ",
          sum(!is.na(terra::values(rwi_rast))))
}

# ─────────────────────────────────────────────────────────────────────────────
# PART 2 — WorldPop under-15 population (child dependency ratio numerator)
# ─────────────────────────────────────────────────────────────────────────────

message("\n[Part 2/2] WorldPop age-structured population (under-15)...")

u15_dest <- here(PATH_WORLDPOP_U15)

if (file.exists(u15_dest) && !FORCE_RERUN) {
  message("  Under-15 raster already exists: ", PATH_WORLDPOP_U15, " — skipping.")
} else {

  wp_raw_dir <- file.path(wealth_dir, "worldpop_age_raw")
  dir.create(wp_raw_dir, showWarnings = FALSE)

  # Age groups that together make up under-15
  # WorldPop naming: 0 = ages 0–1, 1 = ages 1–4, 5 = ages 5–9, 10 = ages 10–14
  wp_ages   <- c(0, 1, 5, 10)
  wp_sexes  <- c("f", "m")
  wp_year   <- 2020
  wp_iso    <- "uga"
  wp_base   <- paste0(
    "https://data.worldpop.org/GIS/AgeSex_structures/",
    "Global_2000_2020/", wp_year, "/UGA/"
  )

  # Construct file list (no _1km suffix in the Global_2000_2020 dataset)
  wp_files <- expand.grid(sex = wp_sexes, age = wp_ages, stringsAsFactors = FALSE)
  wp_files$fname <- with(wp_files,
    sprintf("%s_%s_%d_%d.tif", wp_iso, sex, age, wp_year))
  wp_files$url   <- paste0(wp_base, wp_files$fname)
  wp_files$dest  <- file.path(wp_raw_dir, wp_files$fname)

  message("  Downloading ", nrow(wp_files), " WorldPop age/sex rasters...")

  # Download any missing files
  for (i in seq_len(nrow(wp_files))) {
    f <- wp_files[i, ]
    if (file.exists(f$dest) && !FORCE_RERUN) {
      message("    [skip] ", f$fname)
      next
    }
    message("    [get ] ", f$fname, " ...")
    ok <- tryCatch({
      httr2::request(f$url) |>
        httr2::req_timeout(300) |>
        httr2::req_perform(path = f$dest)
      TRUE
    }, error = function(e) {
      warning("[P1E] Failed to download ", f$fname, ": ", conditionMessage(e))
      FALSE
    })
    if (ok) {
      sz <- round(file.size(f$dest) / 1e6, 1)
      message("           → ", sz, " MB")
    }
  }

  # Check all downloaded
  missing_files <- wp_files$dest[!file.exists(wp_files$dest)]
  if (length(missing_files) > 0)
    stop("[P1E] ", length(missing_files), " WorldPop raster(s) failed to download:\n",
         paste(basename(missing_files), collapse = "\n"), call. = FALSE)

  # ── Sum all 8 rasters to get under-15 total population ───────────────────
  message("  Summing age groups to under-15 raster...")

  # Use first raster as template; reproject all to CRS_GEO
  r_list <- lapply(wp_files$dest, function(p) {
    r <- terra::rast(p)
    # Crop to download AOI immediately to keep memory manageable
    terra::crop(r, aoi_ext)
  })

  # Stack and sum; terra handles alignment via crop to common extent
  r_stack <- terra::rast(r_list)
  r_u15   <- terra::app(r_stack, fun = "sum", na.rm = TRUE)
  names(r_u15) <- "pop_u15"

  terra::writeRaster(r_u15, u15_dest, overwrite = TRUE,
                     gdal = c("COMPRESS=LZW"))

  total_u15 <- terra::global(r_u15, "sum", na.rm = TRUE)[[1]]
  message("  Under-15 raster saved: ", PATH_WORLDPOP_U15,
          "\n  Resolution: ",
          paste(round(terra::res(r_u15) * 111000, 0), collapse = " x "), " m",
          "\n  Total under-15 population in AOI: ",
          format(round(total_u15), big.mark = ","))
}

# ─────────────────────────────────────────────────────────────────────────────
# Summary
# ─────────────────────────────────────────────────────────────────────────────

message(
  "\n===== P1E SUMMARY =====",
  "\n  RWI raster    : ", PATH_RWI,
  "\n  Under-15 pop  : ", PATH_WORLDPOP_U15,
  "\n",
  "\nNext step: re-run P3B_socioec_outcomes.R with FORCE_RERUN <- TRUE",
  "\n  to extract rwi_2km and dep_ratio_2km for each crossing.",
  "\n========================\n"
)

message("=== P1E complete ===\n")
