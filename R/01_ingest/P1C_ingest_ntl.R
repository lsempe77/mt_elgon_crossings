# =============================================================================
# P1C_ingest_ntl.R
# PHASE 1C — Download VIIRS Black Marble nighttime lights (2014 annual)
#
# Downloads VNP46A4 (annual composite) directly from NASA LAADS DAAC,
# bypassing blackmarbler's terra::rast() call which fails on Windows HDF5.
# Reads using GDAL HDF5 subdataset syntax which terra supports natively.
#
# Tile needed: h21v08 (covers 30–40E, 0–10N — includes all of Mt. Elgon)
#
# WORKER C runs this.
# Requires: outputs/aoi/mt_elgon_bbox.gpkg  (from P0)
#           NASA_BEARER_TOKEN in ~/.Renviron
#
# Outputs:
#   data/raw/ntl/ntl_2014.tif   (VNP46A4 annual composite, WGS84)
# =============================================================================

library(here)
library(terra)
library(sf)
library(httr2)

source(here("R/utils/constants.R"))
source(here("R/utils/io_helpers.R"))

message("=== P1C: Downloading nighttime lights ===")

if (.skip_if_done(PATH_NTL)) stop("Skipping.", call. = FALSE)

# -----------------------------------------------------------------------------
# 1. Check bearer token
# -----------------------------------------------------------------------------
bearer <- Sys.getenv("NASA_BEARER_TOKEN")
if (nchar(bearer) == 0) {
  stop("[P1C] NASA_BEARER_TOKEN not found in environment.\n",
       "Add to .Renviron: NASA_BEARER_TOKEN=your_token", call. = FALSE)
}

ntl_dl_dir <- here("data/raw/ntl/h5")
dir.create(ntl_dl_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 2. Find the VNP46A4 file for tile h21v08, year 2014
#    LAADS DAAC directory listing for VNP46A4 DOY 001 (annual = DOY 001)
# -----------------------------------------------------------------------------
LAADS_BASE <- "https://ladsweb.modaps.eosdis.nasa.gov"
TILE       <- "h21v08"
YEAR       <- NTL_BASELINE_YEAR
DOY        <- "001"

# Use NASA CMR API to find the granule download URL — more reliable than
# directory listing, and works regardless of collection version or timestamp.
CMR_URL <- paste0(
  "https://cmr.earthdata.nasa.gov/search/granules.json",
  "?short_name=VNP46A4",
  "&bounding_box=", AOI_BBOX["xmin"], ",", AOI_BBOX["ymin"], ",",
                    AOI_BBOX["xmax"], ",", AOI_BBOX["ymax"],
  "&temporal=", YEAR, "-01-01T00:00:00Z,", YEAR, "-12-31T23:59:59Z",
  "&page_size=20"
)

message("Searching NASA CMR for VNP46A4 granules...")
message("  URL: ", CMR_URL)

cmr_resp <- tryCatch(
  httr2::request(CMR_URL) |>
    httr2::req_headers(Authorization = paste("Bearer", bearer)) |>
    httr2::req_perform(),
  error = function(e) stop("[P1C] CMR search failed: ", conditionMessage(e),
                            call. = FALSE)
)

cmr_json <- httr2::resp_body_json(cmr_resp)
entries  <- cmr_json$feed$entry

if (length(entries) == 0) {
  stop("[P1C] No VNP46A4 granules found via CMR for bbox/year.\n",
       "Check bounding box or year.", call. = FALSE)
}

message("Found ", length(entries), " granule(s). Looking for tile ", TILE, "...")

# Find the granule matching our VIIRS tile
tile_entry <- Filter(function(e) {
  any(grepl(TILE, unlist(e$links), fixed = TRUE))
}, entries)

if (length(tile_entry) == 0) tile_entry <- entries  # fallback: use first

# Extract download URL (link with rel = "http://esipfed.org/ns/fedsearch/1.1/data#")
get_dl_url <- function(entry) {
  links <- entry$links
  data_links <- Filter(function(l) grepl("data#|download", l$rel, ignore.case=TRUE), links)
  h5_links   <- Filter(function(l) grepl("\\.h5", l$href), data_links)
  if (length(h5_links) > 0) return(h5_links[[1]]$href)
  if (length(data_links) > 0) return(data_links[[1]]$href)
  NULL
}

dl_url <- get_dl_url(tile_entry[[1]])
if (is.null(dl_url)) {
  # Print available links for debugging
  message("Available links:")
  for (l in tile_entry[[1]]$links) message("  ", l$rel, " -> ", l$href)
  stop("[P1C] Could not find data download URL in CMR response.", call. = FALSE)
}

h5_filename <- basename(dl_url)
message("  File: ", h5_filename)
message("  URL : ", dl_url)

# -----------------------------------------------------------------------------
# 3. Download HDF5 file
# -----------------------------------------------------------------------------
h5_dest <- file.path(ntl_dl_dir, h5_filename)

if (!file.exists(h5_dest)) {
  message("Downloading (this may take several minutes)...")
  tryCatch(
    httr2::request(dl_url) |>
      httr2::req_headers(Authorization = paste("Bearer", bearer)) |>
      httr2::req_timeout(600) |>
      httr2::req_perform(path = h5_dest),
    error = function(e) stop("[P1C] Download failed: ", conditionMessage(e),
                              call. = FALSE)
  )
  message("  Download complete (", round(file.size(h5_dest)/1e6, 1), " MB)")
} else {
  message("HDF5 file already exists: ", h5_dest, " (",
          round(file.size(h5_dest)/1e6, 1), " MB)")
}

# -----------------------------------------------------------------------------
# 4. Read using GDAL HDF5 subdataset syntax
#    terra CAN read HDF5 on Windows when GDAL has the HDF5 driver (it does).
# -----------------------------------------------------------------------------
message("Reading HDF5 with GDAL subdataset syntax...")

# Use terra::describe() to get exact subdataset paths from this file,
# then select AllAngle_Composite_Snow_Free (best NTL layer for East Africa).
# Note: describe() returns full GDAL path strings in the 'name' column.
sds  <- terra::describe(h5_dest, sds = TRUE)
ntl_row <- sds[grepl("AllAngle_Composite_Snow_Free$", sds$name), ]
if (nrow(ntl_row) == 0) {
  ntl_row <- sds[grepl("Snow_Free$", sds$name), ]
}
if (nrow(ntl_row) == 0) {
  message("Available datasets:"); print(sds$name)
  stop("[P1C] Cannot find AllAngle_Composite_Snow_Free layer.", call. = FALSE)
}

ds_path <- ntl_row$name[1]
message("  Reading subdataset: ", basename(ds_path))

ntl_raw <- tryCatch(
  terra::rast(ds_path),
  error = function(e) stop("[P1C] rast() failed on subdataset: ",
                            conditionMessage(e), call. = FALSE)
)

# -----------------------------------------------------------------------------
# 5. Reproject, crop, clean, and save
# -----------------------------------------------------------------------------
message("Reprojecting to WGS84 and cropping to AOI...")

# VNP46A4 is in sinusoidal projection — reproject to WGS84
ntl_wgs84 <- terra::project(ntl_raw, CRS_GEO)

# Crop to download AOI
aoi_ext  <- terra::ext(AOI_BBOX_DL["xmin"], AOI_BBOX_DL["xmax"],
                       AOI_BBOX_DL["ymin"], AOI_BBOX_DL["ymax"])
ntl_crop <- tryCatch(terra::crop(ntl_wgs84, aoi_ext), error = function(e) ntl_wgs84)

# Replace fill value (65535 = no-data) and apply scale factor (0.1)
ntl_crop <- terra::ifel(ntl_crop >= 65535, NA, ntl_crop * 0.1)
names(ntl_crop) <- paste0("ntl_", NTL_BASELINE_YEAR)

terra::writeRaster(ntl_crop, here(PATH_NTL), overwrite = TRUE,
                   gdal = c("COMPRESS=LZW"))

message(
  "\nNTL saved:",
  "\n  Year       : ", NTL_BASELINE_YEAR,
  "\n  Min / Max  : ",
  round(terra::global(ntl_crop, "min", na.rm=TRUE)[[1]], 3), " / ",
  round(terra::global(ntl_crop, "max", na.rm=TRUE)[[1]], 3), " nW/cm²/sr",
  "\n  Resolution : ", paste(round(terra::res(ntl_crop) * 111000, 0), collapse=" x "), " m (approx)",
  "\n  Output     : ", PATH_NTL
)

message("\n=== P1C: NTL complete ===\n")
