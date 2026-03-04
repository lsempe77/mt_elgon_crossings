# =============================================================================
# create_gaez_proxy.R
# Creates a WorldClim-based agricultural suitability proxy for Mt. Elgon.
#
# Uses WorldClim v2.1 (1970–2000 baseline) bioclimatic variables:
#   Bio1  = Mean Annual Temperature (°C × 10)
#   Bio12 = Annual Precipitation (mm)
#
# Suitability score 0–100:
#   Temperature component: optimal 18–25°C (East African highlands rainfed crops)
#   Precipitation component: optimal 700–1500mm/yr
#   Score = sqrt(temp_score × precip_score)  → 0–100
#
# Output: data/raw/landuse/gaez_rainfed_suit.tif
# =============================================================================

library(here)
library(terra)
library(geodata)

source(here("R/utils/constants.R"))

message("Creating WorldClim-based agricultural suitability proxy...")

dl_dir <- here("data/raw/landuse/worldclim_tmp")
dir.create(dl_dir, showWarnings = FALSE, recursive = TRUE)

aoi_ext <- terra::ext(
  AOI_BBOX_DL["xmin"], AOI_BBOX_DL["xmax"],
  AOI_BBOX_DL["ymin"], AOI_BBOX_DL["ymax"]
)

# ---------------------------------------------------------------------------
# 1. Download WorldClim bioclim (country Uganda: ISO = UGA)
#    res = "2.5" (2.5 arcmin ≈ 5km) — much smaller download than tiles
# ---------------------------------------------------------------------------
message("Downloading WorldClim bioclim for Uganda (2.5 arcmin)...")
bio_uga <- tryCatch(
  geodata::worldclim_country(country = "UGA", var = "bio",
                              res = 2.5, path = dl_dir),
  error = function(e) {
    message("  Country download failed: ", conditionMessage(e))
    message("  Trying tile download (lon=34, lat=1)...")
    geodata::worldclim_tile(var = "bio", lon = 34, lat = 1,
                             res = "2.5", path = dl_dir)
  }
)

if (is.null(bio_uga)) stop("WorldClim download failed.", call. = FALSE)

# Crop to AOI
bio_crop <- terra::crop(bio_uga, aoi_ext)

# geodata::worldclim_country() already scales temperature to °C (no /10 needed)
temp_c  <- bio_crop[[1]]    # mean annual temp in °C
prec_mm <- bio_crop[[12]]   # annual precipitation in mm

# ---------------------------------------------------------------------------
# 2. Compute suitability scores (0–100)
#
# Temperature: gaussian centred on 21°C, sd=5°C
#   score = 100 * exp(-((T-21)^2)/(2*5^2))
# Precipitation: piecewise, linearly increasing 400→700mm (0→100),
#                flat 700→1500mm (100), decreasing 1500→2500mm (100→0)
# ---------------------------------------------------------------------------

# Temperature score
temp_score <- 100 * exp(-((temp_c - 21)^2) / (2 * 5^2))

# Precipitation score
prec_score <- terra::ifel(
  prec_mm < 400,  0,
  terra::ifel(
    prec_mm < 700,  (prec_mm - 400) / 300 * 100,
    terra::ifel(
      prec_mm <= 1500, 100,
      terra::ifel(
        prec_mm < 2500, (2500 - prec_mm) / 1000 * 100,
        0
      )
    )
  )
)

# Combined score: geometric mean
suit_score <- sqrt(temp_score * prec_score)
suit_score <- terra::clamp(suit_score, lower = 0, upper = 100)
names(suit_score) <- "agri_suitability_0_100"

# ---------------------------------------------------------------------------
# 3. Save
# ---------------------------------------------------------------------------
out_path <- here(PATH_GAEZ)
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
terra::writeRaster(suit_score, out_path, overwrite = TRUE,
                   gdal = c("COMPRESS=LZW"))

message(
  "\nGAEZ proxy saved:",
  "\n  Method     : WorldClim temp + precip suitability index",
  "\n  Resolution : ", paste(round(terra::res(suit_score) * 111000, 0), collapse=" x "), " m (approx)",
  "\n  Range      : ", round(terra::minmax(suit_score)[1], 1), " – ",
                       round(terra::minmax(suit_score)[2], 1),
  "\n  Mean       : ", round(terra::global(suit_score, "mean", na.rm=TRUE)[[1]], 1),
  "\n  Output     : ", PATH_GAEZ
)
message("Done.")
