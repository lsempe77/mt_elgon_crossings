library(terra); library(here)
source(here("R/utils/constants.R"))
r <- terra::rast(here(PATH_DEM))
cat("CRS WKT (first 80 chars):", substr(terra::crs(r), 1, 80), "\n")
cat("EPSG code:", terra::crs(r, describe=TRUE)$code, "\n")
cat("Is empty:", terra::crs(r) == "", "\n")
# Try projecting
r2 <- tryCatch(terra::project(r, CRS_PROJ),
               error=function(e) paste("FAIL:", conditionMessage(e)))
if (inherits(r2, "SpatRaster")) {
  cat("Projection to UTM 36N: SUCCESS\n")
  cat("  New CRS:", terra::crs(r2, describe=TRUE)$name, "\n")
} else cat(r2, "\n")
