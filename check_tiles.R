library(terra)
tiles <- list.files("data/raw/dem/tiles", pattern=".tif", recursive=TRUE, full.names=TRUE)
cat("DEM tiles downloaded:\n")
for(t in tiles) {
  r <- terra::rast(t)
  cat(" ", basename(t), "extent:", paste(round(as.vector(terra::ext(r)), 2), collapse=" "), "\n")
}
cat("\nAOI_DL extent: 33.55 35.45 0.17 2.23\n")
