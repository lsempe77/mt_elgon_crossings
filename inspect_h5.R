library(terra)
library(here)
source(here("R/utils/constants.R"))

h5_files <- list.files(here("data/raw/ntl/h5"), pattern=".h5", full.names=TRUE)
cat("H5 files found:", length(h5_files), "\n")
if (length(h5_files) == 0) stop("No h5 files found")

h5_path <- h5_files[1]
cat("Inspecting:", basename(h5_path), "\n\n")

# List subdatasets
cat("=== terra::describe(sds=TRUE) ===\n")
sds <- terra::describe(h5_path, sds=TRUE)
print(sds)
cat("\n")

# Try opening specific layers
cat("=== Attempting to open each subdataset ===\n")
for (i in seq_len(min(5, nrow(sds)))) {
  id <- sds[i, "id"]
  cat("Trying:", id, "\n")
  r <- tryCatch(terra::rast(id), error=function(e) paste("FAIL:", conditionMessage(e)))
  if (inherits(r, "SpatRaster")) {
    cat("  SUCCESS - dims:", nrow(r), "x", ncol(r), "CRS:", terra::crs(r, describe=TRUE)$name, "\n")
  } else {
    cat("  ", r, "\n")
  }
}
