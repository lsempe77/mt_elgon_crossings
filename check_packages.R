pkgs <- c("here","sf","terra","geodata","osmdata","whitebox",
          "blackmarbler","dplyr","tidyr","purrr","broom",
          "spdep","ggplot2","patchwork","modelsummary",
          "malariaAtlas","readr","lwgeom")
installed <- pkgs[pkgs %in% installed.packages()[,"Package"]]
missing   <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
cat("Already installed:", paste(installed, collapse=", "), "\n")
cat("Need to install:  ", paste(missing,   collapse=", "), "\n")
