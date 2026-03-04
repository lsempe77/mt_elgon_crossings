to_install <- c("osmdata", "whitebox", "blackmarbler", "malariaAtlas")
install.packages(to_install, repos = "https://cloud.r-project.org", quiet = FALSE)

# Verify
ok      <- to_install[to_install %in% installed.packages()[,"Package"]]
failed  <- to_install[!to_install %in% installed.packages()[,"Package"]]
cat("\nInstalled OK: ", paste(ok,     collapse=", "), "\n")
cat("Failed:       ", paste(failed,  collapse=", "), "\n")
