# =============================================================================
# constants.R
# Shared constants for the Mt. Elgon orthogonality project.
# Every script sources this file first. Do NOT edit values here without
# informing all team members — changes affect all downstream outputs.
# =============================================================================

# -----------------------------------------------------------------------------
# Coordinate Reference Systems
# -----------------------------------------------------------------------------
CRS_GEO  <- "EPSG:4326"   # WGS84 geographic — used for downloads, OSM queries,
                            # and final output storage (interoperability)
CRS_PROJ <- "EPSG:32636"  # WGS84 / UTM Zone 36N — used for ALL metric
                            # calculations: buffering, area, density, slope

# -----------------------------------------------------------------------------
# Area of Interest — Mt. Elgon, Uganda (WGS84)
# Covers the full drainage basin of Mt. Elgon on the Uganda side,
# including Kapchorwa, Mbale, Sironko, and Bulkwa districts.
# The 25 km download buffer (AOI_BBOX_DL) prevents DEM edge artefacts
# in hydrological processing (sink filling, flow accumulation).
# -----------------------------------------------------------------------------
AOI_BBOX <- c(xmin = 33.8, xmax = 35.2, ymin = 0.4, ymax = 2.0)

AOI_BBOX_DL <- c(           # padded for raw data downloads
  xmin = 33.55,
  xmax = 35.45,
  ymin = 0.17,
  ymax = 2.23
)

# -----------------------------------------------------------------------------
# Buffer radii for outcome extraction (metres, in CRS_PROJ)
# -----------------------------------------------------------------------------
BUFFER_SMALL_M  <-   500   # tight neighbourhood around crossing
BUFFER_LARGE_M  <-  2000   # wider economic catchment
BUFFER_DL_M     <- 25000   # padding applied to AOI for data downloads

# -----------------------------------------------------------------------------
# Analysis parameters
# -----------------------------------------------------------------------------
FLOW_ACC_THRESHOLD <- 500   # minimum flow-accumulation cells to define a stream
                             # (tune in P2 to match OSM river density)

KNN_NEIGHBORS      <- 5     # k for spatial weights matrix in Moran tests

NTL_BASELINE_YEAR  <- 2014  # pre-Fika Uganda baseline for nighttime lights

# -----------------------------------------------------------------------------
# Output file paths (relative to project root via here::here())
# All scripts should use these rather than hard-coding paths.
# -----------------------------------------------------------------------------
PATH_AOI_GPKG       <- "outputs/aoi/mt_elgon_bbox.gpkg"
PATH_AOI_RDS        <- "outputs/aoi/mt_elgon_bbox.rds"

PATH_DEM            <- "data/raw/dem/dem_mt_elgon.tif"
PATH_DEM_FILLED     <- "data/processed/hydro/dem_filled.tif"
PATH_FLOW_DIR       <- "data/processed/hydro/flow_dir.tif"
PATH_FLOW_ACC       <- "data/processed/hydro/flow_acc.tif"
PATH_STREAMS        <- "data/processed/hydro/streams_derived.tif"
PATH_STRAHLER       <- "data/processed/hydro/strahler.tif"

PATH_RIVERS_OSM     <- "data/raw/rivers/rivers_osm.gpkg"
PATH_ROADS_OSM      <- "data/raw/roads/paths_osm.gpkg"

PATH_POP            <- "data/raw/population/worldpop_mt_elgon.tif"
PATH_NTL            <- "data/raw/ntl/ntl_2014.tif"
PATH_TRAVEL_TIME    <- "data/raw/travel_time/accessibility_2015.tif"
PATH_GAEZ           <- "data/raw/landuse/gaez_rainfed_suit.tif"
PATH_CHIRPS         <- "data/raw/landuse/chirps_mean_annual_precip.tif"

PATH_CROSSINGS      <- "data/processed/crossings/crossings.gpkg"

PATH_HYDRO_COV      <- "outputs/covariates/hydro_covariates.csv"
PATH_SOCIO_COV      <- "outputs/covariates/socioeconomic_outcomes.csv"
PATH_ANALYSIS_DATA  <- "outputs/analysis/analysis_dataset.csv"
PATH_RESULTS        <- "outputs/analysis/orthogonality_results.rds"

PATH_BUILD_LOG      <- "outputs/.build_log.csv"

# Wealth / demographic proxies (Phase 1E outputs)
PATH_RWI_CSV        <- "data/raw/wealth/uga_rwi.csv"        # Meta RWI point CSV
PATH_RWI            <- "data/raw/wealth/rwi_uganda.tif"     # RWI rasterised (~2.4 km)
PATH_WORLDPOP_U15   <- "data/raw/wealth/worldpop_u15.tif"   # Under-15 pop (sum of 0-14 age groups)

# -----------------------------------------------------------------------------
# Crossing ID format
# -----------------------------------------------------------------------------
CROSSING_ID_PREFIX  <- "XNG"
CROSSING_ID_FORMAT  <- function(n) sprintf("%s-%05d", CROSSING_ID_PREFIX, n)
