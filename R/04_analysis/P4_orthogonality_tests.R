# =============================================================================
# P4_orthogonality_tests.R
# PHASE 4 — Empirical test of the orthogonality assumption
#
# Runs after both P3A and P3B are complete.
#
# Question: Are river engineering difficulty characteristics (slope, catchment
# area, stream order, flood frequency) systematically correlated with
# socioeconomic outcomes (nighttime lights, population, road density,
# travel time, agricultural suitability)?
#
# If YES  → the exclusion restriction is under pressure; river physical
#           characteristics predict outcomes directly, not only through
#           bridge placement.
# If NO   → orthogonality is supported; the feasibility assessment
#           produces as-good-as-random variation in bridge eligibility.
#
# Tests:
#   A — Bivariate: each difficulty proxy vs each outcome
#   B — Conditional on geography (elevation, distance to Kampala)
#   C — Composite difficulty index (PCA first component) vs all outcomes
#   D — Spatial autocorrelation (Moran's I) on residuals
#   E — Correlation heatmap + coefficient plot
#
# Outputs:
#   outputs/analysis/analysis_dataset.csv
#   outputs/analysis/orthogonality_results.rds
#   outputs/report/  (figures saved as PNG)
# =============================================================================

library(here)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(spdep)
library(ggplot2)
library(patchwork)
library(modelsummary)

source(here("R/utils/constants.R"))
source(here("R/utils/io_helpers.R"))
source(here("R/utils/spatial_helpers.R"))

message("=== P4: Orthogonality tests ===")

.check_input(PATH_CROSSINGS)
.check_input(PATH_HYDRO_COV)
.check_input(PATH_SOCIO_COV)

dir.create(here("outputs/report"), showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. Build analysis dataset
# -----------------------------------------------------------------------------
message("\n[Step 1/5] Building analysis dataset...")

crossings <- read_crossings() |> sf::st_transform(CRS_PROJ)
hydro     <- read_hydro_covariates()
socio     <- read_socio_outcomes()

# Join on crossing_id
df_geo <- crossings |>
  dplyr::left_join(hydro, by = "crossing_id") |>
  dplyr::left_join(socio, by = "crossing_id")

# Distance to Kampala (geographic control)
# Use exact CRS from df_geo to avoid WKT mismatch in st_distance
kampala <- sf::st_sfc(sf::st_point(c(32.582, 0.317)),
                      crs = sf::st_crs(4326)) |>
  sf::st_transform(sf::st_crs(df_geo))

df_geo$dist_kampala_km <- as.numeric(
  sf::st_distance(df_geo, kampala)
) / 1000

# Log transforms (applied to right-skewed variables)
df <- sf::st_drop_geometry(df_geo) |>
  dplyr::mutate(
    log_ntl_2km       = log1p(ntl_2km),
    log_pop_2km       = log1p(pop_2km),
    log_catchment     = log1p(catchment_area_km2),
    log_flood_Q50     = log1p(flood_Q50),
    log_road_dens     = log1p(road_dens_2km),
    log_travel_time   = log1p(travel_time_min),
    dist_kampala_km_s = scale(dist_kampala_km)[, 1],
    elev_m_s          = scale(elev_m)[, 1],
    # Wealth proxy (Meta RWI): roughly N(0,1), no log transform needed
    rwi_2km           = rwi_2km,
    # Child dependency ratio: clamp to valid (0,1) before logit
    # (values can slightly exceed 1 due to resolution mismatch between age and total pop rasters)
    dep_ratio_clamped = pmin(pmax(dep_ratio_2km, 0.001), 0.999),
    dep_ratio_logit   = dplyr::if_else(
      !is.na(dep_ratio_clamped),
      log(dep_ratio_clamped / (1 - dep_ratio_clamped)),
      NA_real_
    )
  )

write_analysis_dataset(df, script = "P4_orthogonality_tests.R")

message("  Rows in analysis dataset: ", nrow(df))
message("  Complete cases: ", sum(complete.cases(
  df[, c("slope_deg","catchment_area_km2","stream_order","flood_Q50",
         "ntl_2km","pop_2km","road_dens_2km","travel_time_min",
         "rwi_2km","dep_ratio_2km")]
)))

# -----------------------------------------------------------------------------
# 2. PCA difficulty index (Test C)
# Reduces the four difficulty proxies to a single first component.
# Sign convention: higher index = harder to bridge.
# -----------------------------------------------------------------------------
message("\n[Step 2/5] Computing PCA difficulty index...")

diff_vars <- c("slope_deg", "log_catchment", "stream_order", "log_flood_Q50")
diff_mat  <- df |>
  dplyr::select(all_of(diff_vars)) |>
  dplyr::filter(complete.cases(df[, diff_vars])) |>
  as.matrix()

pca <- prcomp(diff_mat, scale. = TRUE)

# Ensure PC1 is positively oriented (more = harder)
loadings_pc1 <- pca$rotation[, 1]
if (mean(loadings_pc1) < 0) {
  pca$rotation[, 1] <- -pca$rotation[, 1]
  pca$x[, 1]        <- -pca$x[, 1]
}

df$diff_index_pc1 <- NA_real_
complete_rows <- complete.cases(df[, diff_vars])
df$diff_index_pc1[complete_rows] <- pca$x[, 1]

pct_variance <- round(summary(pca)$importance[2, 1] * 100, 1)

# Sync computed columns back to df_geo for spatial autocorrelation step
df_geo <- df_geo |>
  dplyr::left_join(
    dplyr::select(df, crossing_id, diff_index_pc1,
                  log_ntl_2km, log_pop_2km, log_road_dens, log_travel_time,
                  elev_m_s, dist_kampala_km_s,
                  rwi_2km, dep_ratio_logit),
    by = "crossing_id"
  )
message("  PC1 explains ", pct_variance, "% of difficulty variance")
message("  Loadings: ", paste(names(loadings_pc1), round(loadings_pc1, 2),
                               sep="=", collapse=", "))

# -----------------------------------------------------------------------------
# 3. Define variables for tests
# -----------------------------------------------------------------------------
difficulty_vars <- c(
  "slope_deg",
  "log_catchment",
  "stream_order",
  "log_flood_Q50",
  "diff_index_pc1"
)

outcome_vars <- c(
  "log_ntl_2km",
  "log_pop_2km",
  "log_road_dens",
  "log_travel_time",
  "agri_suit_2km",
  "rwi_2km",
  "dep_ratio_logit"
)

outcome_labels <- c(
  log_ntl_2km      = "Nighttime lights (log, 2km)",
  log_pop_2km      = "Population density (log, 2km)",
  log_road_dens    = "Road density (log, 2km)",
  log_travel_time  = "Travel time to city (log)",
  agri_suit_2km    = "Agricultural suitability (2km)",
  rwi_2km          = "Relative Wealth Index (Meta, 2km)",
  dep_ratio_logit  = "Child dependency ratio (logit)"
)

difficulty_labels <- c(
  slope_deg        = "Slope (degrees)",
  log_catchment    = "Catchment area (log km²)",
  stream_order     = "Stream order (Strahler)",
  log_flood_Q50    = "Flood Q50 (log m³/s)",
  diff_index_pc1   = "Difficulty index (PC1)"
)

# -----------------------------------------------------------------------------
# 4. Test A — Bivariate regressions
# outcome_i = α + β · difficulty_i + ε
# One regression per (difficulty var, outcome var) combination.
# -----------------------------------------------------------------------------
message("\n[Step 3/5] Running bivariate regressions (Test A)...")

results_bivariate <- purrr::map_dfr(difficulty_vars, function(dv) {
  purrr::map_dfr(outcome_vars, function(ov) {
    dat <- df[, c(dv, ov)] |> tidyr::drop_na()
    if (nrow(dat) < 20) return(NULL)
    m <- lm(reformulate(dv, response = ov), data = dat)
    broom::tidy(m, conf.int = TRUE) |>
      dplyr::filter(term == dv) |>
      dplyr::mutate(
        difficulty   = dv,
        outcome      = ov,
        n            = nrow(dat),
        r_squared    = summary(m)$r.squared,
        test         = "bivariate"
      )
  })
})

# -----------------------------------------------------------------------------
# 5. Test B — Conditional on geography
# outcome_i = α + β · difficulty_i + γ₁·elev_m + γ₂·dist_kampala + ε
# β≈0 after conditioning = instrument valid conditional on geography.
# -----------------------------------------------------------------------------
message("[Step 3/5] Running geography-conditional regressions (Test B)...")

results_conditional <- purrr::map_dfr(difficulty_vars, function(dv) {
  purrr::map_dfr(outcome_vars, function(ov) {
    dat <- df[, c(dv, ov, "elev_m_s", "dist_kampala_km_s")] |> tidyr::drop_na()
    if (nrow(dat) < 20) return(NULL)
    m <- lm(reformulate(c(dv, "elev_m_s", "dist_kampala_km_s"), response = ov),
            data = dat)
    broom::tidy(m, conf.int = TRUE) |>
      dplyr::filter(term == dv) |>
      dplyr::mutate(
        difficulty   = dv,
        outcome      = ov,
        n            = nrow(dat),
        r_squared    = summary(m)$r.squared,
        test         = "conditional_on_geo"
      )
  })
})

# Bind all regression results
results_all <- dplyr::bind_rows(results_bivariate, results_conditional)

# Add significance flags
results_all <- results_all |>
  dplyr::mutate(
    sig_05  = p.value < 0.05,
    sig_01  = p.value < 0.01,
    sig_001 = p.value < 0.001
  )

# -----------------------------------------------------------------------------
# 6. Test C — Spatial autocorrelation (Moran's I on residuals)
# If residuals from bivariate regressions cluster spatially, there are
# omitted spatial confounders.
# -----------------------------------------------------------------------------
message("\n[Step 4/5] Spatial autocorrelation tests (Test C)...")

# Build spatial weights from crossing geometry
df_geo_complete <- df_geo |>
  dplyr::filter(!is.na(diff_index_pc1) & !is.na(log_ntl_2km))

w <- tryCatch(
  build_knn_weights(df_geo_complete, k = KNN_NEIGHBORS),
  error = function(e) {
    warning("[P4] Could not build spatial weights: ", conditionMessage(e))
    NULL
  }
)

moran_results <- list()

if (!is.null(w)) {
  # Test on residuals from bivariate difficulty index ~ NTL
  m_ntl <- lm(log_ntl_2km ~ diff_index_pc1, data = df_geo_complete)
  moran_results$ntl_bivariate <- spdep::moran.test(residuals(m_ntl), w)

  # Test on residuals from conditional model
  df_geo_c <- df_geo_complete |>
    dplyr::filter(!is.na(elev_m_s) & !is.na(dist_kampala_km))

  if (nrow(df_geo_c) > KNN_NEIGHBORS + 5) {
    w2 <- build_knn_weights(df_geo_c, k = KNN_NEIGHBORS)
    m_ntl_cond <- lm(log_ntl_2km ~ diff_index_pc1 + elev_m_s +
                       dist_kampala_km_s,
                     data = sf::st_drop_geometry(df_geo_c))
    moran_results$ntl_conditional <- spdep::moran.test(
      residuals(m_ntl_cond), w2
    )
  }

  # Print results
  purrr::walk2(moran_results, names(moran_results), function(mt, nm) {
    message(sprintf("  Moran I [%s]: I=%.3f, p=%.4f",
                    nm, mt$estimate[1], mt$p.value))
  })
} else {
  message("  Skipped — could not build spatial weights.")
}

# -----------------------------------------------------------------------------
# 7. Save results
# -----------------------------------------------------------------------------
message("\n[Step 5/5] Saving results and figures...")

results_list <- list(
  regression_bivariate    = results_bivariate,
  regression_conditional  = results_conditional,
  regression_all          = results_all,
  pca_difficulty          = pca,
  pca_pct_variance        = pct_variance,
  moran                   = moran_results,
  metadata = list(
    n_crossings   = nrow(df),
    buffer_small_m = BUFFER_SMALL_M,
    buffer_large_m = BUFFER_LARGE_M,
    ntl_year       = NTL_BASELINE_YEAR,
    run_date       = Sys.time()
  )
)
saveRDS(results_list, here(PATH_RESULTS))

# -----------------------------------------------------------------------------
# 8. Figures
# -----------------------------------------------------------------------------

# --- Figure 1: Coefficient plot (bivariate vs conditional) ---
coef_plot_dat <- results_all |>
  dplyr::filter(difficulty == "diff_index_pc1") |>
  dplyr::mutate(
    outcome_label = outcome_labels[outcome],
    test_label    = dplyr::if_else(test == "bivariate",
                                   "Bivariate", "Conditional on geography")
  )

fig_coef <- ggplot2::ggplot(
  coef_plot_dat,
  ggplot2::aes(x = estimate, xmin = conf.low, xmax = conf.high,
               y = outcome_label, colour = test_label, shape = sig_05)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  ggplot2::geom_errorbarh(height = 0.25, position = ggplot2::position_dodge(0.5)) +
  ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(0.5)) +
  ggplot2::scale_colour_manual(
    values = c("Bivariate" = "#E07B54", "Conditional on geography" = "#2C7BB6"),
    name   = "Model"
  ) +
  ggplot2::scale_shape_manual(
    values = c("TRUE" = 16, "FALSE" = 1),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
    name   = "Significance"
  ) +
  ggplot2::labs(
    title    = "Difficulty index (PC1) vs socioeconomic outcomes",
    subtitle = paste0("Mt. Elgon, Uganda. n=", nrow(df),
                      " crossings. β = 0 supports orthogonality."),
    x        = "Coefficient on difficulty index (PC1)",
    y        = NULL,
    caption  = "Conditional model controls for elevation and distance to Kampala."
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(here("outputs/report/fig1_coef_plot.png"),
                fig_coef, width = 8, height = 5, dpi = 150)

# --- Figure 2: Correlation heatmap ---
heat_dat <- results_all |>
  dplyr::filter(test == "bivariate") |>
  dplyr::mutate(
    difficulty_label = difficulty_labels[difficulty],
    outcome_label    = outcome_labels[outcome]
  )

fig_heat <- ggplot2::ggplot(
  heat_dat,
  ggplot2::aes(x = difficulty_label, y = outcome_label, fill = estimate)
) +
  ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
  ggplot2::geom_text(
    ggplot2::aes(label = dplyr::case_when(
      sig_001 ~ "***",
      sig_01  ~ "**",
      sig_05  ~ "*",
      TRUE    ~ ""
    )),
    size = 5, colour = "white"
  ) +
  ggplot2::scale_fill_gradient2(
    low = "#2C7BB6", mid = "white", high = "#D7191C",
    midpoint = 0, name = "β"
  ) +
  ggplot2::labs(
    title    = "Bivariate associations: difficulty proxies vs outcomes",
    subtitle = "* p<0.05  ** p<0.01  *** p<0.001",
    x        = "Difficulty proxy",
    y        = "Socioeconomic outcome"
  ) +
  ggplot2::theme_bw(base_size = 10) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

ggplot2::ggsave(here("outputs/report/fig2_heatmap.png"),
                fig_heat, width = 9, height = 5, dpi = 150)

# --- Figure 3: Scatter — difficulty index vs NTL (main test) ---
scatter_dat <- df |>
  dplyr::filter(!is.na(diff_index_pc1), !is.na(log_ntl_2km))

fig_scatter <- ggplot2::ggplot(
  scatter_dat,
  ggplot2::aes(x = diff_index_pc1, y = log_ntl_2km)
) +
  ggplot2::geom_point(alpha = 0.4, size = 1.5, colour = "#555555") +
  ggplot2::geom_smooth(method = "lm", colour = "#E07B54", se = TRUE) +
  ggplot2::geom_smooth(method = "loess", colour = "#2C7BB6",
                       linetype = "dashed", se = FALSE) +
  ggplot2::labs(
    title    = "Difficulty index vs nighttime lights (2014)",
    subtitle = "Orange = linear fit; blue dashed = LOESS",
    x        = "Difficulty index (PC1 of slope, catchment, order, flood)",
    y        = "Nighttime lights, log(1+NTL), 2km buffer"
  ) +
  ggplot2::theme_bw(base_size = 11)

ggplot2::ggsave(here("outputs/report/fig3_scatter_ntl.png"),
                fig_scatter, width = 7, height = 5, dpi = 150)

# -----------------------------------------------------------------------------
# 9. Print interpretation guide
# -----------------------------------------------------------------------------
sig_bivariate <- results_all |>
  dplyr::filter(test == "bivariate", difficulty == "diff_index_pc1", sig_05)

sig_conditional <- results_all |>
  dplyr::filter(test == "conditional_on_geo", difficulty == "diff_index_pc1", sig_05)

message(
  "\n===== ORTHOGONALITY TEST RESULTS =====",
  "\n",
  "\n  Bivariate: difficulty index significant for ",
  nrow(sig_bivariate), "/", length(outcome_vars), " outcomes",
  "\n  Conditional: difficulty index significant for ",
  nrow(sig_conditional), "/", length(outcome_vars), " outcomes (after geo controls)",
  "\n",
  "\n  INTERPRETATION:",
  "\n  - 0/", length(outcome_vars), " significant (conditional): STRONG support for orthogonality",
  "\n  - 1-2/", length(outcome_vars), " significant (conditional): MODERATE — condition on geography in IV first stage",
  "\n  - 3+/", length(outcome_vars), " significant (conditional): WEAK — exclusion restriction under pressure",
  "\n",
  "\n  Figures saved to: outputs/report/",
  "\n  Full results:     outputs/analysis/orthogonality_results.rds",
  "\n========================================\n"
)

message("=== P4 complete ===\n")
