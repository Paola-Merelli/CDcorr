################## check potential forest mask by Sabatini et al., 2022
AD_map <- terra::rast("03.Data/in/Sabatini_AD/w3_tile_sr1000_for.tif")
pot.for <- terra::rast("03.Data/in/Sabatini_AD/potential_forest_mask/potential_forest.tif")
crs(AD_map)
crs(pot.for)
levels(pot.for)

AD01 <- terra::ifel(!is.na(AD_map), 1, 0)
pot01 <- terra::ifel(is.na(pot.for), 0,
                     terra::ifel(pot.for > 0, 1, 0))
pot01 <- terra::project(pot01, AD01, method = "near")
plot(pot01)
plot(AD01)

compare <- AD01 + 2 * pot01
a <- freq(compare)
a <- a |> mutate(freq = (a$count/sum(a$count))*100)

plot(compare)



###############################

# explore AGB = 0
# Quanti 0
sum(df_for$AGB == 0, na.rm = TRUE)
sum(df_for$AGB == 0, na.rm = TRUE) / nrow(df_for) * 100  # percentuale

# Dove sono
df_zeros <- df_for |> filter(AGB == 0)

ggplot() +
  geom_sf(data = world, fill = "grey", color = "NA", linewidth = 0.1)+
  geom_raster(data = df_zeros, aes(x = x, y = y), fill = "yellow") +
  coord_sf(crs = 4326, expand = FALSE, ylim = c(-60, 90)) +
  labs(title = paste0("Pixels with AGB = 0  (n = ", nrow(df_zeros), ", 2.8 %)")) +
  theme_minimal(base_size = 11) +
  theme(axis.title = element_blank())


C_india <- terra::crop(C_map, terra::ext(75, 85, 10, 25))
plot(C_india)
hist(values(C_india), breaks = 100)
freq_india <- terra::freq(C_india, value = 0)
freq_india

C_list <- list.files("03.Data/in/dap.ceda.ac.uk.v6/", pattern ="*.tif$", full.names = TRUE, recursive = TRUE)
C_path <- C_list[str_detect(C_list, "SD", negate = TRUE)]
AD_map <- terra::rast("03.Data/in/Sabatini_AD/w3_tile2026.sr1000.for..tif")

vrt_file <- "agb_mosaic.vrt"
terra::vrt(C_path, filename = vrt_file, overwrite = TRUE)
C_map_orig <- terra::rast(vrt_file)
global(C_map_orig, "min", na.rm = TRUE)

C_india_orig <- terra::crop(C_map_orig, terra::ext(75, 85, 10, 25))
plot(C_india_orig)
hist(values(C_india_orig), breaks = 100)
freq_india_orig <- terra::freq(C_india_orig, value = 0)
freq_india_orig

# c'erano già nel dato originale, artefatto. bisognerebbe provare con un altro anno. per ora continuo filtrando gli 0 






##############################
# AGB SENSITIVITY ANALYSIS
# Spatial metrics, forest threshold, temporal aggregation, ecoregion correlations
##############################

library(terra)
library(data.table)
library(matrixStats)
library(ggplot2)
library(gridExtra)

##############################
# 1. Settings
##############################

input_dir <- "03.Data/out/agb_mosaic_yy"
sens_dir  <- "03.Data/out/agb_sensitivity"
AD_path   <- "03.Data/in/Sabatini_AD/w3_tile2026.sr1000.for..tif"

# REQUIRED for the final ecoregion analysis: integer ID raster on the Sabatini grid.
# Set to NA_character_ to skip that section until the raster is available.
eco_path <- NA_character_

years <- c(2007, 2010, 2015:2022)
files <- file.path(input_dir, sprintf("AGBmetrics_%d.tif", years))
metrics <- c("q85", "q90", "q95", "mean_positive", "median_positive",
             "mean_all", "median_all")
thresholds <- c(0, 0.05, 0.10, 0.20, 0.50)
forest_threshold <- 0.10
min_year_options <- c(1L, 5L, 8L, 10L)
reference_min_years <- 8L
sample_size <- 100000L
map_year <- 2017L
min_eco_cells <- 30L
seed <- 123L
n_repeats <- 12L         # year-subset repetitions
n_subsample <- 10000L    # same complete cells for every repetition

dir.create(sens_dir, recursive = TRUE, showWarnings = FALSE)
stopifnot(all(file.exists(files)), map_year %in% years,
          reference_min_years <= length(years))
AD <- rast(AD_path)[[1]]


##############################
# 2. One reproducible sample
##############################

set.seed(seed)
sample_cells <- integer(0)
while (length(sample_cells) < sample_size) {
  candidates <- sample.int(ncell(AD), sample_size)
  ad <- terra::extract(AD, candidates)[, 1]
  sample_cells <- unique(c(sample_cells, candidates[!is.na(ad)]))
}
sample_cells <- sample_cells[seq_len(sample_size)]
message("Sampled AD cells: ", length(sample_cells))

##############################
# 3. Read each annual raster ONCE
##############################

# Matrices: rows = identical sampled cells; columns = annual rasters.
# Keep all spatial metrics for the later ecoregion sensitivity.
n <- length(sample_cells)
ny <- length(years)
AGB <- setNames(lapply(metrics, function(x) matrix(NA_real_, n, ny)), metrics)
F <- matrix(NA_real_, n, ny)       # positive / valid source pixels

spatial_result <- vector("list", ny)
threshold_result <- vector("list", ny)

for (j in seq_along(years)) {
  message("Reading ", years[j])
  r <- rast(files[j])
  stopifnot(compareGeom(r[[1]], AD, stopOnError = FALSE),
            all(c(metrics, "n_positive", "n_zero") %in% names(r)))
  d <- as.data.table(terra::extract(r, sample_cells))
  valid <- d$n_positive + d$n_zero
  f <- fifelse(valid > 0, d$n_positive / valid, NA_real_)
  F[, j] <- f
  for (m in metrics) AGB[[m]][, j] <- d[[m]]
  
  threshold_result[[j]] <- rbindlist(lapply(thresholds, function(th) {
    ok <- !is.na(f) & d$n_positive > 0 & f >= th & is.finite(d$q90)
    data.table(year = years[j], threshold = th,
               pct_retained = 100 * mean(ok),
               median_q90 = if (any(ok)) median(d$q90[ok]) else NA_real_)
  }))
  ok <- !is.na(f) & f >= forest_threshold & d$n_positive > 0
  spatial_result[[j]] <- rbindlist(lapply(metrics, function(m) {
    x <- d[[m]][ok]; x <- x[is.finite(x)]
    data.table(year = years[j], metric = m,
               median_agb = if (length(x)) median(x) else NA_real_)
  }))
  rm(r, d)
}
spatial_result <- rbindlist(spatial_result)
threshold_result <- rbindlist(threshold_result)

# Annual forest mask; one positive AGB source pixel always required.
forest <- !is.na(F) & F >= forest_threshold



##############################
# 4. Spatial sensitivity figures
##############################

spatial_result[, metric := factor(metric, levels = metrics)]
p_spatial <- ggplot(spatial_result,
                    aes(year, median_agb, colour = metric, group = metric)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.6) +
  scale_x_continuous(breaks = years) +
  labs(x = "Year", y = "Median AGB (Mg/ha)", colour = "Spatial metric") +
  theme_classic()
ggsave(file.path(sens_dir, "01_spatial_metrics.png"), p_spatial,
       width = 9, height = 5.5, dpi = 300)

threshold_result[, threshold_pct := threshold * 100]
p_thr1 <- ggplot(threshold_result,
                 aes(threshold_pct, pct_retained, colour = factor(year), group = year)) +
  geom_line() + geom_point(size = 1.3) +
  scale_x_continuous(breaks = thresholds * 100) +
  labs(x = NULL, y = "Forest cells retained (%)", colour = "Year") + theme_classic()
p_thr2 <- ggplot(threshold_result,
                 aes(threshold_pct, median_q90, colour = factor(year), group = year)) +
  geom_line() + geom_point(size = 1.3) +
  scale_x_continuous(breaks = thresholds * 100) +
  labs(x = "Minimum positive AGB pixels (%)", y = "Median Q90 AGB (Mg/ha)",
       colour = "Year") + theme_classic()
# gridExtra avoids the patchwork / S7 operator issue on this server.
p_threshold <- arrangeGrob(p_thr1 + theme(legend.position = "none"), p_thr2,
                           ncol = 1)
ggsave(file.path(sens_dir, "02_forest_threshold.png"), p_threshold,
       width = 9, height = 8, dpi = 300)

##############################
# 5. Geography (full map, one year)
##############################

r <- rast(files[years == map_year])
pos <- r[["n_positive"]]
vld <- pos + r[["n_zero"]]
frac <- ifel(vld > 0, pos / vld, 0)
# Explicit 0/1 inside AD extent, NA outside it.
mask_ad <- !is.na(AD)
excluded <- ifel(mask_ad, ifel(pos <= 0 | frac < 0.10, 1, 0), NA)
lost_5_10 <- ifel(mask_ad, ifel(pos > 0 & frac >= 0.05 & frac < 0.10, 1, 0), NA)
lost_10_20 <- ifel(mask_ad, ifel(pos > 0 & frac >= 0.10 & frac < 0.20, 1, 0), NA)
geo <- terra::aggregate(c(excluded, lost_5_10, lost_10_20),
                        fact = 12, fun = "mean", na.rm = TRUE) * 100
names(geo) <- c("Excluded at 10%", "Lost: 5% to 10%", "Lost: 10% to 20%")
png(file.path(sens_dir, "03_threshold_geography.png"),
    width = 2200, height = 2600, res = 250)
par(mfrow = c(3, 1), mar = c(2, 2, 3, 4))
for (k in seq_len(nlyr(geo))) {
  plot(geo[[k]], main = names(geo)[k], range = c(0, 100),
       col = hcl.colors(100, "YlOrRd"))
}
dev.off()
rm(r, pos, vld, frac, mask_ad, excluded, lost_5_10, lost_10_20, geo)
gc(verbose = FALSE)

##############################
# 6. Stability of annual forest classification
##############################

stability <- rbindlist(lapply(c(0.05, 0.10, 0.20), function(th) {
  n_forest <- rowSums(!is.na(F) & F >= th)
  data.table(threshold = paste0(100 * th, "%"),
             years_forested = 0:ny,
             pct_cells = 100 * tabulate(n_forest + 1L, nbins = ny + 1L) / n)
}))
p_stability <- ggplot(stability, aes(years_forested, pct_cells)) +
  geom_col(fill = "darkolivegreen3") + facet_wrap(~ threshold, ncol = 1) +
  scale_x_continuous(breaks = 0:ny) +
  labs(x = "Number of years classified as forest", y = "Sampled cells (%)") +
  theme_classic()
ggsave(file.path(sens_dir, "04_forest_stability.png"), p_stability,
       width = 8, height = 8, dpi = 300)



##############################
# 7. Temporal metric and min-year sensitivity
##############################

# AGB Q90 is masked after the annual forest classification.
Q <- AGB$q90
Q[!forest] <- NA_real_
n_years <- rowSums(is.finite(Q))

# Missing cells are set to NA after matrixStats aggregation.
row_stat <- function(x, stat = c("q90", "median", "max")) {
  stat <- match.arg(stat)
  z <- switch(stat,
              q90 = matrixStats::rowQuantiles(x, probs = 0.9, na.rm = TRUE, drop = TRUE),
              median = matrixStats::rowMedians(x, na.rm = TRUE),
              max = matrixStats::rowMaxs(x, na.rm = TRUE))
  z[!is.finite(z)] <- NA_real_
  z
}
T <- data.table(n_years = n_years,
                q90 = row_stat(Q, "q90"),
                median = row_stat(Q, "median"),
                max = row_stat(Q, "max"))

minyear_result <- rbindlist(lapply(min_year_options, function(k) {
  ok <- T$n_years >= k & is.finite(T$q90)
  data.table(min_years = k, pct_retained = 100 * mean(ok),
             median_q90 = if (any(ok)) median(T$q90[ok]) else NA_real_)
}))

common <- T[n_years >= reference_min_years & is.finite(q90)]
p_temporal <- ggplot(melt(common[, .(q90, median, max)],
                          measure.vars = c("q90", "median", "max"),
                          variable.name = "metric", value.name = "AGB"),
                     aes(AGB, colour = metric)) +
  geom_density(linewidth = 0.9) + coord_cartesian(xlim = c(0, 600)) +
  labs(x = "Temporally aggregated AGB (Mg/ha)", y = "Density",
       colour = "Temporal statistic") + theme_classic()
ggsave(file.path(sens_dir, "05_temporal_statistics.png"), p_temporal,
       width = 8.5, height = 5.5, dpi = 300)

p_minyears <- ggplot(melt(minyear_result, id.vars = "min_years",
                          measure.vars = c("pct_retained", "median_q90")),
                     aes(min_years, value)) +
  geom_line() + geom_point(size = 2) +
  facet_wrap(~ variable, ncol = 1, scales = "free_y",
             labeller = as_labeller(c(pct_retained = "Forest cells retained (%)",
                                      median_q90 = "Median temporal Q90 (Mg/ha)"))) +
  scale_x_continuous(breaks = min_year_options) +
  labs(x = "Minimum number of valid years", y = NULL) + theme_classic()
ggsave(file.path(sens_dir, "06_temporal_min_years.png"), p_minyears,
       width = 8, height = 7, dpi = 300)


##############################
# 8. Q90 stability: same cells, fewer years
##############################

# Fixed pool: cells with all 10 annual Q90 estimates, one reproducible subset.
set.seed(seed + 1L)
complete <- which(n_years == ny)
stopifnot(length(complete) >= 50L)
fixed <- sample(complete, min(n_subsample, length(complete)))
Qfixed <- Q[fixed, , drop = FALSE]
reference <- row_stat(Qfixed, "q90")

subsample_results <- rbindlist(lapply(c(5L, 8L, ny), function(k) {
  rbindlist(lapply(seq_len(if (k == ny) 1L else n_repeats), function(rep_id) {
    chosen <- if (k == ny) seq_len(ny) else sort(sample.int(ny, k))
    alt <- row_stat(Qfixed[, chosen, drop = FALSE], "q90")
    data.table(n_years = k, repetition = rep_id,
               median_abs_pct_diff = median(100 * abs(alt / reference - 1)),
               rho_full = cor(alt, reference, method = "spearman"))
  }))
}))

p_subsample <- ggplot(subsample_results[n_years < ny],
                      aes(factor(n_years), median_abs_pct_diff)) +
  geom_boxplot(width = 0.4, outlier.size = 1) +
  geom_point(position = position_jitter(width = 0.09), alpha = 0.55, size = 1.5) +
  labs(x = "Number of years sampled", y = "Median absolute Q90 difference (%)") +
  theme_classic()
ggsave(file.path(sens_dir, "07_q90_year_subsampling.png"), p_subsample,
       width = 7, height = 5, dpi = 300)
message("Year-subset sensitivity (fixed cells):")
print(subsample_results[, .(median_abs_pct_diff = median(median_abs_pct_diff),
                            median_rho = median(rho_full)), by = n_years])




##############################
# 9. Ecoregion correlation sensitivity
##############################

# Baseline: spatial Q90, 10% forest threshold, temporal Q90, >=8 years.
# Compare correlations (i) on each configuration's available cells and
# (ii) on cells common to baseline and alternative (selection vs values).
# This is a descriptive Spearman screening; SES requires the null model.

if (!is.na(eco_path) && file.exists(eco_path)) {
  eco_r <- rast(eco_path)[[1]]
  stopifnot(compareGeom(eco_r, AD, stopOnError = FALSE))
  eco <- terra::extract(eco_r, sample_cells)[, 1]
  sr <- terra::extract(AD, sample_cells)[, 1]
  
  configurations <- data.table(
    config = c("baseline", "spatial_q85", "spatial_q95",
               "spatial_median_positive", "spatial_mean_all",
               "threshold_5", "threshold_20",
               "temporal_median", "temporal_max", "min_years_5", "min_years_10"),
    metric = c("q90", "q85", "q95", "median_positive", "mean_all",
               "q90", "q90", "q90", "q90", "q90", "q90"),
    threshold = c(rep(0.10, 5), 0.05, 0.20, rep(0.10, 4)),
    year_stat = c(rep("q90", 7), "median", "max", "q90", "q90"),
    min_years = c(rep(8L, 9), 5L, 10L)
  )
  
  make_config <- function(metric, threshold, year_stat, min_years) {
    x <- AGB[[metric]]
    x[is.na(F) | F < threshold] <- NA_real_
    y <- row_stat(x, year_stat)
    y[rowSums(is.finite(x)) < min_years] <- NA_real_
    y
  }
  
  estimate_rho <- function(x, y, e) {
    d <- data.table(eco = e, sr = y, agb = x)[
      !is.na(eco) & is.finite(sr) & is.finite(agb)]
    d[, .(n = .N, rho = if (.N >= min_eco_cells &&
                            uniqueN(sr) > 1L && uniqueN(agb) > 1L)
      cor(sr, agb, method = "spearman") else NA_real_), by = eco]
  }
  
  base <- T$q90
  base[T$n_years < reference_min_years] <- NA_real_
  base_full <- estimate_rho(base, sr, eco)
  
  eco_results <- rbindlist(lapply(seq_len(nrow(configurations)), function(j) {
    p <- configurations[j]
    alt <- make_config(p$metric, p$threshold, p$year_stat, p$min_years)
    alt_full <- estimate_rho(alt, sr, eco)
    # Same set of cells for baseline and alternative within each ecoregion.
    paired <- is.finite(base) & is.finite(alt)
    alt_pair <- estimate_rho(replace(alt, !paired, NA_real_), sr, eco)
    base_pair <- estimate_rho(replace(base, !paired, NA_real_), sr, eco)
    
    d <- merge(merge(base_full, alt_full, by = "eco", all = TRUE,
                     suffixes = c("_base", "_alt")),
               merge(base_pair[, .(eco, rho_pair_base = rho)],
                     alt_pair[, .(eco, rho_pair_alt = rho)],
                     by = "eco", all = TRUE), by = "eco", all = TRUE)
    d[, `:=`(config = p$config,
             delta_full = rho_alt - rho_base,
             delta_common = rho_pair_alt - rho_pair_base,
             retained_fraction = n_alt / n_base)]
    d
  }), fill = TRUE)
  
  # Only summary values are saved; no intermediate cell-level matrices.
  eco_summary <- eco_results[, .(
    n_ecoregions = sum(is.finite(delta_full)),
    median_abs_delta_full = median(abs(delta_full), na.rm = TRUE),
    median_abs_delta_common = median(abs(delta_common), na.rm = TRUE),
    pct_sign_flip = 100 * mean(sign(rho_alt) != sign(rho_base), na.rm = TRUE),
    median_retained = median(retained_fraction, na.rm = TRUE)
  ), by = config]
  print(eco_summary)
  fwrite(eco_summary, file.path(sens_dir, "ecoregion_correlation_summary.csv"))
  
  p_eco <- ggplot(eco_results[config != "baseline" & is.finite(delta_full)],
                  aes(delta_full, config)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_boxplot(outlier.size = 0.35) +
    labs(x = expression(Delta * rho[s] * " (alternative - baseline)"),
         y = "Configuration") + theme_classic()
  ggsave(file.path(sens_dir, "08_ecoregion_rho_sensitivity.png"), p_eco,
         width = 9, height = 6.5, dpi = 300)
} else {
  message("Ecoregion analysis skipped: set eco_path to an aligned integer ecoregion raster.")
}

##############################
# 10. Done
##############################

message("Completed. Figures saved in: ", sens_dir)
