library(mgcv)
library(mgcViz)
library(dplyr)
library(ggplot2)
library(spdep)

M0     <- readRDS("03.Data/out/GAM_global/M0.rds")
Mspace50 <- readRDS("03.Data/out/GAM_global/Mspace_k50.rds")
Mfull50  <- readRDS("03.Data/out/GAM_global/Mfull_k50.rds")


summary(M0)
summary(Mspace50)
summary(Mfull50)

k_sensitivity <- data.frame(
  k = c(50, 100, 200, 400, 800),
  beta_AGB = c(
    coef(Mfull50)["AGB"],
    coef(Mfull100)["AGB"],
    coef(Mfull200)["AGB"],
    coef(Mfull400)["AGB"],
    coef(Mfull800)["AGB"]
  ),
  dev_space = 100 * c(
    summary(Mspace50)$dev.expl,
    summary(Mspace100)$dev.expl,
    summary(Mspace200)$dev.expl,
    summary(Mspace400)$dev.expl,
    summary(Mspace800)$dev.expl
  ),
  dev_full = 100 * c(
    summary(Mfull50)$dev.expl,
    summary(Mfull100)$dev.expl,
    summary(Mfull200)$dev.expl,
    summary(Mfull400)$dev.expl,
    summary(Mfull800)$dev.expl,
  ),
  edf_full = c(
    summary(Mfull50)$s.table[1, "edf"],
    summary(Mfull100)$s.table[1, "edf"],
    summary(Mfull200)$s.table[1, "edf"],
    summary(Mfull400)$s.table[1, "edf"],
    summary(Mfull800)$s.table[1, "edf"]
  )
)

k_sensitivity

gam.check(Mspace50)
gam.check(Mfull50)

fit_raw <- fitted(Mfull50)
res_raw <- residuals(Mfull50, type = "pearson")

# campione per i grafici
set.seed(123)
ii <- sample.int(length(res_raw), 50000)

diag_raw <- data.frame(
  fitted = fit_raw[ii],
  resid = res_raw[ii]
)

# Residuals vs fitted
ggplot(diag_raw, aes(fitted, resid)) +
  geom_point(alpha = 0.15, size = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(
    x = "Fitted values",
    y = "Pearson residuals"
  ) +
  theme_classic()

stats::qqnorm(res_raw[ii], pch = 16, cex = 0.4)
stats::qqline(res_raw[ii])


df_for <- df_for |>
  dplyr::rename(
    lon = x,
    lat = y
  )

res_spatial <- data.frame(
  lon = df_for$lon,
  lat = df_for$lat,
  resid = res_raw
) |>
  mutate(
    lon_bin = floor(lon),
    lat_bin = floor(lat)
  ) |>
  group_by(lon_bin, lat_bin) |>
  summarise(
    mean_resid = mean(resid, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(
    lon_c = lon_bin + 0.5,
    lat_c = lat_bin + 0.5
  )

lim <- quantile(
  abs(res_spatial$mean_resid),
  0.98,
  na.rm = TRUE
)

ggplot(res_spatial, aes(lon_c, lat_c, fill = mean_resid)) +
  geom_raster() +
  scale_fill_gradient2(
    midpoint = 0,
    limits = c(-lim, lim),
    oob = scales::squish,
    name = "Mean Pearson\nresidual"
  ) +
  coord_quickmap() +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_classic()

############ log AD
M0log <- readRDS("03.Data/out/GAM_global/M0_log.rds")
Mspacelog800 <- readRDS("03.Data/out/GAM_global/Mspacelog_k800.rds")
Mfulllog800 <- readRDS("03.Data/out/GAM_global/Mfulllog_k800.rds")
summary(M0)
summary(Mspacelog800)
summary(Mfulllog800)

data.frame(
  k = c(400, 800),
  beta_AGB = c(
    coef(Mfulllog400)["AGB"],
    coef(Mfulllog800)["AGB"]
  ),
  edf = c(
    summary(Mfulllog400)$s.table[1, "edf"],
    summary(Mfulllog800)$s.table[1, "edf"]
  ),
  deviance_explained = 100 * c(
    summary(Mfulllog400)$dev.expl,
    summary(Mfulllog800)$dev.expl
  )
)

gam.check(Mfulllog400)


fit_raw <- fitted(Mfulllog400)
res_raw <- residuals(Mfulllog400, type = "pearson")

# campione per i grafici
set.seed(123)
ii <- sample.int(length(res_raw), 50000)

diag_raw <- data.frame(
  fitted = fit_raw[ii],
  resid = res_raw[ii]
)

# Residuals vs fitted
ggplot(diag_raw, aes(fitted, resid)) +
  geom_point(alpha = 0.15, size = 0.5) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(
    x = "Fitted values",
    y = "Pearson residuals"
  ) +
  theme_classic()

stats::qqnorm(res_raw[ii], pch = 16, cex = 0.4)
stats::qqline(res_raw[ii])


res_spatial <- data.frame(
  lon = df_for$lon,
  lat = df_for$lat,
  resid = res_raw
) |>
  mutate(
    lon_bin = floor(lon),
    lat_bin = floor(lat)
  ) |>
  group_by(lon_bin, lat_bin) |>
  summarise(
    mean_resid = mean(resid, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(
    lon_c = lon_bin + 0.5,
    lat_c = lat_bin + 0.5
  )

ggplot(res_spatial, aes(lon_c, lat_c, fill = mean_resid)) +
  geom_raster() +
  scale_fill_gradient2(midpoint = 0) +
  coord_quickmap() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Mean Pearson\nresidual"
  ) +
  theme_classic()


###
library(spdep)
# Residuals from final candidate model
res_log800 <- residuals(Mfulllog800, type = "pearson")

stopifnot(length(res_log800) == nrow(df_for))

# Aggregate residuals to 1-degree cells
res_spatial_log800 <- data.frame(
  lon = df_for$lon,
  lat = df_for$lat,
  resid = res_log800
) |>
  mutate(
    lon_bin = floor(lon),
    lat_bin = floor(lat)
  ) |>
  group_by(lon_bin, lat_bin) |>
  summarise(
    mean_resid = mean(resid, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(
    lon_c = lon_bin + 0.5,
    lat_c = lat_bin + 0.5
  )

lim <- quantile(
  abs(res_spatial_log800$mean_resid),
  0.98,
  na.rm = TRUE
)

ggplot(
  res_spatial_log800,
  aes(lon_c, lat_c, fill = mean_resid)
) +
  geom_raster() +
  scale_fill_gradient2(
    midpoint = 0,
    limits = c(-lim, lim),
    oob = scales::squish,
    name = "Mean Pearson\nresidual"
  ) +
  coord_quickmap() +
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_classic()

moran_dat_log <- res_spatial_log800 |>
  filter(
    is.finite(mean_resid),
    is.finite(lon_c),
    is.finite(lat_c)
  )

coords_log <- as.matrix(
  moran_dat_log[, c("lon_c", "lat_c")]
)

knn_log <- spdep::knearneigh(
  coords_log,
  k = 8,
  longlat = TRUE
)

nb_log <- spdep::knn2nb(knn_log)

lw_log <- spdep::nb2listw(
  nb_log,
  style = "W",
  zero.policy = TRUE
)

moran_log <- spdep::moran.test(
  moran_dat_log$mean_resid,
  lw_log,
  zero.policy = TRUE
)

moran_log


set.seed(123)

moran_perm_log <- spdep::moran.mc(
  moran_dat_log$mean_resid,
  lw_log,
  nsim = 999,
  zero.policy = TRUE
)

moran_perm_log



###
Mspacelog16 <- readRDS("03.Data/out/GAM_global/Mspacelog_k16.rds")
Mfulllog16 <- readRDS("03.Data/out/GAM_global/Mfulllog_k16.rds")

summary(Mfulllog16)
gam.check(Mfulllog16)

coef(Mfulllog16)["AGB"]

100 * (
  summary(Mfulllog16)$dev.expl -
    summary(Mspacelog16)$dev.expl
)


res_log16 <- residuals(
  Mfulllog16,
  type = "pearson"
)

res_spatial_log16 <- data.frame(
  lon = df_for$lon,
  lat = df_for$lat,
  resid = res_log16
) |>
  dplyr::mutate(
    lon_bin = floor(lon),
    lat_bin = floor(lat)
  ) |>
  dplyr::group_by(lon_bin, lat_bin) |>
  dplyr::summarise(
    mean_resid = mean(resid, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    lon_c = lon_bin + 0.5,
    lat_c = lat_bin + 0.5
  )


moran_dat16 <- res_spatial_log16 |>
  dplyr::filter(
    is.finite(mean_resid),
    is.finite(lon_c),
    is.finite(lat_c)
  )

coords16 <- as.matrix(
  moran_dat16[, c("lon_c", "lat_c")]
)

knn16 <- spdep::knearneigh(
  coords16,
  k = 8,
  longlat = TRUE
)

nb16 <- spdep::knn2nb(knn16)

lw16 <- spdep::nb2listw(
  nb16,
  style = "W",
  zero.policy = TRUE
)

spdep::moran.test(
  moran_dat16$mean_resid,
  lw16,
  zero.policy = TRUE
)



# sAGB
Mfulllog_sAGB <- readRDS("03.Data/out/GAM_global/Mfulllog_sAGB.rds")

summary(Mfulllog_sAGB)

gam.check(Mfulllog_sAGB)

plot(
  Mfulllog_sAGB,
  select = 1,
  shade = TRUE,
  rug = FALSE
)

AIC(Mfulllog16, Mfulllog_sAGB)

summary(Mfulllog16)$dev.expl
summary(Mfulllog_sAGB)$dev.expl




quantile(
  df_for$AGB,
  probs = c(0, .01, .05, .25, .50, .75, .90, .95, .975, .99, .995, .999, 1),
  na.rm = TRUE
)

# Quante celle nei valori molto elevati?
sum(df_for$AGB > 500, na.rm = TRUE)
sum(df_for$AGB > 700, na.rm = TRUE)
sum(df_for$AGB > 800, na.rm = TRUE)
sum(df_for$AGB > 900, na.rm = TRUE)

prop.table(table(cut(
  df_for$AGB,
  breaks = c(0, 100, 200, 300, 400, 500, 600,
             700, 800, 900, Inf),
  right = FALSE
)))

ggplot(df_for, aes(AGB)) +
  geom_histogram(bins = 100) +
  scale_y_log10() +
  labs(
    x = expression("AGB (Mg ha"^{-1}*")"),
    y = "Number of cells (log scale)"
  ) +
  theme_classic()


Mfulllog_sAGB10 <- readRDS("03.Data/out/GAM_global/Mfulllog_sAGB10.rds")
summary(Mfulllog_sAGB10)
gam.check(Mfulllog_sAGB10)

plot(
  Mfulllog_sAGB10,
  select = 1,
  shade = TRUE,
  rug = FALSE
)
## last try log AGB
Mfulllog_logAGB <- readRDS("03.Data/out/GAM_global/Mfulllog_logAGB.rds")
summary(Mfulllog_logAGB)

data.frame(
  model = c("Linear AGB", "log(AGB)", "Smooth AGB"),
  dev_expl = 100 * c(
    summary(Mfulllog16)$dev.expl,
    summary(Mfulllog_logAGB)$dev.expl,
    summary(Mfulllog_sAGB10)$dev.expl
  ),
  AIC = c(
    AIC(Mfulllog16),
    AIC(Mfulllog_logAGB),
    AIC(Mfulllog_sAGB10)
  )
)
plot(
  Mfulllog_sAGB10,
  select = 1,
  shade = TRUE,
  rug = FALSE,
  xlim = c(0, quantile(df_for$AGB, 0.99, na.rm = TRUE))
)


v_full <- mgcViz::getViz(Mfull)
v_space <- mgcViz::getViz(Mspace)

mgcViz::check(v_full)
b <- check1D(v_full, "AGB")

b + l_points() +
  l_densCheck()

b_space <- check2D(v_full, "lon", "lat")

sp_eff <- sm(v_full, 1)

plot(sp_eff) +
  l_fitRaster() +
  l_fitContour()