############### questo si potrebbe pensare come un nuovo script: modelli lat e climate


###modelli lat e climate dei median r_obs 


wc_dir <- "03.Data/in/wc2.1"

bio <- geodata::worldclim_global(var = "bio", res = 2.5, path = wc_dir)

names(bio) <- paste0("bio", 1:nlyr(bio))
bioT <- bio[["bio1"]]
bioP <- bio[["bio12"]]
bioTs <- bio[["bio4"]]
bioPs <- bio[["bio15"]]
bio_stk <- c(bioT, bioP, bioTs, bioPs)

clim_means <- terra::extract(
  bio_stk,
  ecoregions,
  fun = median,
  na.rm = TRUE)

#ecoregions <- vect("03.Data/in/Ecoregions_new/Ecoregions2017.dbf")

clim_means$ECO_NAME <- ecoregions$ECO_NAME

centr <- terra::centroids(ecoregions)
centr_xy <- terra::crds(centr)
lat <- data.frame(centr_xy)$y
clim_means$lat <- lat

#df_null <- readRDS("03.Data/out/df_null_final.RDS")

ddf <- merge(df_null, clim_means, by.x = "ECO_NAME", by.y = "ECO_NAME", all.x = TRUE)

#remove ID column
ddf <- ddf[,-13]

ddf <- ddf|>
  mutate(hemi = case_when(
    lat > 0 ~ "N",
    lat < 0 ~ "S",
  ))

#saveRDS(ddf, "03.Data/out/df_null_clim.RDS")


#distributions climatic variables
#############################


## geom density latitude
distr_lat <- ggplot(ddf, aes(x = lat)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "latitude",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(ddf$lat, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) 


### geom density median mean annual temperature
ggplot(ddf, aes(x = bio1)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Mean annual temperature",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(ddf$bio1, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) 


### geom density median temperature seasonality
ggplot(ddf, aes(x = bio4)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Temperature seasonality",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(ddf$bio4, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) 


## geom density median annual precipitation
ggplot(ddf, aes(x = bio12)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Annual precipitation",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(ddf$bio12, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) 


## geom density meadian precipitation seasonality
ggplot(ddf, aes(x = bio15)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Precipitation seasonality",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(ddf$bio15, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) 

r_lat <- ddf |>
  ggplot(aes(x = lat, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "latitude",
    y = "Median Spearman's r"
  ) +
  theme_minimal(base_size = 13)



# scatterplot climatic variables

try <- ddf |> filter(ECO_NAME != "Rock and Ice")

r_latns <- try |>
  ggplot(aes(x = lat, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "lm", color = "#2c5f8a", se = TRUE) +
  facet_wrap(~ hemi, scales = "free_x") +
  labs(
    x = "latitude",
    y = "Median spearman's r"
  ) +
  theme_minimal(base_size = 13)

r_lat / r_latns

ggsave(
  filename = "plots/ecorr_null_final/scatt_r_lat.png",
  plot = r_lat / r_latns,
  width = 13,
  height = 10,
  dpi = 300)
##################

## mean annual temperature
################
r_mat <- ddf |>
  ggplot(aes(x = bio1, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "Mean Annual Temperature",
    y = "Median spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_matns <- ddf |>
  ggplot(aes(x = bio1, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  facet_wrap(~ hemi, scales = "free_x") +
  labs(
    x = "Mean Annual Temperature",
    y = "Median spearman's r"
  ) +
  theme_minimal(base_size = 13)

r_mat / r_matns

ggsave(
  filename = "plots/ecorr_null_final/scatt_r_bio1.png",
  plot = r_mat / r_matns,
  width = 8,
  height = 5,
  dpi = 300)
#####################

## Temperature seasonality
################
r_ts <- ddf |>
  ggplot(aes(x = bio4, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "Temperature seasonality",
    y = "Median spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_tsns <- ddf |>
  ggplot(aes(x = bio4, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  facet_wrap(~ hemi, scales = "free_x") +
  labs(
    x = "Temperature seasonality",
    y = "Median spearman's r"
  ) +
  theme_minimal(base_size = 13)
r_ts / r_tsns

ggsave(
  filename = "plots/ecorr_null_final/scatt_r_bio4.png",
  plot = r_ts / r_tsns,
  width = 8,
  height = 5,
  dpi = 300)

################

## Annual precipitation
###############
r_ap <- ddf |>
  ggplot(aes(x = bio12, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "Annual precipitation",
    y = "Median spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_apns <- ddf |>
  ggplot(aes(x = bio12, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  facet_wrap(~ hemi, scales = "free_x") +
  labs(
    x = "Annual precipitation",
    y = "Median spearman's r"
  ) +
  theme_minimal(base_size = 13)

r_ap / r_apns

ggsave(
  filename = "plots/ecorr_null_final/scatt_r_bio12.png",
  plot = r_ap / r_apns,
  width = 8,
  height = 5,
  dpi = 300)

####################

## Precipitation seasonality
##############
r_ps <- ddf |>
  ggplot(aes(x = bio15, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "Precipitation seasonality",
    y = "Median spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_psns <- ddf |>
  ggplot(aes(x = bio15, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  facet_wrap(~ hemi, scales = "free_x") +
  labs(
    x = "Precipitation seasonality",
    y = "Median spearman's r"
  ) +
  theme_minimal(base_size = 13)
r_ps / r_psns

ggsave(
  filename = "plots/ecorr_raw_q095/scatt_r_bio15.png",
  plot = r_ps / r_psns,
  width = 8,
  height = 5,
  dpi = 300)
###############



