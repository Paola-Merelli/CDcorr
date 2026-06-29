#### null model and standard effect size on r_trimmed
# I will perform a within-ecoregion permutation null model, with a cap on n_pixels at 1k pixels. 

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)
library(ggnewscale)

df_for_final <- readRDS("03.Data/out/df_for_final.RDS")
df_for_final <- filter(df_for_final, df_for_final$ECO_NAME != "Mediterranean dry woodlands and steppe")

trim <- 0.025
df_trimmed <- df_for_final |>
  group_by(ECO_NAME) |>
  filter(AD >= quantile(AD, trim, na.rm = T),
         AD <= quantile(AD, 1 - trim, na.rm = T),
         AGB >= quantile(AGB, trim, na.rm = T),
         AGB <= quantile(AGB, 1- trim, na.rm = T)) |>
  ungroup()

ecorr_null <- function(data, n_perm = 99, cap = 1000) {
  
  n_pixels <- nrow(data)
  use_all  <- n_pixels < cap
  
  r_obs_vec <- replicate(n_perm, {
    if (use_all) {
      d <- data[sample.int(n_pixels, cap, replace = TRUE), ]
    } else {
      d <- data[sample.int(n_pixels, cap, replace = FALSE), ]
    }
    cor(d$AD, d$logAGB, method = "spearman", use = "complete.obs")
  })
  
  r_obs_median <- median(r_obs_vec, na.rm = TRUE)
  r_obs_mean   <- mean(r_obs_vec,   na.rm = TRUE)
  r_obs_sd     <- sd(r_obs_vec,     na.rm = TRUE)
  r_obs_dist   <- list(r_obs_vec)
  
  null_vec <- replicate(n_perm, {
    shuffled    <- data
    shuffled$AD <- sample(shuffled$AD)
    d <- shuffled[sample.int(n_pixels, min(n_pixels, cap), replace = TRUE), ]
    cor(d$AD, d$logAGB, method = "spearman", use = "complete.obs")
  })
  
  tibble(
    n_pixels     = n_pixels,
    resampled    = !use_all,
    r_obs_median = r_obs_median,
    null_median  = median(null_vec, na.rm = TRUE),
    null_sd      = sd(null_vec,    na.rm = TRUE),
    SES_r        = (r_obs_median - null_median) / null_sd,
    r_obs_mean   = r_obs_mean,
    r_obs_sd     = r_obs_sd,
    null_mean    = mean(null_vec, na.rm = TRUE),
    r_obs_dist   = r_obs_dist,
    null_dist    = list(null_vec)
  )
}

set.seed(123)
df_null <- df_trimmed |>
  group_by(ECO_NAME) |>
  group_modify(~ ecorr_null(.x, n_perm = 99, cap = 1000)) |>
  ungroup()

saveRDS(df_null, "03.Data/out/df_null_final.RDS")

df_ecorR_raw <- readRDS("03.Data/out/df_ecorR_raw.RDS")

df_ecorR_null <- df_null |>
  left_join(
    df_ecorR_raw |> 
      dplyr::select(ECO_NAME, r_raw, bio1, bio12, bio4, bio15, lat, hemi),
    by = "ECO_NAME"
  ) |>
  mutate(
    significant = ifelse(SES_r > 1.96, "Positive", ifelse(SES_r < -1.96, "Negative", "Not significant"))
  )

rm(df_null, df_ecorR_raw)

df_perc <- df_ecorR_null |>
  filter(abs(SES_r) >= 1.96)

pct_sig    <- mean(abs(df_ecorR_null$SES_r) >= 1.96, na.rm = TRUE) * 100
pct_nonsig <- mean(abs(df_ecorR_null$SES_r) < 1.96, na.rm = TRUE) * 100
pct_pos    <- mean(df_perc$r_obs_median > 0) * 100
pct_neg    <- mean(df_perc$r_obs_median < 0) * 100
range(df_perc$r_obs_median < 0)

# distribution of r_obs and r_null per ecoregion, pdf
####################### 


df_dist <- df_ecorR_null |>
  dplyr::select(ECO_NAME, resampled, n_pixels, r_obs_dist, null_dist) |>
  mutate(
    r_obs_dist = lapply(r_obs_dist, as.numeric),
    null_dist  = lapply(null_dist,  as.numeric)
  ) |>
  pivot_longer(
    cols      = c(r_obs_dist, null_dist),
    names_to  = "type",
    values_to = "r"
  ) |>
  unnest(r) |>
  mutate(
    type = recode(type,
                  "r_obs_dist" = "Observed",
                  "null_dist"  = "Null"
    )
  )


ecolist <- df_ecorR_null |> 
  arrange(desc(abs(SES_r))) |> 
  pull(ECO_NAME)
#try
#ecolist <- ecolist[100:103]


pdf("rnull_robs_distributions.pdf", width = 7, height = 4)

for (eco in ecolist) {
  
  d <- filter(df_dist, ECO_NAME == eco)
  n <- unique(d$n_pixels)
  resampled <- unique(d$resampled)
  
  obs_single <- filter(d, type == "Observed")
  is_single  <- nrow(obs_single) == 1
  
  p <- ggplot(filter(d, type == "Null"), aes(x = r)) +
    geom_density(fill = "#7F77DD", alpha = 0.4, color = "#7F77DD") +
    geom_rug(alpha = 0.4, color = "#7F77DD") +
    geom_density(
      data = filter(d, type == "Observed"),
      fill = "#1D9E75", alpha = 0.4, color = "#1D9E75"
    ) +
    geom_rug(
      data = filter(d, type == "Observed"),
      alpha = 0.4, color = "#1D9E75"
    ) +
    labs(
      title    = eco,
      subtitle = paste0(
        "n pixels = ", n,
        " | resampled = ", resampled,
        " | SES r = ", round(filter(df_ecorR_null, ECO_NAME == eco)$SES_r, 2)
      ),
      x = "Spearman r",
      y = "Density"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  print(p)
}

dev.off()

################################

## distribution of ecoregion's SES_r, r_obs 
##########################

distr_SES <- ggplot(df_ecorR_null, aes(x = SES_r)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "SES of ecoregions' Spearman r",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_ecorR_null$SES_r, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
           label = paste0("n (ecoregions) = ", nrow(df_ecorR_null), "\nmedian = ", 
                          round(median(df_ecorR_null$SES_r, na.rm = TRUE), 2)),
           hjust = 1.5,
           vjust = 1.5,
           size = 4,
           color = "gray30"
  ) 

ggsave(
  filename = "plots/ecorr_null_final/distr_SES.png",
  plot = distr_SES,
  width = 12,
  height = 8,
  dpi = 300
)

distr_robs <- ggplot(df_ecorR_null, aes(x = r_obs_median)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Ecoregions' median Spearman r (99 iterations, cap 1000 pixels)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_ecorR_null$r_obs_median, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
           label = paste0("n (ecoregions) = ", nrow(df_ecorR_null), "\nmedian = ", 
                          round(median(df_ecorR_null$r_obs_median, na.rm = TRUE), 2)),
           hjust = 1.5,
           vjust = 1.5,
           size = 4,
           color = "gray30"
  ) 

ggsave(
  filename = "plots/ecorr_null_final/distr_robs.png",
  plot = distr_robs,
  width = 12,
  height = 8,
  dpi = 300
)
#######################################

### ecoregions map of SES, r_obs and r_raw significant
################################
ecoregions <- vect("03.Data/in/Ecoregions_new/Ecoregions2017.shp")

ecoregions_map <- ecoregions |>
  left_join(df_ecorR_null, by = "ECO_NAME")
sf::sf_use_s2(FALSE)
bbox_crop <- st_as_sfc(
  st_bbox(
    c(xmin = -180, xmax = 180, ymin = -60, ymax = 90),
    crs = st_crs(4326)
  )
)

ecoregions_crop <- ecoregions_map |>
  st_as_sf() |>
  st_transform(4326) |>
  st_make_valid() |>
  st_intersection(bbox_crop) |>
  st_transform("+proj=robin")

### aggiunto x usare stessi colori mappa bivariata, per poster
ses_high  <- unname(biv_colors["2-3"])  
ses_mid  <- "#f7f7f2"                 
ses_low <- unname(biv_colors["3-2"])  


ecoregions_SES_map <- ggplot() +
  geom_sf(data = ecoregions_crop, fill = "#d9d9d9", color = NA) +
  geom_sf(data = filter(ecoregions_map, significant == "Not significant"), fill = "azure4", color = NA) +
  geom_sf(data = filter(ecoregions_map, significant != "Not significant"),
          aes(fill = SES_r), color = NA) +
  scale_fill_gradient2(
    low = ses_low,
    mid = ses_mid,
    high = ses_high,
    midpoint = 0,
    limits = c(-20, 20),
    oob = scales::squish,
    na.value = "
#d9d9d9",
    name = "SES of Spearman's r",
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(6, "cm"),
      barheight = unit(0.35, "cm")
    )) +
  labs(
    title = "Standardized Effect Size (SES) of Spearman's r of SR–AGB correlation by ecoregion",
    subtitle = "99 iterations, cap 1000 pixels, significant ecoregions only"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    panel.grid.major = element_line(color = "azure2", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(color = "black"),
    plot.subtitle = element_text(color = "grey30")
  ) +
  coord_sf(crs = "+proj=robin")

ecoregions_SES_map

ggsave(
  filename = "plots/ecorr_null_final/ecoregions_SES_map.png",
  plot = ecoregions_SES_map,
  width = 12,
  height = 8,
  dpi = 300
)




ecoregions_signr_map <- ggplot() +
  geom_sf(data = ecoregions_crop, aes(fill = "Not forested"), color = NA) +
  geom_sf(data = filter(ecoregions_map, significant == "Not significant"),
          aes(fill = "Not significant"), color = NA) +
  scale_fill_manual(
    name = NULL,
    values = c("Not forested" = "#d9d9d9",
               "Not significant" = "azure4")
  ) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = filter(ecoregions_map, significant != "Not significant"),
          aes(fill = r_obs_median), color = NA) +
  scale_fill_gradient2(
    low = ses_low, mid = ses_mid, high = ses_high,
    midpoint = 0, limits = c(-1, 1), oob = scales::squish,
    na.value = "#d9d9d9",
    name = "Median Spearman's r",
    guide = guide_colorbar(
      direction = "horizontal", title.position = "top", title.hjust = 0.5,
      barwidth = unit(6, "cm"), barheight = unit(0.35, "cm")
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    panel.grid.major = element_line(color = "azure2", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(crs = "+proj=robin",
           datum = sf::st_crs(4326),
           expand = FALSE)
ecoregions_signr_map

ggsave(  filename = "plots/ecorr_null_final/ecoregions_sign_robs_map.png",
         plot = ecoregions_signr_map,
         width = 8,
         height = 5,
         dpi = 300
)

#############################################

######################### modelli lat e climate dei median r_obs 


##############
r_lat <- df_ecorR_null |>
  ggplot(aes(x = lat, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "latitude",
    y = "Median Spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_latns <- df_ecorR_null |>
  ggplot(aes(x = lat, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
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
r_mat <- df_ecorR_null |>
  ggplot(aes(x = bio1, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "Mean Annual Temperature",
    y = "Median spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_matns <- 
  df_ecorR_null |>
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
r_ts <- df_ecorR_null |>
  ggplot(aes(x = bio4, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "Temperature seasonality",
    y = "Median spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_tsns <- df_ecorR_null |>
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
r_ap <- df_ecorR_null |>
  ggplot(aes(x = bio12, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "Annual precipitation",
    y = "Median spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_apns <- df_ecorR_null |>
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
r_ps <- df_ecorR_null |>
  ggplot(aes(x = bio15, y = r_obs_median)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "Precipitation seasonality",
    y = "Median spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_psns <- df_ecorR_null |>
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


# figure per poster
### scatt climate median_r_obs 

(r_mat + r_ts) / (r_ap + r_ps)

ggsave(
  filename = "plots/ecorr_null_final/scatt_robs_climate.png",
  plot = (r_mat + r_ts) / (r_ap + r_ps),
  width = 13,
  height = 8,
  dpi = 300
)


