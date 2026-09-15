#### null model and standard effect size on r_trimmed
# I will perform a within-ecoregion permutation null model, with a cap on n_pixels at 1k pixels. 

library(dplyr)
library(tidyterra)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggnewscale)
library(sf)
library(terra)
library(biscale)



df_for_final <- readRDS("03.Data/out/df_for_final.RDS")

#trimming q2.5 upper and lower quantile of AD and AGB
trim <- 0.025
df_trimmed <- df_for_final |>
  group_by(ECO_NAME) |>
  filter(AD >= quantile(AD, trim, na.rm = T),
         AD <= quantile(AD, 1 - trim, na.rm = T),
         AGB >= quantile(AGB, trim, na.rm = T),
         AGB <= quantile(AGB, 1- trim, na.rm = T)) |>
  ungroup()




# function for the observed and null model iterations on 1K pixels cap
# WITHIN ECOREGION PERMUTATION
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
    shuffled$AD <- sample(shuffled$AD, replace = FALSE)
    if (n_pixels < cap){
      d <- shuffled[sample.int(n_pixels, cap, replace = TRUE), ]
    } else {
      d <- shuffled[sample.int(n_pixels, cap, replace = FALSE), ]
    }
    
    cor(d$AD, d$logAGB, 
        method = "spearman", 
        use = "complete.obs")
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

saveRDS(df_null, "03.Data/out/df_null_within_eco.RDS")





### GLOBAL PERMUTATION
ecorr_null_global <- function(data, n_perm = 99, cap = 1000, seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Split row indices by ecoregion
  eco_idx <- split(
    seq_len(nrow(data)),
    data$ECO_NAME,
    drop = TRUE
  )
  
  eco_names <- names(eco_idx)
  n_pixels  <- lengths(eco_idx)
  
  
  # Helper: sampling strategy within each ecoregion
  sample_eco <- function(idx) {
    
    n <- length(idx)
    
    sample(
      idx,
      size = cap,
      replace = n < cap
    )
  }
  
  
  # ==========================================================
  # 1. OBSERVED DISTRIBUTIONS
  # ==========================================================
  
  r_obs_dist <- lapply(
    eco_idx,
    function(idx) {
      
      replicate(
        n_perm,
        {
          
          sampled_idx <- sample_eco(idx)
          
          cor(
            data$AD[sampled_idx],
            data$logAGB[sampled_idx],
            method = "spearman",
            use = "complete.obs"
          )
        }
      )
    }
  )
  
  
  # ==========================================================
  # 2. GLOBAL NULL DISTRIBUTIONS
  # ==========================================================
  
  # Empty vectors for each ecoregion
  null_dist <- lapply(
    eco_idx,
    function(x) numeric(n_perm)
  )
  
  
  for (p in seq_len(n_perm)) {
    
    # ----------------------------------------------
    # ONE global permutation for permutation p
    # ----------------------------------------------
    
    AD_perm <- sample(
      data$AD,
      size = nrow(data),
      replace = FALSE
    )
    
    
    # ----------------------------------------------
    # Use SAME global permutation for every ecoregion
    # ----------------------------------------------
    
    for (e in seq_along(eco_idx)) {
      
      idx <- eco_idx[[e]]
      
      sampled_idx <- sample_eco(idx)
      
      null_dist[[e]][p] <- cor(
        AD_perm[sampled_idx],
        data$logAGB[sampled_idx],
        method = "spearman",
        use = "complete.obs"
      )
    }
  }
  
  
  # ==========================================================
  # 3. SUMMARISE
  # ==========================================================
  
  r_obs_median <- vapply(
    r_obs_dist,
    median,
    numeric(1),
    na.rm = TRUE
  )
  
  r_obs_mean <- vapply(
    r_obs_dist,
    mean,
    numeric(1),
    na.rm = TRUE
  )
  
  r_obs_sd <- vapply(
    r_obs_dist,
    sd,
    numeric(1),
    na.rm = TRUE
  )
  
  
  null_median <- vapply(
    null_dist,
    median,
    numeric(1),
    na.rm = TRUE
  )
  
  null_mean <- vapply(
    null_dist,
    mean,
    numeric(1),
    na.rm = TRUE
  )
  
  null_sd <- vapply(
    null_dist,
    sd,
    numeric(1),
    na.rm = TRUE
  )
  
  
  # ==========================================================
  # 4. OUTPUT
  # ==========================================================
  
  tibble(
    ECO_NAME = eco_names,
    
    n_pixels = unname(n_pixels),
    
    # TRUE = bootstrap with replacement because n < cap
    resampled = unname(n_pixels) < cap,
    
    r_obs_median = unname(r_obs_median),
    
    null_median = unname(null_median),
    null_sd     = unname(null_sd),
    
    SES_r = (
      unname(r_obs_median) -
        unname(null_median)
    ) / unname(null_sd),
    
    r_obs_mean = unname(r_obs_mean),
    r_obs_sd   = unname(r_obs_sd),
    
    null_mean = unname(null_mean),
    
    r_obs_dist = unname(r_obs_dist),
    null_dist  = unname(null_dist)
  )
}

df_null_global <- ecorr_null_global(
  data = df_trimmed,
  n_perm = 99,
  cap = 1000,
  seed = 123
)

saveRDS(df_null_global, "03.Data/out/df_null_global.RDS")





# calculate moran's I ithin ecoregions
#install.packages("spdep", repos = "https://cloud.r-project.org", type = "source")
library(spdep)
library(terra)

moran_results <- df_trimmed |>
  group_by(ECO_NAME) |>
  group_modify(~ {
    
    eco <- .x |>
      filter(!duplicated(pick(x, y))) |>
      slice_sample(n = min(2000, nrow(.x)))
    
    if (nrow(eco) < 10) {
      return(tibble(moran_I = NA_real_, n = nrow(eco)))
    }
    
    coords <- as.matrix(eco[, c("x", "y")])
    
    nb <- tryCatch(
      knn2nb(knearneigh(coords, k = 8)),
      error = function(e) NULL
    )
    
    if (is.null(nb)) return(tibble(moran_I = NA_real_, n = nrow(eco)))
    
    lw <- nb2listw(nb, style = "W")
    
    mt <- tryCatch(
      moran.test(eco$AD, lw),
      error = function(e) NULL
    )
    
    if (is.null(mt)) return(tibble(moran_I = NA_real_, n = nrow(eco)))
    
    tibble(moran_I = as.numeric(mt$estimate["Moran I statistic"]),
           n = nrow(eco))
  }) |>
  ungroup()

moran_results |>
  summarise(
    mean   = mean(moran_I,   na.rm = TRUE),
    median = median(moran_I, na.rm = TRUE),
    sd     = sd(moran_I,     na.rm = TRUE),
    min    = min(moran_I,    na.rm = TRUE),
    max    = max(moran_I,    na.rm = TRUE),
    n_NA   = sum(is.na(moran_I))
  )

library(ggplot2)

ggplot(moran_results, aes(x = moran_I)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  geom_vline(xintercept = median(moran_results$moran_I), 
             color = "red", linetype = "dashed") +
  labs(x = "Moran's I (within-ecoregion, species richness)",
       y = "N ecoregions") +
  theme_minimal()

moran_results |>
  left_join(df_null |> select(ECO_NAME, n_pixels), by = "ECO_NAME") |>
  ggplot(aes(x = n_pixels, y = moran_I)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  theme_minimal()

#### high spatial correlation, justifies need for spatially contrained null model with blocks






df_perc <- df_null_global |>
  filter(abs(SES_r) >= 1.96)

mean(abs(df_null_global$SES_r) >= 1.96, na.rm = TRUE) * 100 #% significant
mean(abs(df_null_global$SES_r) < 1.96, na.rm = TRUE) * 100 #% not significant
mean(df_perc$r_obs_median > 0) * 100 #% positive among significant
mean(df_perc$r_obs_median < 0) * 100 #%negative among significant
range(df_perc$r_obs_median < 0)

# distribution of r_obs and r_null per ecoregion, pdf ----
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
##----


## distribution of ecoregion's SES_r, r_obs & ecoregions map  r_obs significant ----
##########################
#----
distr_SES <- ggplot(df_null, aes(x = SES_r)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "SES of ecoregions' Spearman r",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_null$SES_r, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
           label = paste0("n (ecoregions) = ", nrow(df_null), "\nmedian = ", 
                          round(median(df_null$SES_r, na.rm = TRUE), 2)),
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

distr_robs <- ggplot(df_null, aes(x = r_obs_median)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Ecoregions' median Spearman r (99 iterations, cap 1000 pixels)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_null$r_obs_median, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
           label = paste0("n (ecoregions) = ", nrow(df_null), "\nmedian = ", 
                          round(median(df_null$r_obs_median, na.rm = TRUE), 2)),
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
#----

# map
#df_null <- readRDS("03.Data/out/df_null_within_eco.RDS")
#df_null <- readRDS("03.Data/out/df_null_global.RDS")


ecoregions <- terra::vect("03.Data/in/Ecoregions_new/Ecoregions2017.shp")
df_null <- df_null |>
  filter(ECO_NAME != "Rock and Ice")

ecoregions_map <- ecoregions |>
  left_join(df_null, by = "ECO_NAME") |>
  st_as_sf()

sf::sf_use_s2(FALSE)

bbox_crop <- st_as_sfc(
  st_bbox(
    c(xmin = -180, xmax = 180, ymin = -60, ymax = 90),
    crs = st_crs(4326)))

ecoregions_crop <- ecoregions_map |>
  st_transform(4326) |>
  st_make_valid() |>
  st_intersection(bbox_crop) |>
  st_transform("+proj=robin") |>
  mutate(map_class = case_when(
    is.na(SES_r) ~ "Not forested",
    abs(SES_r) >= 1.96 ~ "Significant",
    TRUE ~ "Not significant"
  ))


biv_colors <- bi_pal(
  pal = "DkBlue2", 
  dim = 3, 
  preview = FALSE)

ses_high  <- unname(biv_colors["2-3"])  
ses_mid  <- "#f7f7f2"                 
ses_low <- unname(biv_colors["3-2"])  


ecoregions_signr_map <- ggplot() +
  geom_sf(data = filter(ecoregions_crop, map_class == "Not forested"),
                        aes(fill = "Not forested"), color = NA) +
  geom_sf(data = filter(ecoregions_crop, map_class == "Not significant"),
          aes(fill = "Not significant"), color = NA) +
  scale_fill_manual(
    name = NULL,
    values = c("Not forested" = "#d9d9d9", 
               "Not significant" = "azure4")) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = filter(ecoregions_crop, map_class == "Significant"),
          aes(fill = r_obs_median), color = NA) +
  scale_fill_gradient2(
    low = ses_low, mid = ses_mid, high = ses_high,
    midpoint = 0, limits = c(-1, 1), oob = scales::squish,
    na.value = "#d9d9d9",
    name = "Median Spearman's r",
    guide = guide_colorbar(direction = "horizontal", title.position = "top",
                           title.hjust = 0.5, barwidth = unit(6, "cm"), barheight = unit(0.35, "cm"))
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.box = "horizontal",
    legend.title = element_text(size = 8), legend.text = element_text(size = 7),
    panel.grid.major = element_line(color = "azure2", linewidth = 0.25),
    panel.grid.minor = element_blank(), axis.title = element_blank()
  ) +
  coord_sf(crs = "+proj=robin", expand = FALSE)

ecoregions_signr_map

ggsave(  filename = "plots/ecoregions_map_within.png",
         plot = ecoregions_signr_map,
         width = 8,
         height = 5,
         dpi = 300)


########### prova mappa raster non poligons

forest_mask <- rast("03.Data/out/pot_forest_mask.tif")
forest_mask[forest_mask != 1] <- NA
forest_poly <- as.polygons(forest_mask, dissolve = T, na.rm = T) |>
  st_as_sf() |>
  st_transform(st_crs(ecoregions_crop))
ecoregions_forest <- ecoregions_crop |>
  st_intersection(forest_poly)

ggplot() +
  geom_sf(
    data = ecoregions_crop,
    aes(fill = "Not forested"),
    color = NA
  ) +
  geom_sf(
    data = filter(ecoregions_forest,
                  map_class == "Not significant"),
    aes(fill = "Not significant"),
    color = NA
  ) +
  
  scale_fill_manual(
    name = NULL,
    values = c(
      "Not forested" = "#d9d9d9",
      "Not significant" = "azure4"
    )
  ) +
  
  ggnewscale::new_scale_fill() +
  
  geom_sf(
    data = filter(ecoregions_forest,
                  map_class == "Significant"),
    aes(fill = r_obs_median),
    color = NA
  ) +
  
  scale_fill_gradient2(
    low = ses_low,
    mid = ses_mid,
    high = ses_high,
    midpoint = 0,
    limits = c(-1, 1),
    oob = scales::squish,
    name = "Median Spearman's r"
  )

#####----




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



