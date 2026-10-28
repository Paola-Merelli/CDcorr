library(terra)
library(sf)
library(dplyr)
library(dplyr)
library(tidyterra)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggnewscale)
library(sf)
library(terra)
library(biscale)


AD_map <- rast("03.Data/in/Sabatini_AD/w3_tile2026.sr1000.for..tif") 
plot(AD_map)

df_for <- readRDS("03.Data/out/df_for.RDS")
vt <- vect(df_for[, c("x", "y")], geom = c("x", "y"), crs = crs(AD_map))
data_mask <- rasterize(vt, AD_map, field = 1, background = 0)
plot(data_mask)


pot_for <- rast("03.Data/in/Sabatini_AD/potential_forest_mask/potential_forest.tif")
levels(pot_for)
l <- levels(pot_for)[[1]]
m <- l[l$status_txt == "No Data", 1]
pot_mask <- classify(pot_for, cbind(m, 0), others = 1) |> terra::project(AD_map, method = "near")
res(pot_mask)
plot(pot_mask)
#writeRaster(pot_mask, "03.Data/out/pot_forest_mask.tif")



ecoregions <- vect("03.Data/in/Ecoregions_new/Ecoregions2017.dbf")
df <- as.data.frame(ecoregions)
eco_rast <- terra::rasterize(ecoregions, pot_mask, field = "ECO_NAME")

#area of each forest pixel in hectares
A_pix_ha <- cellSize(pot_mask, unit= "ha", mask = FALSE)
# total area in each ecoregion
A_eco <- zonal(A_pix_ha, eco_rast, fun = "sum", na.rm = TRUE)
# area only of forested pixels
A_for_ha <- A_pix_ha * pot_mask
#forested area in each ecoregion
A_eco_for <- zonal(A_for_ha, eco_rast, fun = "sum", na.rm = TRUE)
#calculate number of obs for each ecoregion
n_obs <- zonal(data_mask, eco_rast, fun = "sum", na.rm = TRUE)

eco_for_df <- A_eco |>
  rename(ECO_NAME = 1, total_ha = 2) |>
  left_join(A_eco_for |>
              rename(ECO_NAME = 1, forest_ha = 2),
            by = "ECO_NAME") |>
  left_join(n_obs |>
              rename(ECO_NAME = 1, n_obs = 2),
            by = "ECO_NAME") |>
  mutate(forest_perc = (forest_ha/total_ha) * 100)

eco_filt_or <- eco_for_df |>
  filter(forest_ha >= 100000 | forest_perc >= 10)

eco_filt_and <- eco_for_df |>
  filter(forest_ha >= 100000 & forest_perc >= 10)

eco_diff <- anti_join(eco_filt_or, eco_filt_and, by = "ECO_NAME")

eco_filt_controlled <- eco_for_df |>
  filter((forest_ha >= 100000 | forest_perc >= 10) & n_obs >= 50)

#saveRDS(eco_filt_controlled, "03.Data/out/filtered_ecoregions.RDS")


#df_for <- readRDS("03.Data/out/df_for.RDS")

df_for_final <- df_for |> 
  filter(ECO_NAME %in% eco_filt_controlled$ECO_NAME)

saveRDS(df_for_final, "03.Data/out/df_for_final.RDS")

dim(df_for_final)
#4424776
length(unique(df_for_final$REALM))
#8 Realms
length(unique(df_for_final$ECO_NAME))
#653
length(unique(df_for_final$BIOME_NAME))
#15 Biomes
length(unique(df_for_final$ECO_BIOME_))
#58





#correlation within ecoregions raw
#########---

df_for_final <- readRDS("03.Data/out/df_for_final.RDS")

eco_Rs_raw <- df_for_final |>
  group_by(ECO_NAME) |>
  summarise(
    n = n(),
    rs_raw = round(cor(logAGB, AD, method = "spearman"), digits=3),
    .groups = "drop"
  ) |>
  arrange(n)


########## plot ecoregions' r_raw dstribution (r_log)
r <- ggplot(eco_Rs_raw, aes(x = rs_raw)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Ecoregion's r (Spearman)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(eco_Rs_raw$rs_raw, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +  
  annotate("text", x = Inf, y = Inf,
           label = paste0("n = ", nrow(eco_Rs_raw), "\nmedian = ", round(median(eco_Rs_raw$rs_raw), 2)),
           hjust = 1.1, vjust = 1.5, size = 4, color = "gray30")

ggsave(
  filename = "plots/eco_Rs_raw/distr_rs_raw.png",
  plot = r,
  width = 8,
  height = 5,
  dpi = 300)



### aggiungi plot r_raw vs n_pixles x ecoregion and maybe map of ecoregions'r_raw
# WHAT ABOUT all those diagnostic plots presentend to fms at the "presentazione brutta" meeting? they might be needed for supplementary


######## scatterplot by ecoregion 
##########################################


ecolist <- eco_Rs_raw$ECO_NAME


make_plot <- function(eco) {
  df_sub <- df_for_final |> filter(ECO_NAME == eco)
  
  info <- eco_Rs_raw |> filter(ECO_NAME == eco) |> slice(1)
  n_val <- info$n
  r_val <- info$r
  
  ggplot(df_sub, aes(x = AGB, y = AD)) +
    geom_point(alpha = 0.2, size = 0.5, color = "#4a90d9") +
    geom_smooth(method = "gam",formula = y ~ s(x, k = 5), color = "tomato", se = FALSE, linewidth = 0.8) +
    labs(
      title = eco,
      subtitle = sprintf("n = %s | r = %.3f", format(n_val, big.mark = ","), r_val),
      x = "AGB", y = "Sr"
    ) +
    theme_minimal(base_size = 11)
}

pdf("try.pdf", width = 6, height = 8)

for (i in seq(1, length(ecolist), by = 2)){
  p1 <- make_plot(ecolist[i])
  if (i+1 <= length(ecolist)){
    p2 <- make_plot(ecolist[i+1])
  } else {
    p2 <- ggplot() + theme_void()
  }
  
  print(p1 / p2)
}

dev.off()

##########################################










#### null model and standard effect size on r_trimmed
# I will perform a within-ecoregion permutation null model, with a cap on n_pixels at 1k pixels. 





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




