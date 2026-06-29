#creating df_for_final with only ecoregions that pass the filter and with AGB not 0 
df_for <- readRDS("03.Data/out/df_for.RDS")
eco_filt <- readRDS("03.Data/out/df_ecor_filtered.RDS")


df_for_final <- df_for |> 
  filter(ECO_NAME %in% eco_filt$ECO_NAME)

saveRDS(df_for_final, "03.Data/out/df_for_final.RDS")

dim(df_for_final)
#1656740 (removed 47015 0)

length(unique(df_for_final$REALM))
#7 Realms

length(unique(df_for_final$ECO_NAME))
#472 Ecoregions

length(unique(df_for_final$BIOME_NAME))
#14 Biomes

length(unique(df_for_final$ECO_BIOME_))
#52


## plot agb ad and logagb distribution after filtering eco
###################################
SR_distr <- ggplot(df_for_final, aes(x = AD)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "SR global (filtered)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_for_final$AD, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
           label = paste0("n (pixels) = ", nrow(df_for_final), "\nmedian = ", 
                          round(median(df_for_final$AD, na.rm = TRUE), 2)),
           hjust = 2.5,
           vjust = 1.5,
           size = 4,
           color = "gray30"
  ) 

ggsave(
  filename = "plots/ecorr_raw_q095/distr_SR_filtered.png",
  plot = SR_distr,
  width = 8,
  height = 5,
  dpi = 300)

AGB_distr <- ggplot(df_for_final, aes(x = AGB)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "AGB global (filtered - Mg/ha)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_for_final$AGB, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
           label = paste0("n (pixels) = ", nrow(df_for_final), "\nmedian = ", 
                          round(median(df_for_final$AGB, na.rm = TRUE), 2)),
           hjust = 2.5,
           vjust = 1.5,
           size = 4,
           color = "gray30"
  ) 

ggsave(
  filename = "plots/ecorr_raw_q095/distr_AGB_filtered.png",
  plot = AGB_distr,
  width = 8,
  height = 5,
  dpi = 300)


logAGB_distr <- ggplot(df_for_final, aes(x = logAGB)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "log10(AGB) global (filtered)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_for_final$logAGB, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
           label = paste0("n (pixels) = ", nrow(df_for_final), "\nmedian = ", 
                          round(median(df_for_final$logAGB, na.rm = TRUE), 2)),
           hjust = 3.0,
           vjust = 1.5,
           size = 4,
           color = "gray30"
  ) 

ggsave(
  filename = "plots/ecorr_raw_q095/distr_AGBlog_filtered.png",
  plot = logAGB_distr,
  width = 8,
  height = 5,
  dpi = 300)
############################################


#### correlation raw with log10(AGB +1)
############################################

cor_ecor <- df_for_final |>
  group_by(ECO_NAME) |>
  summarise(
    n = n(),
    r_raw = round(cor(logAGB, AD, method = "spearman"), digits=3),
    .groups = "drop"
  ) |>
  arrange(n)

df_ecorR <- cor_ecor |> filter(!is.na(r_raw) & n > 50) |>
  arrange(n)
# 471 ecoregions. one was removed because after removing pixels with AGB = 0  it remained with n = 27 (Mediterranean dry woodlands and steppe)
rm(cor_ecor)

########## plot ecoregions' dstribution ( r and r_log)
r <- ggplot(df_ecorR, aes(x = r_raw)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Ecoregion's r (Spearman)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_ecorR$r_raw, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +  
  annotate("text", x = Inf, y = Inf,
           label = paste0("n = ", nrow(df_ecorR), "\nmedian = ", round(median(df_ecorR$r_raw), 2)),
           hjust = 1.1, vjust = 1.5, size = 4, color = "gray30")

ggsave(
  filename = "plots/ecorr_raw_q095/distr_r_raw.png",
  plot = r,
  width = 8,
  height = 5,
  dpi = 300)


### map r by ecoregion
##########################################

ecoregions <- vect("03.Data/in/Ecoregions_new/Ecoregions2017.shp")
ecoregions_sf <- st_as_sf(ecoregions)
ecoregions_map <- ecoregions_sf %>%
  left_join(df_ecorR, by = "ECO_NAME")

# install.packages("scico")
library(scico)
ecorR_map <- ggplot(ecoregions_map) +
  geom_sf(aes(fill = r_raw), color = NA) +
  scale_fill_distiller(
    palette = "RdBu",
    direction = -1,
    limits = c(-1, 1),
    na.value = "grey85",
    name = "r Spearman"
  ) +
  labs(title = "Ecoregions' r - Spearman (all 471)",) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank()) +
  coord_sf(crs = "+proj=robin")
ecorR_map

ggsave(
  filename = "plots/ecorr_raw_q095/map_r_raw.png",
  plot = ecorR_map,
  width = 10,
  height = 6,
  dpi = 300
)

##########################################


######## scatterplot by ecoregion 
##########################################


ecolist <- cor_ecor$ECO_NAME

#try
#ecolist <- ecolist[50:53]

make_plot <- function(eco) {
  df_sub <- df_for_final |> filter(ECO_NAME == eco)
  
  info <- cor_ecor |> filter(ECO_NAME == eco) |> slice(1)
  n_val <- info$n
  r_val <- info$r
  
  ggplot(df_sub, aes(x = AGB, y = AD)) +
    geom_point(alpha = 0.2, size = 0.5, color = "#4a90d9") +
    geom_smooth(method = "gam",formula = y ~ s(x, k = 5), color = "tomato", se = FALSE, linewidth = 0.8) +
    labs(
      title = eco,
      subtitle = sprintf("n = %s | r = %.3f", format(n_val, big.mark = ","), r_val),
      x = "AGB", y = "Alpha Diversity"
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



########  creation df with ecor, r, climate and lat 
##########################################


wc_dir <- "03.Data/in/wc2.1"
#dir.create(wc_dir, showWarnings = FALSE)
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

clim_means$ECO_NAME <- ecoregions$ECO_NAME

centr <- terra::centroids(ecoregions)
centr_xy <- terra::crds(centr)
lat <- data.frame(centr_xy)$y
clim_means$lat <- lat

ddf <- merge(df_ecorR, clim_means, by.x = "ECO_NAME", by.y = "ECO_NAME", all.x = TRUE)

#remove ID column
ddf <- ddf[,-4]

ddf <- ddf|>
  mutate(hemi = case_when(
    lat > 0 ~ "N",
    lat < 0 ~ "S",
  ))

saveRDS(ddf, "03.Data/out/df_ecorR_raw.RDS")
#############################


## geom density latitude
#############
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


###########

### geom density median mean annual temperature
############
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
################

### geom density median temperature seasonality
##########
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
#############

## geom density median annual precipitation
##########
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
############

## geom density meadian precipitation seasonality
###########
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
###############

###### scatterplot r vs latitude, global and dividing hemispheres
##############
r_lat <- ggplot(ddf, aes(x = abs(lat), y = r_raw)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "latitude",
    y = "Spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_latns <- ggplot(ddf, aes(x = lat, y = r_raw)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  facet_wrap(~ hemi, scales = "free_x") +
  labs(
    x = "latitude",
    y = "spearman's r"
  ) +
  theme_minimal(base_size = 13)

r_lat / r_latns

ggsave(
  filename = "plots/ecorr_raw_q095/scatt_r_lat.png",
  plot = r_lat / r_latns,
  width = 8,
  height = 5,
  dpi = 300)
##################

## mean annual temperature
################
r_mat <- ggplot(ddf, aes(x = bio1, y = r_raw)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "Mean Annual Temperature",
    y = "Spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_matns <- ggplot(ddf, aes(x = bio1, y = r_raw)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  facet_wrap(~ hemi, scales = "free_x") +
  labs(
    x = "Mean Annual Temperature",
    y = "spearman's r"
  ) +
  theme_minimal(base_size = 13)


ggsave(
  filename = "plots/ecorr_raw_q095/scatt_r_bio1.png",
  plot = r_mat / r_matns,
  width = 8,
  height = 5,
  dpi = 300)
#####################

## Temperature seasonality
################
r_ts <- ggplot(ddf, aes(x = bio4, y = r_raw)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "Temperature seasonality",
    y = "Spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_tsns <- ggplot(ddf, aes(x = bio4, y = r_raw)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  facet_wrap(~ hemi, scales = "free_x") +
  labs(
    x = "Temperature seasonality",
    y = "spearman's r"
  ) +
  theme_minimal(base_size = 13)


ggsave(
  filename = "plots/ecorr_raw_q095/scatt_r_bio4.png",
  plot = r_ts / r_tsns,
  width = 8,
  height = 5,
  dpi = 300)

################

## Annual precipitation
###############
r_ap <- ggplot(ddf, aes(x = bio12, y = r_raw)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "Annual precipitation",
    y = "Spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_apns <- ggplot(ddf, aes(x = bio12, y = r_raw)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  facet_wrap(~ hemi, scales = "free_x") +
  labs(
    x = "Annual precipitation",
    y = "spearman's r"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = "plots/ecorr_raw_q095/scatt_r_bio12.png",
  plot = r_ap / r_apns,
  width = 8,
  height = 5,
  dpi = 300)

####################

## Precipitation seasonality
##############
r_ps <- ggplot(ddf, aes(x = bio15, y = r_raw)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "Precipitation seasonality",
    y = "Spearman's r"
  ) +
  theme_minimal(base_size = 13)


r_psns <- ggplot(ddf, aes(x = bio15, y = r_raw)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  facet_wrap(~ hemi, scales = "free_x") +
  labs(
    x = "Precipitation seasonality",
    y = "spearman's r"
  ) +
  theme_minimal(base_size = 13)


ggsave(
  filename = "plots/ecorr_raw_q095/scatt_r_bio15.png",
  plot = r_ps / r_psns,
  width = 8,
  height = 5,
  dpi = 300)

#########################

lm <- lm(r_spearman ~ bio1 + bio12, data = ddf)

library(pdp)

pdp_T <- partial(lm, pred.var = "bio1", train = ddf)
plotPartial(pdp_T)

pdp_P <- partial(lm, pred.var = "bio12", train = ddf)
plotPartial(pdp_P)

library(mgcv)

gamT <- gam(r_spearman ~ s(bio1) + s(bio12), data = ddf)

plot(gamT, select = 1, residuals = TRUE)  # partial effect of bio1
plot(gamT, select = 2, residuals = TRUE)  # partial effect of bio12
