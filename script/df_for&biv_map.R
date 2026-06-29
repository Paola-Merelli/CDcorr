library(dplyr)
library(terra)
library(tidyterra)
library(raster)
library(sf)
library(ggplot2)
library(patchwork)
library(rnaturalearth)
library(biscale)
library(pals)
library(stringr)

# Sabatini Alpha Diversity
AD_map <- terra::rast("03.Data/in/Sabatini_AlphaDiversity/w3_tile_sr1000_for.tif")
res(AD_map)
plot(AD_map)

C_map <- terra::rast("03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q95.tif")
res(C_map)
plot(C_map)


# create Global raster and get df with coordinates for each pixel
rgeo <- terra::rast(nrows=360, ncols=720, xmin=-180, xmax=180, ymin=-90, ymax=90, crs = crs(AD_map)) 
terra::res(rgeo) <- terra::res(AD_map) 
res(rgeo)
rgeo_ctr <- terra::crds(rgeo)

AD_ex <- terra::extract(AD_map, rgeo_ctr)
C_ex <- terra::extract(C_map, rgeo_ctr)

df <- cbind(rgeo_ctr, AD_ex, C_ex, log10(C_ex + 1)) |> 
  as.data.frame() |>
  setNames(c("x", "y", "AD", "AGB", "logAGB"))


ecoregions <- vect("03.Data/in/Ecoregions_new/Ecoregions2017.shp")
fields <- c("ECO_NAME", "BIOME_NAME", "REALM", "ECO_BIOME_", "NNH_NAME")

eco_ex <- lapply(fields, function(f) {
  ecorast <- terra::rasterize(ecoregions, AD_map, field = f)
  ex <- terra::extract(ecorast, df[, 1:2])
  rm(ecorast)
  gc()
  ex[[2]]
}) |>
  setNames(fields) |> 
  as.data.frame()


df_eco <- cbind(df, eco_ex)


df_for <- df_eco |> 
  filter(!is.na(AD),
         !is.na(AGB),
         is.finite(AD),
         is.finite(AGB), 
         AGB != 0)

range(df_for$AGB, na.rm = TRUE)
range(df_for$AD, na.rm = TRUE)

saveRDS(df_for, "03.Data/out/df_for.RDS")
rm(df, df_eco, eco_ex)
#df_for <- readRDS("03.Data/out/df_for.RDS")

# distribution SR, AGB, AGBlog raw
###############
dis_SR <- ggplot(df_for, aes(x = AD)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "SR global",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_for$AD, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
           label = paste0("n (pixels) = ", nrow(df_for), "\nmedian = ", 
                          round(median(df_for$AD, na.rm = TRUE), 2)),
           hjust = 3.1,
           vjust = 1.5,
           size = 4,
           color = "gray30"
  ) 
ggsave(
  filename = "plots/ecorr_raw_q095/distr_SR.png",
  plot = dis_SR,
  width = 8,
  height = 5,
  dpi = 300)

dis_SR

dis_AGB <- ggplot(df_for, aes(x = AGB)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "AGB global",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_for$AGB, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
           label = paste0("n (pixels) = ", nrow(df_for), "\nmedian = ", 
                          round(median(df_for$AGB, na.rm = TRUE), 2)),
           hjust = 3.1,
           vjust = 1.5,
           size = 4,
           color = "gray30"
  ) 

ggsave(
  filename = "plots/ecorr_raw_q095/distr_AGB.png",
  plot = dis_AGB,
  width = 8,
  height = 5,
  dpi = 300
)

dis_AGB


dis_AGBlog <- ggplot(df_for, aes(x = logAGB)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "log(AGB) global",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_for$logAGB, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
    label = paste0("n (pixels) = ", nrow(df_for), "\nmedian = ", 
                   round(median(df_for$logAGB, na.rm = TRUE), 2)),
    hjust = 3.1,
    vjust = 1.5,
    size = 4,
    color = "gray30"
  )

ggsave(
    filename = "plots/ecorr_raw_q095/distr_AGBlog.png",
    plot = dis_AGBlog,
    width = 8,
    height = 5,
    dpi = 300
  )
dis_AGBlog
###############

#scatterplot AD AGB global
###################

scat <- ggplot(df_for, aes(x = AD, y = AGB)) +
  geom_hex(bins = 80) +
  scale_fill_viridis_c(trans = "log10", name = "n pixels \n log10") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5),
              color = "#D55E00", se = FALSE) +
  labs(x = "Species richness", y = "Aboveground biomass") +
  theme_minimal(base_size = 13)
scat
ggsave(
  filename = "plots/ecorr_raw_q095/scatt_SR-AGB_global.png",
  plot = scat,
  width = 8,
  height = 5,
  dpi = 300)

scat_log <- ggplot(df_for, aes(x = log10(AD), y = logAGB)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log10", name = "N pixels \n (log10)", option = "mako") +
  geom_smooth(method = "lm",
              color = "#D55E00", se = FALSE) +
  labs(x = "Species richness (log10)", y = "Aboveground biomass (log10)") +
  theme_minimal(base_size = 13)
scat_log



ggsave(
  filename = "plots/ecorr_raw_q095/scatt_SR-AGB_global_log.png",
  plot = scat_log,
  width = 8,
  height = 6,
  dpi = 300)


mod <- lm(logAGB ~ log10(AD), data = df_for)
summary(mod)

# estrazione dei singoli valori
slope    <- coef(mod)[["log10(AD)"]]
intercept <- coef(mod)[["(Intercept)"]]
r2       <- summary(mod)$r.squared
pval     <- summary(mod)$coefficients["log10(AD)", "Pr(>|t|)"]

slope
r2
pval

sprintf("slope = %.3f, R² = %.3f, p %s",
        slope,
        r2,
        ifelse(pval < 0.001, "< 0.001", paste0("= ", signif(pval, 2))))




################################

### bivariate map
######################################################
library(biscale)


##### with manual quantiles ---> need to be changed in robinson as quantiles
############
world <- ne_countries(scale = "medium", returnclass = "sf")
summary(df_for$AD)
summary(df_for$AGB)
agb_breaks <- c(0, 100, 250, Inf) 
ad_breaks  <- c(0, 25, 50, Inf)

df_biv <- df_for |>
  mutate(
    AGB_class = as.integer(cut(AGB, breaks = agb_breaks, labels = FALSE, include_lowest = TRUE)),
    AD_class  = as.integer(cut(AD,  breaks = ad_breaks,  labels = FALSE, include.lowest = TRUE)),
    biv_class = paste0(AD_class, "-", AGB_class)
  )

biv_colors <- bi_pal(pal = "DkBlue2", dim = 3, preview = FALSE)

biv_map <- ggplot() +
  geom_sf(data = world, fill = "azure4", color = "NA", linewidth = 0.01) +
  geom_raster(data = df_biv, aes(x = x, y = y, fill = biv_class)) +
  scale_fill_manual(values = biv_colors, guide = "none") +
  labs(
    title = "Bivariate Map of Species Richness anf Aboveground Biomass in Global Forests") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(color = "black"),
    panel.grid = element_line(color = "azure2"),
    axis.title = element_blank()
  ) + 
  coord_sf(crs = 4326, expand = FALSE, ylim = c(-60, 90))

legend_df <- expand.grid(AD_class = 1:3, AGB_class = 1:3) |>
  mutate(biv_class = paste0(AD_class, "-", AGB_class))

agb_labels <- c("1-100", "100-250", ">250")
ad_labels  <- c("0-25", "25-50", ">50")

legend <- ggplot(legend_df, aes(x = AD_class, y = AGB_class, fill = biv_class)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(values = biv_colors, guide = "none") +
  scale_x_continuous(breaks = 1:3, labels = ad_labels) +
  scale_y_continuous(breaks = 1:3, labels = agb_labels) +
  labs(x = "SR →", y = "AGB (Mg/ha) →") +
  theme_minimal(base_size = 8) +
  theme(
    axis.text = element_text(size = 6),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 7),
    panel.grid = element_blank(),
    aspect.ratio = 1
  )

biv_map + inset_element(legend, left = 0.0, bottom = 0.0, right = 0.35, top = 0.35)

ggsave(
  filename = "plots/ecorr_raw_q095/biv_map_manual.png",
  plot = biv_map + inset_element(legend, left = -0.01, bottom = 0.0, right = 0.35, top = 0.35),
  width = 8,
  height = 5,
  dpi = 300
)
##############

#### biv map with quantiles
##################################

ad_breaks_q <- quantile(df_for$AD,  probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
agb_breaks_q <- quantile(df_for$AGB, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)

ad_labels_q  <- round(ad_breaks_q, 1)
agb_labels_q <- round(agb_breaks_q, 0)

df_biv_q <- df_for |>
  mutate(
    AGB_classq = ntile(AGB, 3),
    AD_classq  = ntile(AD,  3),
    biv_classq = paste0(AD_classq, "-", AGB_classq)
  )

df_biv_q$biv_classq <- as.factor(df_biv_q$biv_classq)
df_biv_q$biv_code   <- as.integer(df_biv_q$biv_classq)

biv_colors <- bi_pal(pal = "DkBlue2", dim = 3, preview = FALSE)

robin_crs <- "+proj=robin"
sf::sf_use_s2(FALSE)

world_crop <- world |>
  st_crop(xmin = -180, xmax = 180, ymin = -60, ymax = 90) |>
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) |>
  st_transform(robin_crs)

r_biv <- rast(
  df_biv_q[, c("x", "y", "biv_code")],
  type = "xyz",
  crs  = "EPSG:4326"
)

levels(r_biv) <- data.frame(
  value      = seq_along(levels(df_biv_q$biv_classq)),
  biv_classq = levels(df_biv_q$biv_classq)
)
names(r_biv) <- "biv_classq"

r_biv_crop <- crop(r_biv, ext(-180, 180, -60, 90))
r_biv_robin <- project(r_biv_crop, robin_crs, method = "near")

df_biv_robin <- as.data.frame(r_biv_robin, xy = TRUE, na.rm = TRUE)



biv_map_q <- ggplot() +
  geom_sf(data = world_crop, fill = "azure4", color = NA, linewidth = 0.15) +
  geom_raster(data = df_biv_robin, aes(x = x, y = y, fill = biv_classq)) +
  scale_fill_manual(values = biv_colors, guide = "none") +
  labs(title = "Bivariate Map of Species Richness and Aboveground Biomass in Global Forests") +
  coord_sf(
    crs = robin_crs,
    datum = sf::st_crs(4326),
    expand = FALSE
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(color = "black"),
    panel.grid.major = element_line(color = "azure2", linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.title = element_blank()
  )


legend_dfq <- expand.grid(AD_classq = 1:3, AGB_classq = 1:3) |>
  mutate(biv_classq = paste0(AD_classq, "-", AGB_classq))

legend_q <- ggplot(legend_dfq, aes(x = AD_classq, y = AGB_classq, fill = biv_classq)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(values = biv_colors, guide = "none") +
  scale_x_continuous(
    breaks = 1:3,
    labels = c(
      paste0(ad_labels_q[1], "–", ad_labels_q[2]),
      paste0(ad_labels_q[2], "–", ad_labels_q[3]),
      paste0(ad_labels_q[3], "–", ad_labels_q[4])
    ),
    name = "SR →"
  ) +
  scale_y_continuous(
    breaks = 1:3,
    labels = c(
      paste0(agb_labels_q[1], "–", agb_labels_q[2]),
      paste0(agb_labels_q[2], "–", agb_labels_q[3]),
      paste0(agb_labels_q[3], "–", agb_labels_q[4])
    ),
    name = "AGB (Mg/ha) →"
  ) +
  theme_minimal(base_size = 6) +
  theme(
    axis.text.x  = element_text(size = 4.5, angle = 30, hjust = 1),
    axis.text.y  = element_text(size = 4.5),
    axis.title   = element_text(size = 5.5),
    aspect.ratio = 1,
    plot.background = element_rect(fill = "white", color = NA)
  )

biv_map_q +  inset_element(legend_q, left = 0.00, bottom = 0.1, right = 0.3, top = 0.4)

ggsave(
  filename = "plots/poster_IAVS/biv_map_quantiles.png",
  plot = biv_map_q  + inset_element(legend_q, left = -0.01, bottom = 0.0, right = 0.35, top = 0.35),
  width = 8,
  height = 5,
  dpi = 300
)




### this should go up, before removing zeroes
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

C_list <- list.files("03.Data/in/dap.ceda.ac.uk/", pattern ="*.tif$", full.names = TRUE, recursive = TRUE)
C_path <- C_list[str_detect(C_list, "SD", negate = TRUE)]
AD_map <- terra::rast("03.Data/in/Sabatini_AlphaDiversity/w3_tile_sr1000_for.tif")

vrt_file <- "agb_mosaic.vrt"
terra::vrt(C_path, filename = vrt_file, overwrite = TRUE)
C_map_orig <- terra::rast(vrt_file)

C_india_orig <- terra::crop(C_map_orig, terra::ext(75, 85, 10, 25))
plot(C_india_orig)
hist(values(C_india_orig), breaks = 100)
freq_india_orig <- terra::freq(C_india_orig, value = 0)
freq_india_orig

# c'erano già nel dato originale, artefatto. bisognerebbe provare con un altro anno. per ora continuo filtrando gli 0 