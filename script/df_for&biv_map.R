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

# Sabatini Alpha Diversity
AD_map <- terra::rast("03.Data/in/Sabatini_AlphaDiversity/w3_tile_sr1ha_for.tif")
res(AD_map)
plot(AD_map)

C_map <- terra::rast("03.Data/out/agb_mosaic_yy/AGB_mosaic_2017.tif")
res(C_map)
plot(C_map)


# create Global raster and get df with coordinates for each pixel
rgeo <- terra::rast(nrows=360, ncols=720, xmin=-180, xmax=180, ymin=-90, ymax=90, crs = crs(AD_map)) 
terra::res(rgeo) <- terra::res(AD_map) 
res(rgeo)
rgeo_ctr <- terra::crds(rgeo)

AD_ex <- terra::extract(AD_map, rgeo_ctr)
C_ex <- terra::extract(C_map, rgeo_ctr)

df <- cbind(rgeo_ctr, AD_ex, C_ex, log1p(C_ex)) |> 
  as.data.frame() |>
  setNames(c("x", "y", "AD", "AGB", "logAGB"))


ecoreg <- vect("03.Data/in/Ecoregions/Ecoregions2017.shp")
fields <- c("ECO_NAME", "BIOME_NAME", "REALM", "ECO_BIOME_", "NNH_NAME")

eco_ex <- lapply(fields, function(f) {
  ecorast <- terra::rasterize(ecoreg, AD_map, field = f)
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
         is.finite(AGB))

range(df_for$AGB, na.rm = TRUE)
range(df_for$AD, na.rm = TRUE)

saveRDS(df_for, "03.Data/out/df_for.RDS")


ggplot(df_for, aes(x = logAGB)) +
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

######################################################
#######          bivariate map              ##########
######################################################


world <- ne_countries(scale = "medium", returnclass = "sf")

df_biv <- df_for |>
  mutate(
    AGB_class = ntile(AGB, 3),
    AD_class = ntile(AD, 3), 
    biv_class = paste0(AGB_class, "-", AD_class)
  )

biv_colors <- setNames(
  stevens.purplegold(9),
  as.vector(outer(1:3, 1:3, FUN = function(a, b) paste0(a, "-", b)))
)

biv_map <- ggplot() +
  geom_sf(data = world, fill = "gray46", color = "white", linewidth = 0.15) +
  geom_raster(data = df_biv, aes(x = x, y = y, fill = biv_class)) +
  scale_fill_manual(values = biv_colors, guide = "none") +
  labs(
    title = "Bivariate Map of AGB and AD") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(color = "black"),
    panel.grid = element_line(color = "gray"),
    axis.title = element_blank()
  ) + 
  coord_sf(crs = 4326, expand = FALSE, ylim = c(-60, 90))

legend_df <- expand.grid(AGB_class = 1:3, AD_class = 1:3) |>
  mutate(biv_class = paste0(AGB_class, "-", AD_class))

legend <- ggplot(legend_df, aes(x = AGB_class, y = AD_class, fill = biv_class)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_manual(values = biv_colors, guide = "none") +
  labs(x= "AGB ->", y = "AD ->") +
  theme_minimal(base_size = 8) +
  theme(
    axis.text = element_blank(),
    axis.title = element_text(size = 7),
    panel.grid = element_blank(),
    aspect.ratio = 1
  )


biv_map + inset_element(legend, left = 0.0, bottom = 0.0, right = 0.3, top = 0.3)


