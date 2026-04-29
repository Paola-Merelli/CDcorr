library(tidyverse)
library(terra)
library(tidyterra)
library(raster)
library(rnaturalearth)
library(sf)
library(maps)
library(mgcv)
library(ggplot2)

# Sabatini Alpha Diversity
AD_1ha <- rast("03.Data/Sabatini_AlphaDiversity/w3_tile_sr1ha_for.tif")
res(AD_1ha)
plot(AD_1ha)

# esa agb
C_list <- list.files("03.Data/dap.ceda.ac.uk/", pattern ="*.tif$", full.names = TRUE, recursive = TRUE)
C_rast <- C_list[str_detect(C_list, "SD", negate = TRUE)] |>
  lapply(FUN = terra::rast)
C_list[[1]]
C_rast[[1]] 

C_res <- lapply(seq_along(C_rast), function(i) {
  out <- paste0("03.Data/agb_resampled/res_", i, ".tif")
  C_rast[[i]] |>
    terra::resample(AD_1ha, method = "mean") |>
    terra::writeRaster(out, overwrite = TRUE)
  gc()
  out
})


#C_res <- list.files("03.Data/agb_resampled/", full.names = T)
vrt_file <- "agb_mosaic.vrt"
terra::vrt(C_res, filename = vrt_file, overwrite = T)
C_map <- terra::rast(vrt_file)
plot(C_map)

terra::writeRaster(
  C_map,
  "03.Data/AGB_merged.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BLOCKXSIZE=512", "BLOCKYSIZE=512")
)


rm(vrt_file)
gc()


#C_map <- terra::rast("03.Data/AGB_merged.tif")
res(AD_1ha)
res(C_map)
plot(AD_1ha)


# Global raster
rgeo <- terra::rast(nrows=360, ncols=720, xmin=-180, xmax=180, ymin=-90, ymax=90, crs = crs(AD_1ha)) # raster at half a degree resolution (cf. 30 arc minute resolution)
terra::res(rgeo) <- terra::res(AD_1ha) 
#rgeo <- raster::disaggregate(rgeo, fact=12) # raster at 2.5 arc minute resolution
res(rgeo)
rgeo_ctr <- terra::crds(rgeo)

AD_ex <- terra::extract(AD_1ha, rgeo_ctr)
C_ex <- terra::extract(C_map, rgeo_ctr)

df <- cbind(rgeo_ctr, AD_ex, C_ex) |> 
  as.data.frame()

colnames(df)[3:4] <- c("AD_1ha", "AGB")

df_for <- df |> 
  filter(!is.na(AD_1ha),
         !is.na(AGB))

saveRDS(df_for, "03.Data/AD-AGB_pixval_for.RDS")

#df_for <- readRDS("03.Data/AD-AGB_pixval_for.RDS")

ecoreg <- vect(file.path("03.Data/Ecoregions/Ecoregions2017.shp"))
fields <- c("ECO_NAME", "BIOME_NAME", "REALM", "ECO_BIOME_", "NNH_NAME")

eco_ex <- lapply(fields, function(f) {
  ecorast <- terra::rasterize(ecoreg, AD_1ha, field = f)
  ex <- terra::extract(ecorast, df_for[, 1:2])
  rm(ecorast)
  gc()
  ex[[2]]
}) |>
  setNames(fields) |> 
  as.data.frame()

df_for <- cbind(df_for, eco_ex)
sum(is.na(df_for$AGB))

df_for$BIOME_NAME |> unique()
df_for$REALM |> unique()

df <- df_for |> 
  filter(!is.na(REALM),
         REALM != "N/A")

df$BIOME_NAME |> unique()

dim(df)
#1692410

length(unique(df$REALM))
#7 Realms

length(unique(df$ECO_NAME))
#604 Ecoregions

length(unique(df$BIOME_NAME))
#14 Biomes

length(unique(df$ECO_BIOME_))
#58


df$REALM <- as.factor(df$REALM)
df$BIOME_NAME <- as.factor(df$BIOME_NAME)
df$ECO_BIOME_ <- as.factor(df$ECO_BIOME_)

saveRDS(df, "03.Data/df_out.RDS")






############## try corr
hist(df$AGB)
hist(df$AD_1ha)


cor_raw_bior <- df %>%
  group_by(ECO_BIOME_) %>%
  summarise(
    n = n(),
    r = cor(AGB, AD_1ha, method = "spearman"),
    .groups = "drop"
  )

cor_raw_ecor <- df %>%
  group_by(ECO_NAME) %>%
  summarise(
    n = n(),
    r = cor(AGB, AD_1ha, method = "spearman"),
    .groups = "drop"
  )

ecoreg <- vect(file.path("03.Data/Ecoregions/Ecoregions2017.shp"))
eco_df <- as.data.frame(ecoreg)
unique(eco_df$ECO_ID)

bioregions_map <- ecoreg %>%
  left_join(cor_raw_bior, by = "ECO_BIOME_")

ecoregions_map <- ecoreg %>%
  left_join(cor_raw_ecor, by = "ECO_NAME")

p_bio <- ggplot(bioregions_map) +
  geom_sf(aes(fill = r), color = NA) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    na.value = "grey85",
    name = "Spearman r"
  ) +
  labs(
    title = "by bioregion",
    caption = "Grey = no data / insufficient observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(crs = "+proj=robin")

p_bio

p_eco <- ggplot(ecoregions_map) +
  geom_sf(aes(fill = r), color = NA) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    na.value = "grey85",
    name = "Spearman r"
  ) +
  labs(
    title = "by ecoregion",
    caption = "Grey = no data / insufficient observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(crs = "+proj=robin")
p_eco


mask_for <- !is.na(AD_1ha)
plot(mask_for)





