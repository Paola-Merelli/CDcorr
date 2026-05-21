library(tidyverse)
library(terra)
library(tidyterra)
library(raster)
library(sf)
library(ggplot2)

# Sabatini Alpha Diversity
AD_map <- rast("03.Data/in/Sabatini_AlphaDiversity/w3_tile_sr1000_for.tif")
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

df <- cbind(rgeo_ctr, AD_ex, C_ex, log(C_ex)) |> 
  as.data.frame() |>
  setNames(c("x", "y", "AD", "AGB", "logAGB"))

#df$logAGB <- log(df$AGB)

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
         !is.infinite(AD),
         !is.infinite(AGB),
         !is.infinite(logAGB))

range(df_for$AGB, na.rm = TRUE)
range(df_for$AD, na.rm = TRUE)

saveRDS(df_for, "03.Data/out/df_for.RDS")

#df_for <- readRDS("03.Data/out/df_for.RDS")

#filter df_for  keeping only filtered ecoregions
eco_filt <- readRDS("03.Data/out/df_ecor_filtered.RDS")
df_for_final <- df_for |> 
  filter(ECO_NAME %in% eco_filt$ECO_NAME)

dim(df_for_final)
#1704330

length(unique(df_for_final$REALM))
#7 Realms

length(unique(df_for_final$ECO_NAME))
#497 Ecoregions

length(unique(df_for_final$BIOME_NAME))
#14 Biomes

length(unique(df_for_final$ECO_BIOME_))
#53


#df$REALM <- as.factor(df$REALM)
#df$BIOME_NAME <- as.factor(df$BIOME_NAME)
#df$ECO_BIOME_ <- as.factor(df$ECO_BIOME_)


############## try corr
#hist(df_for_final$logAGB)
#hist(df_for_final$AD)

cor_ecor <- df_for_final %>%
  group_by(ECO_NAME) %>%
  summarise(
    n = n(),
    r = cor(AGB, AD, method = "pearson"),
    .groups = "drop"
  ) 

cor_ecor_log <- df_for_final %>%
  group_by(ECO_NAME) %>%
  summarise(
    r_log= cor(logAGB, AD, method = "pearson"),
    .groups = "drop"
  ) 
cor_ecor$r_log <- cor_ecor_log$r_log

df_ecorR <- cor_ecor |> filter(!is.na(r), !is.na(r_log), n >= 10) |>
  arrange(n)

saveRDS(df_ecorR, "03.Data/out/df_ecorR.RDS")

ecoregions <- st_as_sf(ecoreg)
ecoregions_map <- ecoregions %>%
  left_join(df_ecorR, by = "ECO_NAME")


ecorR_map <- ggplot(ecoregions_map) +
  geom_sf(aes(fill = r), color = NA) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    na.value = "grey85",
    name = "Pearson's r"
  ) +
  labs(
    title = "AD-AGB by ecoregion (491)",
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(crs = "+proj=robin")
ecorR_map

ecorR_log_map <- ggplot(ecoregions_map) +
  geom_sf(aes(fill = r_log), color = NA) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    na.value = "grey85",
    name = "Pearson's r"
  ) +
  labs(
    title = "AD-logAGB by ecoregion (491)",
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(crs = "+proj=robin")
ecorR_log_map


