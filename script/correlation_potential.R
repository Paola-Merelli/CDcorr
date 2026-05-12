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

df <- cbind(rgeo_ctr, AD_ex, C_ex) |> 
  as.data.frame() |>
  setNames(c("x", "y", "AD", "AGB"))

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
         !is.na(AGB))

range(df_for$AGB, na.rm = TRUE)
range(df_for$AD, na.rm = TRUE)

saveRDS(df_for, "03.Data/out/df_for.RDS")

#df_for <- readRDS("03.Data/out/df_for.RDS")


dim(df_for)
#1711559

length(unique(df_for$REALM))
#7 Realms

length(unique(df_for$ECO_NAME))
#605 Ecoregions

length(unique(df_for$BIOME_NAME))
#15 Biomes

length(unique(df_for$ECO_BIOME_))
#59


#df$REALM <- as.factor(df$REALM)
#df$BIOME_NAME <- as.factor(df$BIOME_NAME)
#df$ECO_BIOME_ <- as.factor(df$ECO_BIOME_)


############## try corr
hist(df_for$AGB)
hist(df_for$AD)


cor_raw_ecor <- df_for %>%
  group_by(ECO_NAME) %>%
  summarise(
    n = n(),
    r = cor(AGB, AD, method = "spearman"),
    .groups = "drop"
  )

cor_raw_ecor %>% 
  filter(is.na(r)) %>% 
  arrange(n)

summary(cor_raw_ecor$n)

cor_ecor_filt <- cor_raw_ecor %>%
  filter(!is.na(r), n >= 30, !is.na(ECO_NAME))

ecoreg <- vect(file.path("03.Data/Ecoregions/Ecoregions2017.shp"))
eco_df <- as.data.frame(ecoreg)
unique(eco_df$ECO_ID)

ecoregions_map <- ecoreg %>%
  left_join(cor_ecor_filt, by = "ECO_NAME")


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




