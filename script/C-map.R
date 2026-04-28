library(tidyverse)
library(terra)
library(tidyterra)
library(raster)
library(rnaturalearth)
library(sf)
library(maps)
library(mgcv)

C.files <- list.files("03.Data/prova_agb",
           pattern = "*.tif$", full.names = TRUE, recursive = TRUE)


C.files.sub <- C.files[str_detect(C.files, "SD")]

C.list <- lapply(C.files.sub, FUN = terra::rast)

C.list[[1]] # resolution is 0.000888888, ~ 3 arc sec --> we want 2.5 arc min (as Alpha Div, Sabatini et al. 2022)

# Resample to destination resolution 2.5 arc min ~ 0.416667
C.res <- lapply(C.list, FUN = function(x) {
  # x2 <- x
  # res(x2) <- 100
  # x2 <- resample(x, x2)
  # x2
  terra::aggregate(x, fact = 0.0416667/res(x)[1], fun = mean, na.rm = TRUE)
})
C.res[[1]]
saveRDS(C.res, "agb_resampled.RDS")
C.map <- do.call(mosaic, C.res)

plot(C.map)

saveRDS(C.map, "../03.Data/_abovegroundBiomass_2.5arcmin.RDS")

# Sabatini Alpha Diversity

#for 1ha forests

AD.1ha <- rast("../03.Data/Sabatini-AlphaDiversity/w3_tile_sr1ha_for.tif")

plot(AD.1ha)

res(AD.1ha)
res(C.map)

# Global raster
rgeo <- raster::raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90) # raster at half a degree resolution (cf. 30 arc minute resolution)
res(rgeo)
rgeo <- raster::disaggregate(rgeo, fact=12) # raster at 2.5 arc minute resolution
res(rgeo)
rgeo_ctr <- raster::coordinates(rgeo)

ex.sp <- terra::extract(AD.1ha, rgeo_ctr)
ex.C <- terra::extract(C.map, rgeo_ctr)

df <- cbind(rgeo_ctr, ex.sp, ex.C) |> 
  as.data.frame()

colnames(df)[3:4] <- c("AD_1ha", "AGB")

df.for <- df |> 
  filter(!is.na(AD_1ha),
         !is.na(AGB))

#save df

saveRDS(df.for, "../03.Data/_AD1ha-AGB_for.RDS")

df.for <- readRDS("../03.Data/_AD1ha-AGB_for.RDS")

#read extracted ecoregions (HPC)

eco.files <- list.files("../03.Data/Extracted-Ecoregions", pattern = "*.Rds",
                        full.names = TRUE)

eco.list <- lapply(eco.files, readRDS)

eco.df <- do.call(rbind, eco.list)

sum(is.na(eco.df$x))

df.for$longlat <- paste(df.for$x, df.for$y, sep = ",")
eco.df$longlat <- paste(eco.df$x, eco.df$y, sep = ",")

eco.na <- eco.df |> 
  filter(is.na(x))

df.xy <- df.for |> 
  filter(!longlat %in% eco.df$longlat)

ids <- eco.na$id.y[duplicated(eco.na$id.y)]

eco.na |> 
  filter(id.y %in% ids)
#we drop the duplicates

df.na <- cbind(
  df.xy[-c(ids),],
  eco.na |> 
    filter(!id.y %in% ids) |> 
    dplyr::select(-c(x:AGB))
)

eco.out <- rbind(
  eco.df |> 
    filter(!is.na(x)) |> 
    dplyr::select(-longlat),
  df.na |> 
    dplyr::select(-longlat)
  ) |> 
  filter(!is.na(REALM))

dim(eco.out)
#1707820 points with 2.5arcmin spacing

eco.df$BIOME_NAME |> unique()
eco.df$REALM |> unique()

df <- eco.out |> 
  filter(!is.na(REALM),
         REALM != "N/A")
dim(df)
#1707819

length(unique(df$REALM))
#7 Realms

length(unique(df$ECO_NAME))
#602 Ecoregions

length(unique(df$BIOME_NAME))
#14 Biomes

length(unique(df$ECO_BIOME_))
#58
table(df$ECO_BIOME_)

df$REALM <- as.factor(df$REALM)
df$BIOME_NAME <- as.factor(df$BIOME_NAME)
df$ECO_BIOME_ <- as.factor(df$ECO_BIOME_)

saveRDS(df, "../03.Data/_AD1ha-AGB-Eco_for.RDS")

# ggplot() +
#   geom_spatraster(data = AD.1ha) +
#   geom_point(data = df, aes(x = x, y = y, color = REALM), size = 0.01)
# 
ggplot() +
  geom_point(data = df, aes(x = x, y = y, color = BIOME_NAME), size = 0.01)

#let s do some models- we go from the simpliest to the most complex

#remove everything beside df
rm(list =ls()[-1])

data.wgs <- sp::SpatialPointsDataFrame(coords = df[,1:2], 
                                   data = df[3:10],
                                   proj4string = sp::CRS("+proj=eck4"))

gam1 <- mgcv::gam(AGB ~ log(AD_1ha) +
                    s(x, y, bs = "sos"), 
                  family = "gaussian", method = "REML", data = data.wgs)

saveRDS(gam1, "../03.Data/_GAM-AGB-AD_1ha.RDS")

summary(gam1)

#62.7%

gam2 <- mgcv::gam(AGB ~ log(AD_1ha) + REALM +
                    s(x, y, bs = "sos"), 
                  family = "gaussian", method = "REML", data = data.wgs)

saveRDS(gam2, "../03.Data/_GAM-AGB-AD_1ha_R.RDS")

summary(gam2)

gam3 <- mgcv::gam(AGB ~ log(AD_1ha) + BIOME_NAME +
                    s(x, y, bs = "sos"), 
                  family = "gaussian", method = "REML", data = data.wgs)

saveRDS(gam3, "../03.Data/_GAM-AGB-AD_1ha_B.RDS")

gam4 <- mgcv::gam(AGB ~ log(AD_1ha) + ECO_BIOME_ +
                    s(x, y, bs = "sos"), 
                  family = "gaussian", method = "REML", data = data.wgs)

saveRDS(gam4, "../03.Data/_GAM-AGB-AD_1ha_RB.RDS")

gam5 <- mgcv::gam(AGB ~ log(AD_1ha) +
                    REALM + BIOME_NAME + 
                    s(x, y, bs = "sos"), 
          family = "gaussian", method = "REML", data = data.wgs)

saveRDS(gam5, "../03.Data/_GAM-AGB-AD_1ha_B-R.RDS")

gam6 <- mgcv::gam(AGB ~ log(AD_1ha) +
                    REALM * BIOME_NAME + 
                    s(x, y, bs = "sos"), 
                  family = "gaussian", method = "REML", data = data.wgs)

saveRDS(gam6, "../03.Data/_GAM-AGB-AD_1ha_BR.RDS")

