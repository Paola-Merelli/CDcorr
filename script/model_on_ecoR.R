ecoregions <- terra::vect("03.Data/in/Ecoregions_new/Ecoregions2017.shp")
df_ecorr <- readRDS("03.Data/out/df_cor_ecor.RDS")


wc_dir <- "03.Data/in/wc2.1"
dir.create(wc_dir, showWarnings = FALSE)
bio <- geodata::worldclim_global(var = "bio", res = 2.5, path = wc_dir)
names(bio) <- paste0("bio", 1:nlyr(bio))
bioT <- bio[["bio1"]]
bioP <- bio[["bio12"]]
bio_stk <- c(bioT, bioP)

clim_means <- terra::extract(
  bio_stk,
  ecoreg,
  fun = mean,
  na.rm = TRUE)

clim_means$ECO_NAME <- ecoreg$ECO_NAME

centr <- terra::centroids(ecoregions)
centr_xy <- terra::crds(centr)
lat <- data.frame(centr_xy)$y
clim_means$lat <- lat

ddf <- merge(df_ecorr, clim_means, by.x = "ECO_NAME", by.y = "ECO_NAME", all.x = TRUE)
#remove ID column
ddf <- ddf[,-4]
lmlat <- lm(r ~ abs(lat), data = ddf)
summary(lmlat)



