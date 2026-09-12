library(sf)
library(terra)
library(stringr)


######## for now just spatial resampling on a single AGB layer (2017) 
##### resample con q90 su vrt globale, ma q90 solo dei forested pixels dentro 2.5 arcminute cell (forested pixels = AGB > 0)

C_list <- list.files("03.Data/in/dap.ceda.ac.uk.v6/", pattern ="*.tif$", full.names = TRUE, recursive = TRUE)
C_path <- C_list[str_detect(C_list, "SD", negate = TRUE)]
AD_map <- terra::rast("03.Data/in/Sabatini_AD/w3_tile2026.sr1000.for..tif")

vrt_file <- "agb_mosaic.vrt"
terra::vrt(C_path, filename = vrt_file, overwrite = TRUE)
C_map <- terra::rast(vrt_file)
plot(C_map)


C_test <- terra::crop(C_map, terra::ext(-10, 10, -10, 10))

terraOptions(memfrac = 0.3)
system.time(terra::aggregate(
  C_test,
  fact = round(0.0416667 / res(C_test)[1]),
  fun = function(x, na.rm = TRUE) {
    x_notz <- x[x != 0 & !is.na(x)]
    frac <- length(x_notz) / length(x[!is.na(x)])
    if (frac < 0.10 || length(x_notz) == 0) return(NA)
    quantile(x_notz, 0.90)
  },
  na.rm = TRUE,
  filename = "03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q90_for_test.tif",
  overwrite = TRUE
))

#user  system elapsed 
#19.187   0.217  19.335 
file.remove("03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q90_for_test.tif")


terraOptions(memfrac = 0.3)

system.time(terra::aggregate(
  C_map,
  fact = round(0.0416667 / res(C_map)[1]),
  fun = function(x, na.rm = TRUE) {
    x_notz <- x[x != 0 & !is.na(x)]
    frac <- length(x_notz) / length(x[!is.na(x)])
    if (frac < 0.10 || length(x_notz) == 0) return(NA)
    quantile(x_notz, 0.90)
    },
  na.rm = TRUE,
  filename = "03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q90_for_tmp.tif",
  overwrite = TRUE
))

#user   system  elapsed 
#2347.805  965.275 3307.308 
#55 min sui 50 GB #valuta se aumentare memfrac

terra::writeRaster(
  terra::rast("03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q90_for_tmp.tif"),
  filename = "03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q90_for.tif",
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIF=YES"),
  overwrite = TRUE
)

file.remove("03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q90_for_tmp.tif")
rm(vrt_file)
gc()

agb_agg <- terra::rast("03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q90_for.tif")
plot(agb_agg)
res(agb_agg)

terra::resample(
  agb_agg,
  AD_map,
  method = "near",   
  filename = "03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q90_for_final.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES")
)

############# temporal quantile



############## sensitivity analysis doing it with different % and with the mean & median



################## check potential forest mask by Sabatini et al., 2022
AD_map <- terra::rast("03.Data/in/Sabatini_AD/w3_tile_sr1000_for.tif")
pot.for <- terra::rast("03.Data/in/Sabatini_AD/potential_forest_mask/potential_forest.tif")
crs(AD_map)
crs(pot.for)
levels(pot.for)

AD01 <- terra::ifel(!is.na(AD_map), 1, 0)
pot01 <- terra::ifel(is.na(pot.for), 0,
                     terra::ifel(pot.for > 0, 1, 0))
pot01 <- terra::project(pot01, AD01, method = "near")
plot(pot01)
plot(AD01)

compare <- AD01 + 2 * pot01
a <- freq(compare)
a <- a |> mutate(freq = (a$count/sum(a$count))*100)

plot(compare)



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

