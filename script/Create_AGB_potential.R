library(sf)
library(terra)
library(stringr)


######## for now just spatial resampling on a single AGB layer (2017) 
##### resample con q90 su vrt globale, ma q90 solo dei forested pixels dentro 2.5 arcminute cell (forested pixels = AGB > 0)

C_list <- list.files("03.Data/in/dap.ceda.ac.uk.v6/", pattern ="*.tif$", full.names = TRUE, recursive = TRUE)
C_path <- C_list[str_detect(C_list, "SD", negate = TRUE)]
AD_map <- terra::rast("03.Data/in/Sabatini_AD/w3_tile_sr1000_for.tif")

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
