library(sf)
library(terra)
library(stringr)


######## ora faccio lo spaziale su un anno solo. 
###### devo provare a scaricare un altro anno, metterlo nella stessa cartella e fare un loop in modo che mi faccia automaticamente gli anni

#####resample con q0.95 su vrt globale 


C_list <- list.files("03.Data/in/dap.ceda.ac.uk/", pattern ="*.tif$", full.names = TRUE, recursive = TRUE)
C_path <- C_list[str_detect(C_list, "SD", negate = TRUE)]
AD_map <- terra::rast("03.Data/in/Sabatini_AlphaDiversity/w3_tile_sr1000_for.tif")

vrt_file <- "agb_mosaic.vrt"
terra::vrt(C_path, filename = vrt_file, overwrite = TRUE)
C_map <- terra::rast(vrt_file)
plot(C_map)

#C_test <- terra::crop(C_map, terra::ext(-10, 10, -10, 10))
#AD_test <- terra::crop(AD_map, terra::ext(-10, 10, -10, 10))

#system.time(terra::resample(C_test, AD_test, method = "q3"))
# 12 sec per 20°x20°

system.time(
  terra::resample(
    C_map,
    AD_map,
    method = "q3",
    filename = "03.Data/out/AGB_mosaic_2017.tif",
    overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES")
  )
)

# 15 min e ca 28G RAM

agb_mosaic_2017 <- terra::rast("03.Data/out/AGB_mosaic_2017.tif")
plot(agb_mosaic_2017)

# x ora ho usato resample perchè ha come custom il method q3. claude dice che aggregate con
# per fare 0.95 mi serve una funzione custom quindi devo usare aggregate 
C_test <- terra::crop(C_map, terra::ext(-10, 10, -10, 10))


system.time(terra::aggregate(C_test, 
                             fact = round(0.041667 / res(C_test)[1]),
                             fun = function(x, na.rm = TRUE) quantile(x, 0.95, na.rm = na.rm),
                             na.rm = TRUE))
                             
# 23 sec per 20°x20°

terraOptions(memfrac = 0.3)
# pupoi provare a aggiungere argomento ncores in aggregate. così ci mette 65 min e ca 110 GB

system.time(terra::aggregate(
  C_map,
  fact = round(0.0416667 / res(C_map)[1]),
  fun = function(x, na.rm = TRUE) quantile(x, 0.95, na.rm = na.rm),
  na.rm = TRUE,
  filename = "03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q95_tmp.tif",
  overwrite = TRUE
))


terra::writeRaster(
  terra::rast("03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q95_tmp.tif"),
  filename = "03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q95.tif",
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES"),
  overwrite = TRUE
)

# Rimuovi il tmp
file.remove("03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q95_tmp.tif")
rm(vrt_file)
gc()

agb_agg <- terra::rast("03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q95.tif")

terra::resample(
  agb_agg,
  AD_map,
  method = "near",   
  filename = "03.Data/out/agb_mosaic_yy/AGB_mosaic_2017_q95.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES")
)

############# temporal quantile



############## sensitivity analysis doing it with different % and with the mean
