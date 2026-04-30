library(sf)
library(terra)
library(parallel)

# sono ancora indecisa se per la biomassa potenziale sia meglio fare prima temporale e poi spaziale o viceversa
# mi sembra piu sensato fare prima il temporale, ma sembra infattibile in termini di memoria (10 raster globali, uno per anno a 100m res)

 

######## ora faccio lo spaziale su un anno solo. 
###### devo provare a scaricare un altro anno, metterlo nella stessa cartella e fare un loop in modo che mi faccia automaticamente gli anni

#####resample con method q3 su vrt globale 


C_list <- list.files("03.Data/in/dap.ceda.ac.uk/", pattern ="*.tif$", full.names = TRUE, recursive = TRUE)
C_path <- C_list[str_detect(C_list, "SD", negate = TRUE)]
AD_1ha <- terra::rast("03.Data/in/Sabatini_AlphaDiversity/w3_tile_sr1ha_for.tif")

vrt_file <- "agb_mosaic.vrt"
terra::vrt(C_path, filename = vrt_file, overwrite = TRUE)
C_map <- terra::rast(vrt_file)
plot(C_map)

#C_test <- terra::crop(C_map, terra::ext(-10, 10, -10, 10))
#AD_test <- terra::crop(AD_1ha, terra::ext(-10, 10, -10, 10))

#system.time(terra::resample(C_test, AD_test, method = "q3"))
# 12 sec per 20°x20°

system.time(
  terra::resample(
    C_map,
    AD_1ha,
    method = "q3",
    filename = "03.Data/out/AGB_mosaic_2017.tif",
    overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES")
  )
)

# 15 min e ca 28G RAM

agb_mosaic_2017 <- terra::rast("03.Data/out/AGB_mosaic_2017.tif")
plot(agb_mosaic_2017)

############### devo ancora capire se è meglio aggregate o resample.
# x ora ho usato resample perchè ha come custom il method q3. claude dice che aggregate con
# funzione custom per il quantile è piu rischioso comunque fx sarebbe: 

terra::aggregate(
  C_map,
  fact = round(0.0416667 / res(C_map)[1]),
  fun = function(x, na.rm = TRUE) quantile(x, 0.75, na.rm = na.rm),
  na.rm = TRUE,
  filename = "03.Data/out/AGB_mosaic_2017.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES")
)

############# temporal quantile



############## sensitivity analysis doing it with different % and with the mean
