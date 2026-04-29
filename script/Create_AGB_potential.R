library(sf)
library(terra)
library(parallel)

###### devo provare a scaricare un altro anno, metterlo nella stessa cartella e fare un loop in modo che mi faccia automaticamente gli anni 
######## provare a parallelizzare i tiles

# Sabatini Alpha Diversity
AD_1ha <- rast("03.Data/in/Sabatini_AlphaDiversity/w3_tile_sr1ha_for.tif")
res(AD_1ha)
plot(AD_1ha)

# esa agb
C_list <- list.files("03.Data/in/dap.ceda.ac.uk/", pattern ="*.tif$", full.names = TRUE, recursive = TRUE)
C_rast <- C_list[str_detect(C_list, "SD", negate = TRUE)] |>
  lapply(FUN = terra::rast)

C_res <- lapply(seq_along(C_rast), function(i) {
  out <- paste0("03.Data/out/agb_resampled/res_", i, ".tif")
  C_rast[[i]] |>
    terra::resample(AD_1ha, method "q3") |>
    terra::writeraster(out, overwrite = TRUE)
  gc()
  out
})

vrt_file <- "agb_mosaic.vrt"
terra::vrt(unlist(C_res), filename = vrt_file, overwrite = T)
C_map <- terra::rast(vrt_file)
plot(C_map)

terra::writeRaster(
  C_map,
  "03.Data/out/agb_mosaic_yy/AGB_mosaic_2017.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BLOCKXSIZE=512", "BLOCKYSIZE=512")
)

unlink(unlist(C_res))
unlink(vrt_file)
gc()

############# temporal quantile



############## sensitivity analysis doing it with different % and with the mean
