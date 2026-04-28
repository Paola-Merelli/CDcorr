# Sabatini Alpha Diversity
AD_1ha <- rast("03.Data/Sabatini_AlphaDiversity/w3_tile_sr1ha_for.tif")
res(AD_1ha)
plot(AD_1ha)

# esa agb
C_list <- list.files("03.Data/dap.ceda.ac.uk/", pattern ="*.tif$", full.names = TRUE, recursive = TRUE)
C_rast <- C_list[str_detect(C_list, "SD", negate = TRUE)] |>
  lapply(FUN = terra::rast)

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
  "03.Data/AGB_merged_YEAR.tif",
  overwrite = TRUE,
  gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BLOCKXSIZE=512", "BLOCKYSIZE=512")
)


rm(vrt_file)
gc()

#repeat across all years untill you have the saved mosaic raster of each year then upload it as a multilayer spatraster eg agb

############# temporal quantile

agb_p90_years <- quantile(
  agb,
  probs = 0.90,
  na.rm = TRUE,
  filename = "agb_p90_years.tif",
  overwrite = TRUE
)

names(agb_p90_time) <- "agb_p90_time"

############## mask for forest ##################

##########spatial quantile

# Create one unique ID per Sabatini pixel
zone_r <- rast(div)
values(zone_r) <- seq_len(ncell(zone_r))
names(zone_r) <- "zone_id"

zone_poly <- as.polygons(zone_r, na.rm = FALSE)
zone_poly <- st_as_sf(zone_poly)

p90_df <- exact_extract(
  agb_p90_time,
  zone_poly,
  fun = "quantile",
  quantiles = 0.90,
  force_df = TRUE,
  progress = TRUE
)

# Add extracted values back to the polygons
zone_poly$agb_p90_final <- p90_df[[1]]

# Rasterize back to the Sabatini grid
agb_p90_final <- rasterize(
  vect(zone_poly),
  div,
  field = "agb_p90_ts",
  filename = "agb_p90_temporal_spatial.tif",
  overwrite = TRUE
)

names(agb_p90_final) <- "agb_p90_final"




############## sensitivity analysis doing it with different % and with the mean
