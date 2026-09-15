library(terra)
library(sf)
library(dplyr)

AD_map <- rast("03.Data/in/Sabatini_AD/w3_tile2026.sr1000.for..tif") 
plot(AD_map)

df_for <- readRDS("03.Data/out/df_for.RDS")
vt <- vect(df_for[, c("x", "y")], geom = c("x", "y"), crs = crs(AD_map))
data_mask <- rasterize(vt, AD_map, field = 1, background = 0)
plot(data_mask)


pot_for <- rast("03.Data/in/Sabatini_AD/potential_forest_mask/potential_forest.tif")
levels(pot_for)
l <- levels(pot_for)[[1]]
m <- l[l$status_txt == "No Data", 1]
pot_mask <- classify(pot_for, cbind(m, 0), others = 1) |> terra::project(AD_map, method = "near")
res(pot_mask)
plot(pot_mask)
#writeRaster(pot_mask, "03.Data/out/pot_forest_mask.tif")



ecoregions <- vect("03.Data/in/Ecoregions_new/Ecoregions2017.dbf")
df <- as.data.frame(ecoregions)
eco_rast <- terra::rasterize(ecoregions, pot_mask, field = "ECO_NAME")

#area of each forest pixel in hectares
A_pix_ha <- cellSize(pot_mask, unit= "ha", mask = FALSE)
# total area in each ecoregion
A_eco <- zonal(A_pix_ha, eco_rast, fun = "sum", na.rm = TRUE)
# area only of forested pixels
A_for_ha <- A_pix_ha * pot_mask
#forested area in each ecoregion
A_eco_for <- zonal(A_for_ha, eco_rast, fun = "sum", na.rm = TRUE)
#calculate number of obs for each ecoregion
n_obs <- zonal(data_mask, eco_rast, fun = "sum", na.rm = TRUE)

eco_for_df <- A_eco |>
  rename(ECO_NAME = 1, total_ha = 2) |>
  left_join(A_eco_for |>
              rename(ECO_NAME = 1, forest_ha = 2),
            by = "ECO_NAME") |>
  left_join(n_obs |>
              rename(ECO_NAME = 1, n_obs = 2),
            by = "ECO_NAME") |>
  mutate(forest_perc = (forest_ha/total_ha) * 100)

eco_filt_or <- eco_for_df |>
  filter(forest_ha >= 100000 | forest_perc >= 10)

eco_filt_and <- eco_for_df |>
  filter(forest_ha >= 100000 & forest_perc >= 10)

eco_diff <- anti_join(eco_filt_or, eco_filt_and, by = "ECO_NAME")

eco_filt_controlled <- eco_for_df |>
  filter((forest_ha >= 100000 | forest_perc >= 10) & n_obs >= 50)

#saveRDS(eco_filt_controlled, "03.Data/out/filtered_ecoregions.RDS")


#df_for <- readRDS("03.Data/out/df_for.RDS")

df_for_final <- df_for |> 
  filter(ECO_NAME %in% eco_filt_controlled$ECO_NAME)

saveRDS(df_for_final, "03.Data/out/df_for_final.RDS")

dim(df_for_final)
#4424776
length(unique(df_for_final$REALM))
#8 Realms
length(unique(df_for_final$ECO_NAME))
#653
length(unique(df_for_final$BIOME_NAME))
#15 Biomes
length(unique(df_for_final$ECO_BIOME_))
#58
