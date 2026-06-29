library(terra)
library(sf)
library(dplyr)

AD <- rast("03.Data/in/Sabatini_AlphaDiversity/w3_tile_sr1000_for.tif")
forest_mask <- !is.na(AD)

ecoregions <- vect("03.Data/in/Ecoregions_new/Ecoregions2017.dbf")
df <- as.data.frame(ecoregions)
eco_rast <- terra::rasterize(ecoregions, forest_mask, field = "ECO_NAME")

# Calculate the area of each pixel in hectares
A_pix_ha <- cellSize(AD, unit= "ha", mask = FALSE)
# calculate area for each ecoregion
A_eco <- zonal(A_pix_ha, eco_rast, fun = "sum", na.rm = TRUE)
# calculate area only of forested pixels
A_for_ha <- A_pix_ha * forest_mask
#calculate area of forested pixels for each ecoregion
A_eco_for <- zonal(A_for_ha, eco_rast, fun = "sum", na.rm = TRUE)
#calculate number of forested pixels for each ecoregion
n_pix_for <- zonal(forest_mask, eco_rast, fun = "sum", na.rm = TRUE)

eco_for_df <- A_eco |>
  rename(ECO_NAME = 1, total_ha = 2) |>
  left_join(A_eco_for |>
              rename(ECO_NAME = 1, forest_ha = 2),
            by = "ECO_NAME") |>
  left_join(n_pix_for |>
              rename(ECO_NAME = 1, n_pix_for = 2),
            by = "ECO_NAME") |>
  mutate(forest_perc = (forest_ha/total_ha) * 100) |>
  arrange(desc(forest_perc))

eco_filt_or <- eco_for_df |>
  filter(forest_ha >= 100000 | forest_perc >= 10)

eco_filt_and <- eco_for_df |>
  filter(forest_ha >= 100000 & forest_perc >= 10)

eco_diff <- anti_join(eco_filt_or, eco_filt_and, by = "ECO_NAME")

eco_filt_controlled <- eco_for_df |>
  filter((forest_ha >= 100000 | forest_perc >= 10) & n_pix_for >= 50)

saveRDS(eco_filt_controlled, "03.Data/out/df_ecor_filtered.RDS")
