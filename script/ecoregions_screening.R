library(terra)
library(sf)
library(dplyr)

AD <- rast("03.Data/in/Sabatini_AlphaDiversity/w3_tile_sr1000_for.tif")
forest_mask <- !is.na(AD)

ecoregions <- vect("03.Data/in/Ecoregions_new/Ecoregions2017.dbf")
df <- as.data.frame(ecoregions)
eco_rast <- terra::rasterize(ecoregions, forest_mask, field = "ECO_NAME")

A_pix_ha <- cellSize(AD, unit= "ha", mask = FALSE)

A_eco <- zonal(A_pix_ha, eco_rast, fun = "sum", na.rm = TRUE)

A_for_ha <- A_pix_ha * forest_mask

A_eco_for <- zonal(A_for_ha, eco_rast, fun = "sum", na.rm = TRUE)

eco_for_df <- A_eco |>
  rename(ECO_NAME = 1, total_ha = 2) |>
  left_join(A_eco_for |>
              rename(ECO_NAME = 1, forest_ha = 2),
            by = "ECO_NAME") |>
  mutate(forest_perc = (forest_ha/total_ha) * 100) |>
  arrange(desc(forest_perc))


eco_filt_pc <- eco_for_df |>
  filter(forest_perc >= 10)
#380

eco_filt_ha <- eco_for_df |>
  filter(forest_ha >= 100000)
#472

eco_filt_pc_ha <- eco_for_df |>
  filter(forest_ha >= 100000 & forest_perc >= 10)
#355

