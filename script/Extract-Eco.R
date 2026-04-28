####packages ----
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(terra))


sessionInfo()

args <- commandArgs(trailingOnly = T)
input_dir <- args[1]
output <- args[2]
start <- as.numeric(args[3])
end <- as.numeric(args[4])

####load data ----
df.for <- readRDS(file.path(input_dir, "_AD1ha-AGB_for.RDS"))




ecoreg <- vect(file.path("03.Data/Ecoregions/Ecoregions2017.shp"))
dd <- as.data.frame(ecoreg)

#subset
ecoreg_subset <- ecoreg[,c("ECO_NAME", "BIOME_NAME", "REALM", "ECO_BIOME_", "NNH_NAME")]

####Extract ----

if(end > nrow(df_for)){end <- nrow(df_for)}

eco_ex <- terra::extract(ecoreg_subset,
                         df_for[start:end,1:2])

if(nrow(df_for[start:end,]) == nrow(eco_ex)) {
 
  df_out <- cbind(df_for[start:end,], eco_ex)
  
} else{
  df_out <- cbind(
    data.frame(
      x = rep(NA, nrow(eco_ex)),
      y = rep(NA, nrow(eco_ex)),
      AD_1ha  = rep(NA, nrow(eco_ex)),
      AGB = rep(NA, nrow(eco_ex))
    ), eco_ex)
}



saveRDS(df.out, file = output)