###########################################
# global correlation across all gird cells
# on df_for, no ecoregion filter
###########################################

library(dplyr)
library(ggplot2)


df_for <- readRDS("03.Data/out/df_for.RDS")
####### correlazione spearman globale sottocampionando 999 vole 100k pixels
####### modello globale o GAM o glmmTMB sottocampionando 999 volte 100k pixels e usando spatial autocorr come random effect




