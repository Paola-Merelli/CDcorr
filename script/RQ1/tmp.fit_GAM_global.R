## questo script verrà lanciato da bash perchè il server ha OpenBLAS 0.3.20 e una CPU Xeon w9-3475X
## OpenBLAS sceglie in automatico un kernel non adatto a questa CPU
# quindi l'algebra lineare sbaglia in silenzio senza dare errori ma restituendo modelli insensati (intercetta 1e29, R quadro negativo, edf 1)
# un modello su dati smulati falliva allo stesso modo mentre con OPENBLAS_CORETYPE = HASWELL dava il risultato giusto
# ho aggiunto da terminale export OPENBLAS_CORETYPE=HASWELL a ~/.profile e ~/.bashrc. Così ogni nuova sessione SSH e ogni R o Rscript lanciato da terminale usa il kernel giusto
# quindi ho salvato questo script nello script folder come fit_GAM_global
# poi da bash apro una sessione tmux e lancio lo script e salvo i risultati dei modelli come .RDS in "/03.Data/out/GAM_global/"
# tmux new -s gam_global
#cd /BIGDATA1/r_projects/paola.merelli2/R/CB
#OPENBLAS_CORETYPE=HASWELL Rscript script/fit_GAM_global.R
# exit

library(mgcv)
library(dplyr)

cat("OPENBLAS_CORETYPE =", Sys.getenv("OPENBLAS_CORETYPE"), "\n")

if (Sys.getenv("OPENBLAS_CORETYPE") != "HASWELL") {
  stop("Wrong OpenBLAS core type")
}

df_for <- readRDS(
  "/BIGDATA1/r_projects/paola.merelli2/R/CB/03.Data/out/df_for.RDS"
)

df_for <- df_for |>
  rename(
    lon = x,
    lat = y
  )

#M0 <- bam(
#  log(AD) ~ AGB,
#  data = df_for,
#  family = gaussian(),
#  method = "fREML"
#)

#Mspace16 <- mgcv::bam(
#  log(AD) ~ s(lat, lon, bs = "sos", k = 1600),
#  data = df_for,
#  family = gaussian(),
#  method = "fREML",
#  discrete = TRUE
#)

Mfulllog_logAGB <- mgcv::bam(
  log(AD) ~ log(AGB) + s(lat, lon, bs = "sos", k = 1600),
  data = df_for,
  family = gaussian(),
  method = "fREML",
  discrete = TRUE
)


#saveRDS(M0,     "03.Data/out/GAM_global/M0_log.rds")
#saveRDS(Mspace16, "03.Data/out/GAM_global/Mspacelog_k16.rds")
saveRDS(Mfulllog_logAGB,  "03.Data/out/GAM_global/Mfulllog_logAGB.rds")



