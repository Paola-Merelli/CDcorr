#filter df_for  keeping only filtered ecoregions
df_for <- readRDS("03.Data/out/df_for.RDS")
eco_filt <- readRDS("03.Data/out/df_ecor_filtered.RDS")

df_for_final <- df_for |> 
  filter(ECO_NAME %in% eco_filt$ECO_NAME)

dim(df_for_final)
#1704330

length(unique(df_for_final$REALM))
#7 Realms

length(unique(df_for_final$ECO_NAME))
#497 Ecoregions

length(unique(df_for_final$BIOME_NAME))
#14 Biomes

length(unique(df_for_final$ECO_BIOME_))
#53


############# styill need to plot agb ad and logagb distribution after filtering eco

ggplot(df_for_final, aes(x = AGB)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "AGB global (filter_eco)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_for_final$AGB, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
           label = paste0("n (pixels) = ", nrow(df_for_final), "\nmedian = ", 
                          round(median(df_for_final$AGB, na.rm = TRUE), 2)),
           hjust = 1.5,
           vjust = 1.5,
           size = 4,
           color = "gray30"
  ) 


hist(df_for_final$AGB)
hist(df_for_final$AD)



############################################
###########   correlation raw   ############
############################################

cor_ecor <- df_for_final |>
  group_by(ECO_NAME) |>
  summarise(
    n = n(),
    r_spearman = round(cor(AGB, AD, method = "spearman"), digits=3),
    .groups = "drop"
  ) |>
  arrange(n)

cor_ecor_log_p <- df_for_final %>%
  group_by(ECO_NAME) %>%
  summarise(
    n = n(),
    r_log= round(cor(logAGB, AD, method = "pearson"), digits=3),
    .groups = "drop"
  ) |>
  arrange(n)

cor_ecor_log_s <- df_for_final %>%
  group_by(ECO_NAME) %>%
  summarise(
    n = n(),
    r_log= round(cor(logAGB, AD, method = "spearman"), digits=3),
    .groups = "drop"
  ) |>
  arrange(n)

cor_ecor$r_log_spearman <- cor_ecor_log_s$r_log
cor_ecor$r_log_pearson <- cor_ecor_log_p$r_log


df_ecorR <- cor_ecor |> filter(!is.na(r_spearman)) |>
  arrange(n)



##########################################
##########   map r by ecoregion   ########
##########################################

ecoreg <- vect("03.Data/in/Ecoregions_new/Ecoregions2017.shp")
ecoregions <- st_as_sf(ecoreg)
ecoregions_map <- ecoregions %>%
  left_join(df_ecorR, by = "ECO_NAME")


ecorR_map <- ggplot(ecoregions_map) +
  geom_sf(aes(fill = r_spearman), color = NA) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1),
    na.value = "grey85",
    name = "Spearman's r"
  ) +
  labs(title = "AD-AGB by ecoregion (497)",) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank()) +
  coord_sf(crs = "+proj=robin")
ecorR_map
##########################################

##########################################
######## scatterplot by ecoregion ########
##########################################


ecolist <- cor_ecor$ECO_NAME

#try
#ecolist <- ecolist[50:53]

make_plot <- function(eco) {
  df_sub <- df_for_final |> filter(ECO_NAME == eco)
  
  info <- cor_ecor |> filter(ECO_NAME == eco) |> slice(1)
  n_val <- info$n
  r_val <- info$r
  
  ggplot(df_sub, aes(x = AGB, y = AD)) +
    geom_point(alpha = 0.2, size = 0.5, color = "#4a90d9") +
    geom_smooth(method = "gam",formula = y ~ s(x, k = 5), color = "tomato", se = FALSE, linewidth = 0.8) +
    labs(
      title = eco,
      subtitle = sprintf("n = %s | r = %.3f", format(n_val, big.mark = ","), r_val),
      x = "AGB", y = "Alpha Diversity"
    ) +
    theme_minimal(base_size = 11)
}

pdf("try.pdf", width = 6, height = 8)

for (i in seq(1, length(ecolist), by = 2)){
  p1 <- make_plot(ecolist[i])
  if (i+1 <= length(ecolist)){
    p2 <- make_plot(ecolist[i+1])
  } else {
    p2 <- ggplot() + theme_void()
  }
  
  print(p1 / p2)
}

dev.off()

##########################################


##########################################
########  model with climate and lat #####
##########################################


wc_dir <- "03.Data/in/wc2.1"
#dir.create(wc_dir, showWarnings = FALSE)
bio <- geodata::worldclim_global(var = "bio", res = 2.5, path = wc_dir)

names(bio) <- paste0("bio", 1:nlyr(bio))
bioT <- bio[["bio1"]]
bioP <- bio[["bio12"]]
bioTs <- bio[["bio4"]]
bioPs <- bio[["bio15"]]
bio_stk <- c(bioT, bioP, bioTs, bioPs)

clim_means <- terra::extract(
  bio_stk,
  ecoregions,
  fun = mean,
  na.rm = TRUE)

clim_means$ECO_NAME <- ecoregions$ECO_NAME

centr <- terra::centroids(ecoregions)
centr_xy <- terra::crds(centr)
lat <- data.frame(centr_xy)$y
clim_means$lat <- lat

ddf <- merge(df_ecorR, clim_means, by.x = "ECO_NAME", by.y = "ECO_NAME", all.x = TRUE)

#remove ID column
ddf <- ddf[,-5]

saveRDS(ddf, "03.Data/out/df_ecorR_raw.RDS")

########## plot ecoregions' dstribution ( r and r_log)
r <- ggplot(ddf, aes(x = r_spearman)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Ecoregion's r (Spearman)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(ddf$r_spearman, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +  
  annotate("text", x = Inf, y = Inf,
           label = paste0("n = ", nrow(ddf), "\nmedian = ", round(median(ddf$r_spearman), 2)),
           hjust = 1.1, vjust = 1.5, size = 4, color = "gray30")

r_log <- ggplot(ddf, aes(x = r_log_pearson)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Ecoregion's r (Pearson - logAGB)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(ddf$r_log_pearson, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +  
  annotate("text", x = Inf, y = Inf,
           label = paste0("n = ", nrow(ddf), "\nmedian = ", round(median(ddf$r_log_pearson), 2)),
           hjust = 1.1, vjust = 1.5, size = 4, color = "gray30")

r + r_log


###### scatterplot r vs latitude

ggplot(ddf, aes(x = bio15, y = r_spearman)) +
  geom_point(color = "#4a90d9", alpha = 0.6) +
  geom_smooth(method = "gam", color = "#2c5f8a", se = TRUE) +
  labs(
    x = "P seasonality",
    y = "Ecoregion's r (spearman)"
  ) +
  theme_minimal(base_size = 13)


lm <- lm(r_spearman ~ bio1 + bio12, data = ddf)

library(pdp)

pdp_T <- partial(lm, pred.var = "bio1", train = ddf)
plotPartial(pdp_T)

pdp_P <- partial(lm, pred.var = "bio12", train = ddf)
plotPartial(pdp_P)

library(mgcv)

gamT <- gam(r_spearman ~ s(bio1) + s(bio12), data = ddf)

plot(gamT, select = 1, residuals = TRUE)  # partial effect of bio1
plot(gamT, select = 2, residuals = TRUE)  # partial effect of bio12
