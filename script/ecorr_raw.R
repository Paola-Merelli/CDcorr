# this basically becomes the script of the global correlation and set up for intra ecoregion
# correlation by filtering ecoregions, creating df_for_final and trying corr within ecoregions raw without null model


# global correlation across all gird cells, on df_for, no ecoregion filter

library(dplyr)
library(ggplot2)


df_for <- readRDS("03.Data/out/df_for.RDS")

#scatterplot AD AGB global

scat_log <- ggplot(df_for, aes(x = log10(AD), y = logAGB)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log10", name = "N pixels \n (log10)", option = "mako") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5),
              color = "#D55E00", se = FALSE) +
  labs(x = "Species richness (log10)", y = "Aboveground biomass (log10)") +
  theme_minimal(base_size = 13)
scat_log

ggsave(
  filename = "plots/eco_Rs_raw/scatt_SR-AGB_global_log_new.png",
  plot = scat_log,
  width = 8,
  height = 6,
  dpi = 300)

# model global correlation

mod <- lm(logAGB ~ log10(AD), data = df_for)
summary(mod)

# estrazione dei singoli valori
slope    <- coef(mod)[["log10(AD)"]]
intercept <- coef(mod)[["(Intercept)"]]
r2       <- summary(mod)$r.squared
pval     <- summary(mod)$coefficients["log10(AD)", "Pr(>|t|)"]

slope
r2
pval

sprintf("slope = %.3f, R² = %.3f, p %s",
        slope,
        r2,
        ifelse(pval < 0.001, "< 0.001", paste0("= ", signif(pval, 2))))

spearman_global <- cor.test(df_for$AD, df_for$logAGB, method = "spearman")
spearman_global$estimate  # rho
spearman_global$p.value


#scatterplot ma colorato per biomi - da definire
################---

#Aggregazione a griglia più larga 1°
target_res <- 1  # gradi

df_agg <- df_for |>
  mutate(
    x_bin = floor(x / target_res) * target_res + target_res / 2,
    y_bin = floor(y / target_res) * target_res + target_res / 2
  ) |>
  group_by(x_bin, y_bin) |>
  summarise(
    AD = mean(AD, na.rm = TRUE),
    logAGB = mean(logAGB, na.rm = TRUE),
    BIOME_NAME = names(sort(table(BIOME_NAME), decreasing = TRUE))[1],  # bioma dominante
    n_px = n(),  # utile per controllare quante celle originali entrano in ogni bin
    .groups = "drop"
  )

scat_biome <- ggplot(df_agg, aes(x = log10(AD), y = logAGB, color = BIOME_NAME)) +
  geom_point(alpha = 0.7, size = 1.4) +
  scale_color_viridis_d(name = "Bioma", option = "turbo") +
  labs(x = "Species richness (log10)", y = "Aboveground biomass (log10)") +
  theme_minimal(base_size = 13) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))
scat_biome

scat_facet <- ggplot(df_agg, aes(x = log10(AD), y = logAGB, color = BIOME_NAME)) +
  geom_point(alpha = 0.5, size = 0.8, show.legend = FALSE) +
  facet_wrap(~ BIOME_NAME, ncol = 4) +
  scale_color_viridis_d(option = "turbo") +
  labs(x = "Species richness (log10)", y = "Aboveground biomass (log10)") +
  theme_minimal(base_size = 11)
scat_facet


df_plot <- df_agg |> filter(BIOME_NAME != "N/A")

biome_pal <- c(
  "Boreal Forests/Taiga" = "#4B0082",
  "Temperate Broadleaf & Mixed Forests" = "#228B22",
  "Temperate Conifer Forests" = "#556B2F",
  "Tropical & Subtropical Coniferous Forests" = "#FF8C00",
  "Tropical & Subtropical Dry Broadleaf Forests" = "#FF6347",
  "Tropical & Subtropical Moist Broadleaf Forests" = "#8B0000"
)

df_selected <- df_plot |> filter(BIOME_NAME %in% names(biome_pal))

scat_cluster <- ggplot() +
  geom_point(data = df_plot, aes(x = log10(AD), y = logAGB),
             color = "grey80", alpha = 0.15, size = 0.5) +
  geom_point(data = df_selected, aes(x = log10(AD), y = logAGB, color = BIOME_NAME),
             alpha = 0.25, size = 0.6) +
  stat_ellipse(data = df_selected, aes(x = log10(AD), y = logAGB, color = BIOME_NAME, group = BIOME_NAME),
               level = 0.68, linewidth = 0.9) +
  scale_color_manual(values = biome_pal, name = "Bioma") +
  labs(x = "Species richness (log10)", y = "Aboveground biomass (log10)") +
  theme_minimal(base_size = 13) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))
scat_cluster


scat_density <- ggplot() +
  geom_point(data = df_plot, aes(x = log10(AD), y = logAGB),
             color = "grey85", alpha = 0.15, size = 0.4) +
  geom_density_2d(data = df_selected,
                  aes(x = log10(AD), y = logAGB, color = BIOME_NAME),
                  contour_var = "ndensity", breaks = 0.2,  # un solo contorno = "core" del cluster
                  linewidth = 1) +
  scale_color_manual(values = biome_pal, name = "Bioma") +
  labs(x = "Species richness (log10)", y = "Aboveground biomass (log10)") +
  theme_minimal(base_size = 13) +
  guides(color = guide_legend(override.aes = list(linewidth = 2)))
scat_density


scat_facet <- ggplot() +
  geom_point(data = df_plot, aes(x = log10(AD), y = logAGB),
             color = "grey85", alpha = 0.15, size = 0.4) +
  geom_point(data = df_selected, aes(x = log10(AD), y = logAGB, color = BIOME_NAME),
             alpha = 0.3, size = 0.6) +
  geom_density_2d(data = df_selected,
                  aes(x = log10(AD), y = logAGB, color = BIOME_NAME),
                  contour_var = "ndensity", breaks = 0.4, linewidth = 0.8) +
  scale_color_manual(values = biome_pal, guide = "none") +
  facet_wrap(~ BIOME_NAME, ncol = 3) +
  labs(x = "Species richness (log10)", y = "Aboveground biomass (log10)") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold", size = 9))
scat_facet

################---



#### forse qui potrei mettere ecoregion screening 



#correlation within ecoregions raw
#########---

df_for_final <- readRDS("03.Data/out/df_for_final.RDS")

eco_Rs_raw <- df_for_final |>
  group_by(ECO_NAME) |>
  summarise(
    n = n(),
    rs_raw = round(cor(logAGB, AD, method = "spearman"), digits=3),
    .groups = "drop"
  ) |>
  arrange(n)


########## plot ecoregions' r_raw dstribution (r_log)
r <- ggplot(eco_Rs_raw, aes(x = rs_raw)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Ecoregion's r (Spearman)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(eco_Rs_raw$rs_raw, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +  
  annotate("text", x = Inf, y = Inf,
           label = paste0("n = ", nrow(eco_Rs_raw), "\nmedian = ", round(median(eco_Rs_raw$rs_raw), 2)),
           hjust = 1.1, vjust = 1.5, size = 4, color = "gray30")

ggsave(
  filename = "plots/eco_Rs_raw/distr_rs_raw.png",
  plot = r,
  width = 8,
  height = 5,
  dpi = 300)



### aggiungi plot r_raw vs n_pixles x ecoregion and maybe map of ecoregions'r_raw
# WHAT ABOUT all those diagnostic plots presentend to fms at the "presentazione brutta" meeting? they might be needed for supplementary


######## scatterplot by ecoregion 
##########################################


ecolist <- eco_Rs_raw$ECO_NAME


make_plot <- function(eco) {
  df_sub <- df_for_final |> filter(ECO_NAME == eco)
  
  info <- eco_Rs_raw |> filter(ECO_NAME == eco) |> slice(1)
  n_val <- info$n
  r_val <- info$r
  
  ggplot(df_sub, aes(x = AGB, y = AD)) +
    geom_point(alpha = 0.2, size = 0.5, color = "#4a90d9") +
    geom_smooth(method = "gam",formula = y ~ s(x, k = 5), color = "tomato", se = FALSE, linewidth = 0.8) +
    labs(
      title = eco,
      subtitle = sprintf("n = %s | r = %.3f", format(n_val, big.mark = ","), r_val),
      x = "AGB", y = "Sr"
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

