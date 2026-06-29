### trimmed spearman's r cutting extremes by each ecoregion's ad - agb distribution
library(dplyr)

df_for_final <- readRDS("03.Data/out/df_for_final.RDS")

trim <- 0.025
df_trimmed <- df_for_final |>
  group_by(ECO_NAME) |>
  filter(AD >= quantile(AD, trim, na.rm = T),
         AD <= quantile(AD, 1 - trim, na.rm = T),
         AGB >= quantile(AGB, trim, na.rm = T),
         AGB <= quantile(AGB, 1- trim, na.rm = T)) |>
  ungroup()

r_trimmed <- df_trimmed |>
  group_by(ECO_NAME) |>
  summarise(
    n_trimmed = n(),
    r_trimmed = cor(AD, log10(AGB+1), method = "spearman", use = "complete.obs"),
    .groups = "drop") 

df_ecorR_raw <- readRDS("03.Data/out/df_ecorR_raw.RDS")

df_r_trimmed <- df_ecorR_raw |>
  left_join(r_trimmed, by = "ECO_NAME") |>
  mutate(
    delta_r = r_trimmed - r_raw,
    abs_delta_r = abs(r_trimmed - r_raw),
    prc_removed = 100 * (1- n_trimmed/n)) |>
  mutate(
    delta_class = case_when(
      is.na(abs_delta_r) ~ NA_character_,
      abs_delta_r < 0.1 ~ "stable: < 0.1",
      abs_delta_r >= 0.1 ~ "sensitive: >= 0.1",
      ))
saveRDS(df_r_trimmed, "03.Data/out/df_ecorR_trimmed.RDS")

rm(r_trimmed, df_ecorR_raw, df_trimmed, df_for_final)

### plot delta r distribution
############################
distr_deltar <- ggplot(df_r_trimmed, aes(x = delta_r)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Change in Spearman's r after trimming",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_r_trimmed$delta_r, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) 

ggsave(
  filename = "plots/ecorr_trimming/distr_delta_r.png",
  plot = distr_deltar,
  width = 8,
  height = 5,
  dpi = 300
)

distr_deltar_abs <- ggplot(df_r_trimmed, aes(x = abs_delta_r)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Absolute change in Spearman's r after trimming",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_r_trimmed$abs_delta_r, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8)

ggsave(
  filename = "plots/ecorr_trimming/distr_abs_delta_r.png",
  plot = distr_deltar_abs,
  width = 8,
  height = 5,
  dpi = 300
)
######################################

#### map delta r by ecoregion (class, delta r, abs delta r)
###########################################

ecoregions <- vect("03.Data/in/Ecoregions_new/Ecoregions2017.shp")
ecoregions_sf <- st_as_sf(ecoregions)
ecoregions_map <- ecoregions_sf %>%
  left_join(df_r_trimmed, by = "ECO_NAME")

ecor_sensitive_map <- ggplot(ecoregions_map) +
  geom_sf(aes(fill = delta_class), color = NA) +
  scale_fill_manual(
    values = c("stable: < 0.1" = "#4a90d9", "sensitive: >= 0.1" = "tomato"),
    na.value = "grey85",
    name =  expression( "|"* Delta * " r | sensitivity class")
  ) +
  labs(
    title = "Sensitivity of AD–AGB correlation to trimming",
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(crs = "+proj=robin")

ecor_sensitive_map

ggsave(
  filename = "plots/ecorr_trimming/map_delta_class.png",
  plot = ecor_sensitive_map,
  width = 10,
  height = 6,
  dpi = 300
)

ecor_delta_map <- ggplot(ecoregions_map) +
  geom_sf(aes(fill = delta_r), color = NA) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    limits = c(-0.5, 0.5),
    na.value = "grey85",
    name = expression( " "* Delta * "r")
  ) +
  labs(
    title = "Sensitivity of AD–AGB correlation to trimming",
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(crs = "+proj=robin")

ecor_delta_map

ggsave(
  filename = "plots/ecorr_trimming/map_delta_r.png",
  plot = ecor_delta_map,
  width = 10,
  height = 6,
  dpi = 300
)

ecor_abs_delta_map <- ggplot(ecoregions_map) +
  geom_sf(aes(fill = abs_delta_r), color = NA) +
  scale_fill_gradient(
    low = "white",
    high = "red",
    limits = range(ecoregions_map$abs_delta_r, na.rm = TRUE),
    na.value = "grey85",
    name = expression("|" * Delta * "r|")
  ) +
  labs(
    title = "Sensitivity of AD–AGB correlation to trimming",
    ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(crs = "+proj=robin")

ecor_abs_delta_map

ggsave(
  filename = "plots/ecorr_trimming/map_abs_delta_r.png",
  plot = ecor_abs_delta_map,
  width = 10,
  height = 6,
  dpi = 300
)


####################################


### scatterplot by ecoregion of r_sensitive
#######################################

r_sensitive <- filter(df_r_trimmed, delta_class != "Stable: < 0.1")
ecolist <- r_sensitive$ECO_NAME

#try
#ecolist <- ecolist[1:4]

make_plot <- function(eco) {
  df_sub <- df_for |> filter(ECO_NAME == eco)
  
  info <- r_sensitive |> filter(ECO_NAME == eco) |> slice(1)
 
  n_val <- info$n
  r_val <- info$r_raw
  n_trim_val <- info$n_trimmed
  r_trim_val <- info$r_trimmed
  
  ggplot(df_sub, aes(x = logAGB, y = AD)) +
    geom_point(alpha = 0.2, size = 0.5, color = "#4a90d9") +
    geom_smooth(method = "gam",formula = y ~ s(x, k = 5), color = "tomato", se = FALSE, linewidth = 0.8) +
    labs(
      title = eco,
      subtitle = sprintf("raw: n = %s | r = %.3f\nTrimmed: n = %s | r = %.3f", 
                         format(n_val, big.mark = ","), 
                         r_val,
                         format(n_trim_val, big.mark = ","),
                         r_trim_val),
      
      x = "AGB", y = "Alpha Diversity"
    ) +
    theme_minimal(base_size = 11)
}

pdf("r_sensitive_trimming.pdf", width = 6, height = 8)

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

############################################



### plot ecoregions'r dstribution (r_raw vs r_trimmed)
##################
r_raw <- ggplot(df_r_trimmed, aes(x = r_raw)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Ecoregion's r (Spearman)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_r_trimmed$r_raw, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +  
  annotate("text", x = Inf, y = Inf,
           label = paste0("n = ", nrow(df_r_trimmed), "\nmedian = ", round(median(df_r_trimmed$r_raw), 2)),
           hjust = 1.1, vjust = 1.5, size = 4, color = "gray30")

r_trim <- ggplot(df_r_trimmed, aes(x = r_trimmed)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "Ecoregion's r (trimmed)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_r_trimmed$r_trimmed, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +  
  annotate("text", x = Inf, y = Inf,
           label = paste0("n = ", nrow(df_r_trimmed), "\nmedian = ", round(median(df_r_trimmed$r_trimmed), 2)),
           hjust = 1.1, vjust = 1.5, size = 4, color = "gray30")

r_raw + r_trim

ggsave(
  filename = "plots/ecorr_trimming/distr_rtrim.png",
  plot = r_raw + r_trim,
  width = 10,
  height = 5,
  dpi = 300
)
#####################

