#### null model and standard effect size 
# I will perform a within-ecoregion permutation null model, so that each ecoregion
# mantains AD and AGB distributions, as well as n of pixels, but the pixel pairing AD-AGB is random
# Is the observed AD–AGB correlation stronger than expected if AD and AGB were randomly associated within that ecoregion?

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)


df_for <- readRDS("03.Data/out/df_for.RDS")
eco_filt <- readRDS("03.Data/out/df_ecor_filtered_stronger.RDS")

df_for_final <- df_for |> 
  filter(ECO_NAME %in% eco_filt$ECO_NAME)


ecorr_null <- function(data, n_perm = 999) {
  
  n_pixels <- nrow(data)
  
  r_obs <- cor(
    data$AD, 
    data$AGB,
    method = "spearman",
    use = "complete.obs"
  )
  
  null_r <- replicate(
    n_perm, {
    cor(
      data$AGB, 
      sample(data$AD), 
      method = "spearman", 
      use = "complete.obs"
    )
  })
  
  null_mean <- mean(null_r, na.rm = TRUE)
  null_sd <- sd(null_r, na.rm= TRUE)

  SES_r <- (r_obs - null_mean) / null_sd
  
  tibble(
    n_pixels = n_pixels,
    r_obs = r_obs,
    null_mean = null_mean,
    null_sd = null_sd,
    SES_r = SES_r,
  )
}

set.seed(123)

df_null_AD <- df_for_final |>
  group_by(ECO_NAME) |>
  group_modify(~ ecorr_null(.x, n_perm = 999)) |>
  ungroup()

#########
##### distribution of null_mean SES_r and null_sd across ecoregions
ggplot(df_null_AD, aes(x = SES_r)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "SES of ecoregions' Spearman r",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_null_AD$SES_r, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
           label = paste0("n (ecoregions) = ", nrow(df_null_AD), "\nmedian = ", 
                          round(median(df_null_AD$SES_r, na.rm = TRUE), 2)),
           hjust = 1.5,
           vjust = 1.5,
           size = 4,
           color = "gray30"
  ) 

ggplot(df_null_AD, aes(x = null_mean)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "ecoregions' Spearman r of the null model (mean)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_null_AD$null_mean, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
           label = paste0("n (ecoregions) = ", nrow(df_null_AD), "\nmedian = ", 
                          round(median(df_null_AD$null_mean, na.rm = TRUE), 2)),
           hjust = 1.5,
           vjust = 1.5,
           size = 4,
           color = "gray30"
  ) 
ggplot(df_null_AD, aes(x = null_sd)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(
    x = "null_sd of ecoregions' null r",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  geom_vline(xintercept = median(df_null_AD$null_sd, na.rm = TRUE),
             linetype = "dotted", color = "tomato", linewidth = 0.8) +
  annotate("text",  x = Inf,  y = Inf,
           label = paste0("n (ecoregions) = ", nrow(df_null_AD), "\nmedian = ", 
                          round(median(df_null_AD$null_sd, na.rm = TRUE), 2)),
           hjust = 1.5,
           vjust = 1.5,
           size = 4,
           color = "gray30"
  ) 


###############
ecoregions_map <- ecoregions %>%
  left_join(df_null_AD, by = "ECO_NAME")

ggplot(ecoregions_map) +
  geom_sf(aes(fill = SES_r), color = NA) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-10, 10),
    oob = scales::squish,
    na.value = "grey85",
    name = "SES of\nSpearman's r"
  ) +
  labs(
    title = "SES of AD–AGB correlation by ecoregion",
    subtitle = "Values truncated to -10, 10"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(crs = "+proj=robin")


ses <- ggplot(df_null_AD, aes(x = n_pixels, y = SES_r)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  theme_minimal() +
  labs(
    x = "Number of pixels per ecoregion log10",
    y = "SES of Spearman's r"
  )

sd <- ggplot(df_null_AD, aes(x = n_pixels, y = null_sd)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  theme_minimal() +
  labs(
    x = "Number of pixels per ecoregion log10",
    y = "SD of null r"
  )
ses + sd

#nel null model semplice ho randomizzato AGB rispetto ad AD all’interno di ogni ecoregione.
#Questo tratta ogni pixel come se fosse indipendente. Nei raster ecologici questo è quasi mai vero, perché pixel vicini sono spazialmente autocorrelati.
#Quindi con tanti pixel il test diventa molto “potente” e può produrre SES enormi anche per correlazioni ecologicamente moderate.

#try with spatial blocking
# aggregate pixels into spatial blocks within ecoregions, 
#calculate AD_AGB mean for each block and compute correlation obs and null model among blocks rather than among individual pixels


# funzione per creare i blocchi
make_spatial_blocks <- function(df,
                                x_col = "x",
                                y_col = "y",
                                eco_col = "ECO_NAME",
                                ad_col = "AD",
                                agb_col = "AGB",
                                crs_in = 4326,
                                crs_out = 6933,
                                block_size_m = 50000,
                                min_pixels_per_block = 1) {
  
  df_clean <- df |>
    mutate(
      ECO_NAME_tmp = .data[[eco_col]],
      AD_tmp = .data[[ad_col]],
      AGB_tmp = .data[[agb_col]]
    ) |>
    filter(
      !is.na(.data[[x_col]]),
      !is.na(.data[[y_col]]),
      !is.na(ECO_NAME_tmp),
      !is.na(AD_tmp),
      !is.na(AGB_tmp)
    )
  
  df_sf <- st_as_sf(
    df_clean,
    coords = c(x_col, y_col),
    crs = crs_in,
    remove = FALSE
  )
  
  df_proj <- st_transform(df_sf, crs_out)
  
  coords_proj <- st_coordinates(df_proj)
  
  df_blocked <- df_clean |>
    mutate(
      x_proj = coords_proj[, 1],
      y_proj = coords_proj[, 2],
      block_size_m = block_size_m,
      block_x = floor(x_proj / block_size_m),
      block_y = floor(y_proj / block_size_m),
      block_id = paste(block_x, block_y, sep = "_")
    ) |>
    group_by(ECO_NAME_tmp, block_id) |>
    summarise(
      AD = mean(AD_tmp, na.rm = TRUE),
      AGB = mean(AGB_tmp, na.rm = TRUE),
      n_pixels_block = n(),
      block_size_m = first(block_size_m),
      .groups = "drop"
    ) |>
    filter(n_pixels_block >= min_pixels_per_block) |>
    rename(ECO_NAME = ECO_NAME_tmp)
  
  df_blocked
}

#funzione per permutare r_null sui blocchi
ecorr_null_blocks <- function(data, n_perm = 999, min_blocks = 10) {
  
  data <- data |>
    filter(!is.na(AD), !is.na(AGB))
  
  n_blocks <- nrow(data)
  
  if(
    n_blocks < min_blocks ||
    length(unique(data$AD)) < 2 ||
    length(unique(data$AGB)) < 2
  ) {
    return(tibble(
      n_blocks = n_blocks,
      r_obs = NA_real_,
      null_mean = NA_real_,
      null_sd = NA_real_,
      SES_r = NA_real_,
    ))
  }
  
  r_obs <- cor(
    data$AD,
    data$AGB,
    method = "spearman",
    use = "complete.obs"
  )
  
  null_r <- replicate(n_perm, {
    cor(
      data$AD,
      sample(data$AGB),
      method = "spearman",
      use = "complete.obs"
    )
  })
  
  null_mean <- mean(null_r, na.rm = TRUE)
  null_sd <- sd(null_r, na.rm = TRUE)
  
  SES_r <- ifelse(
    null_sd == 0 | is.na(null_sd),
    NA_real_,
    (r_obs - null_mean) / null_sd
  )
  
  tibble(
    n_blocks = n_blocks,
    r_obs = r_obs,
    null_mean = null_mean,
    null_sd = null_sd,
    SES_r = SES_r,
  )
}


## try with different block sizes

block_sizes <- c(25000, 50000, 100000)
# 25 km, 50 km, 100 km


set.seed(123)

df_null_blocks <- map_dfr(block_sizes, function(bs) {
  
  message("Running block size: ", bs / 1000, " km")
  
  df_blocks_bs <- make_spatial_blocks(
    df = df_for_final,
    x_col = "x",
    y_col = "y",
    eco_col = "ECO_NAME",
    ad_col = "AD",
    agb_col = "AGB",
    crs_in = 4326,
    block_size_m = bs,
    min_pixels_per_block = 1
  )
  
  df_blocks_bs |>
    arrange(ECO_NAME) |>
    group_by(ECO_NAME) |>
    group_modify(~ ecorr_null_blocks(.x, n_perm = 999, min_blocks = 10)) |>
    ungroup() |>
    mutate(
      block_size_m = bs,
      block_size_km = bs / 1000
    )
})

stats_SES <- df_null_blocks |>
  group_by(block_size_km) |>
  summarise(n_ecoregions = sum(!is.na(SES_r)),
    med = median(SES_r, na.rm = TRUE),
    label = paste0("n = ", n_ecoregions, "\nmedian = ", round(med, 2)),
    .groups = "drop")

ggplot(df_null_blocks, aes(x = SES_r)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1, na.rm = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_vline(data = stats_SES, aes(xintercept = med), linetype = "dotted", color = "tomato", linewidth = 0.8, inherit.aes = FALSE) +
  geom_text(data = stats_SES, aes(x = Inf, y = Inf, label = label), hjust = 1.1, vjust = 1.5, size = 4, color = "gray30", inherit.aes = FALSE) +
  facet_wrap(~ block_size_km, scales = "free_y") +
  labs(x = "SES of ecoregions' Spearman r", y = "Density") +
  theme_minimal(base_size = 13)


stats_nullsd <- df_null_blocks |>
  group_by(block_size_km) |>
  summarise(n_ecoregions = sum(!is.na(null_sd)),
    med = median(null_sd, na.rm = TRUE),
    label = paste0("n = ", n_ecoregions, "\nmedian = ", round(med, 2)),
    .groups = "drop")

ggplot(df_null_blocks, aes(x = null_sd)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1, na.rm = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_vline(data = stats_nullsd, aes(xintercept = med), linetype = "dotted", color = "tomato", linewidth = 0.8, inherit.aes = FALSE) +
  geom_text(data = stats_nullsd, aes(x = Inf, y = Inf, label = label), hjust = 1.1, vjust = 1.5, size = 4, color = "gray30", inherit.aes = FALSE) +
  facet_wrap(~ block_size_km, scales = "free_y") +
  labs(x = "null sd", y = "Density") +
  theme_minimal(base_size = 13)




stats_nullr <- df_null_blocks |>
  group_by(block_size_km) |>
  summarise(n_ecoregions = sum(!is.na(null_mean)),
    med = median(null_mean, na.rm = TRUE),
    label = paste0("n = ", n_ecoregions,"\nmedian = ", round(med, 2)),
    .groups = "drop")

ggplot(df_null_blocks, aes(x = null_mean)) +
  geom_density(fill = "#4a90d9", alpha = 0.4, color = "#2c5f8a", linewidth = 1, na.rm = TRUE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_vline(data = stats_nullr, aes(xintercept = med), linetype = "dotted", color = "tomato", linewidth = 0.8, inherit.aes = FALSE) +
  geom_text(data = stats_nullr, aes(x = Inf, y = Inf, label = label), hjust = 1.1, vjust = 1.5, size = 4, color = "gray30", inherit.aes = FALSE) +
  facet_wrap(~ block_size_km, scales = "free_y") +
  labs(x = "null mean", y = "Density") +
  theme_minimal(base_size = 13)



g_ses <- ggplot(df_null_blocks, aes(x = n_blocks, y = SES_r)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  facet_wrap(~ block_size_km, scales = "free_x") +
  theme_minimal(base_size = 13) +
  labs(
    x = "Number of spatial blocks per ecoregion (log10)",
    y = "SES of Spearman's r",
    )

g_ses


g_nullsd <- ggplot(df_null_blocks, aes(x = n_blocks, y = null_sd)) +
  geom_point(alpha = 0.6) +
  scale_x_log10() +
  facet_wrap(~ block_size_km, scales = "free_x") +
  theme_minimal(base_size = 13) +
  labs(
    x = "Number of spatial blocks per ecoregion (log10)",
    y = "SD of null Spearman's r",
    )

g_nullsd


## n blocks distribution 
ggplot(df_null_blocks, aes(x = factor(block_size_km), y = n_blocks)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_hline(
    yintercept = 10,
    linetype = "dashed",
    color = "tomato",
    linewidth = 0.8
  ) +
  scale_y_log10() +
  labs(
    x = "Block size (km)",
    y = "Number of spatial blocks per ecoregion",
    ) +
  theme_minimal(base_size = 13)



df_class_wide <- df_null_blocks |>
  dplyr::mutate(
    SES_class = dplyr::case_when(
      is.na(SES_r) ~ NA_character_,
      SES_r <= -1.96 ~ "negative_outside_null",
      SES_r >= 1.96 ~ "positive_outside_null",
      TRUE ~ "within_null"
    )
  ) |>
  dplyr::select(ECO_NAME, block_size_km, SES_class) |>
  tidyr::pivot_wider(
    names_from = block_size_km,
    values_from = SES_class,
    names_prefix = "block_"
  )

df_class_wide <- df_class_wide |>
  dplyr::mutate(
    complete_all_sizes = !is.na(block_25) & !is.na(block_50) & !is.na(block_100),
    same_class_all = complete_all_sizes &
      block_25 == block_50 &
      block_50 == block_100
  )

df_class_wide |>
  dplyr::count(complete_all_sizes, same_class_all)

df_sign_wide <- df_null_blocks |>
  dplyr::mutate(
    SES_sign = dplyr::case_when(
      is.na(SES_r) ~ NA_character_,
      SES_r > 0 ~ "positive",
      SES_r < 0 ~ "negative",
      TRUE ~ "zero"
    )
  ) |>
  dplyr::select(ECO_NAME, block_size_km, SES_sign) |>
  tidyr::pivot_wider(
    names_from = block_size_km,
    values_from = SES_sign,
    names_prefix = "block_"
  ) |>
  dplyr::mutate(
    complete_all_sizes = !is.na(block_25) & !is.na(block_50) & !is.na(block_100),
    same_sign_all = complete_all_sizes &
      block_25 == block_50 &
      block_50 == block_100
  )

df_sign_wide |>
  dplyr::filter(complete_all_sizes) |>
  dplyr::summarise(
    n_complete = dplyr::n(),
    n_same_sign = sum(same_sign_all),
    n_changed_sign = sum(!same_sign_all),
    perc_same_sign = 100 * n_same_sign / n_complete
  )

df_reliability <- df_class_wide |>
  dplyr::select(
    ECO_NAME,
    complete_all_sizes,
    same_class_all
  ) |>
  dplyr::left_join(
    df_sign_wide |>
      dplyr::select(ECO_NAME, same_sign_all),
    by = "ECO_NAME"
  ) |>
  dplyr::mutate(
    reliability_class = dplyr::case_when(
      !complete_all_sizes ~ "not comparable across scales",
      same_sign_all & same_class_all ~ "highly robust",
      same_sign_all & !same_class_all ~ "direction robust, strength scale-sensitive",
      !same_sign_all ~ "direction unstable",
      TRUE ~ "check"
    )
  )

df_reliability |>
  dplyr::count(reliability_class)

## map 50 km

df_null_blocks_classified <- df_null_blocks |>
  dplyr::mutate(
    SES_class = dplyr::case_when(
      is.na(SES_r) ~ "not evaluable",
      SES_r <= -1.96 ~ "negative_outside_null",
      SES_r >= 1.96 ~ "positive_outside_null",
      TRUE ~ "within_null"
    ),
    SES_class = factor(
      SES_class,
      levels = c(
        "negative_outside_null",
        "within_null",
        "positive_outside_null",
        "not evaluable"
      )
    )
  )




make_ses_map <- function(size_km) {
  
  df_size <- df_null_blocks_classified |>
    dplyr::filter(block_size_km == size_km)
  
  ecoregions_map_size <- ecoregions |>
    dplyr::left_join(df_size, by = "ECO_NAME")
  
  ggplot(ecoregions_map_size) +
    geom_sf(aes(fill = SES_class), color = NA) +
    scale_fill_manual(
      values = c(
        "negative_outside_null" = "royalblue3",
        "within_null" = "grey90",
        "positive_outside_null" = "firebrick2",
        "not evaluable" = "grey60"
      ),
      na.value = "grey85",
      drop = FALSE,
      name = paste0("SES class\n", size_km, " km")
    ) +
    labs(
      title = "AD–AGB relationship relative to null expectation",
      subtitle = paste0("Block-based null model, ", size_km, " km")
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major = element_line(color = "transparent"),
      axis.text = element_blank(),
      axis.title = element_blank()
    ) +
    coord_sf(crs = "+proj=robin")
}

map_ses_25km <- make_ses_map(25)
map_ses_50km <- make_ses_map(50)
map_ses_100km <- make_ses_map(100)

map_ses_25km
map_ses_50km
map_ses_100km



ecoregions_map_reliability <- ecoregions |>
  dplyr::left_join(df_reliability, by = "ECO_NAME") |>
  dplyr::mutate(
    reliability_class = dplyr::case_when(
      is.na(reliability_class) ~ "not included in analysis",
      TRUE ~ reliability_class
    ),
    reliability_class = factor(
      reliability_class,
      levels = c(
        "highly robust",
        "direction robust, strength scale-sensitive",
        "direction unstable",
        "not comparable across scales",
        "not included in analysis",
        " "
      )
    )
  )

map_reliability <- ggplot(ecoregions_map_reliability) +
  geom_sf(aes(fill = reliability_class), color = NA) +
  scale_fill_manual(
    values = c(
      "highly robust" = "darkgreen",
      "direction robust, strength scale-sensitive" = "goldenrod2",
      "direction unstable" = "tomato3",
      "not comparable across scales" = "grey70",
      "not included in analysis" = "grey85",
      " " = "black"
    ),
    drop = FALSE,
    name = "Reliability"
  ) +
  labs(
    title = "Consistency of SES across block sizes",
    ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(crs = "+proj=robin")

map_reliability


direction_unstable <- ecoregions_map_reliability |>
  filter(reliability_class == "direction unstable")
