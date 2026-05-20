# script to check plots distribution
library(stringr)
library(ggplot2)
library(dplyr)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)


unique(outdata$DB_BIBTEXKEY)
outdata$YEAR <- as.integer(str_extract(outdata$DB_BIBTEXKEY, "\\d{4}"))

for_data <- outdata |>
  filter(isforest == "for")
missing <- sum(is.na(for_data$YEAR))
n_before <- sum(for_data$YEAR < 2007, na.rm = TRUE)
n_after <- sum(for_data$YEAR >= 2007, na.rm = TRUE)
perc_missing <- round((missing / nrow(for_data)) * 100, digits = 2)
perc_before <- round((n_before / nrow(for_data)) * 100, digits = 2)
perc_after <- round((n_after / nrow(for_data)) * 100, digits = 2)
total <- nrow(for_data)


for_data |>
  count(YEAR) |>
  ggplot(aes(x = YEAR, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  annotate("label",
           x = -Inf, y = Inf,
           hjust = -0.05, vjust = 1.5,
           label = paste0("Tot for: ", total, "\n",
                          "NA: ", missing, " (", perc_missing, "%)\n",
                          "Before 2007: ", n_before, " (", perc_before, "%)\n",
                          "After: ", n_after, " (", perc_after, "%)"),
           size = 3.5, fill = "white", color = "black") +
  labs(x = "Year", y = "N plots") +
  scale_x_continuous(breaks = unique(na.omit(for_data$YEAR))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


world <- ne_countries(scale = "medium", returnclass = "sf")


for_data <- for_data |>
  mutate(period = case_when(
    is.na(YEAR)    ~ "Unknown",
    YEAR < 2007    ~ "Before 2007",
    YEAR >= 2007   ~ "2007 and after"
  ))

ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey70", linewidth = 0.2) +
  geom_point(data = for_data,
             aes(x = POINT_X, y = POINT_Y, color = period),  # <-- adatta i nomi delle colonne
             alpha = 0.7, size = 1.5) +
  annotate("label",
           x = -Inf, y = -Inf,
           hjust = -0.05, vjust = -1,
           label = paste0("Tot for: ", total, "\n",
                          "NA: ", missing, " (", perc_missing, "%)\n",
                          "Before 2007: ", n_before, " (", perc_before, "%)\n",
                          "After: ", n_after, " (", perc_after, "%)"),
           size = 3.5, fill = "white", color = "black") +
  scale_color_manual(values = c("Before 2007"    = "#E69F00",
                                "2007 and after" = "#0072B2",
                                "Unknown"        = "grey50")) +
  labs(title = " ", color = " ",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")




