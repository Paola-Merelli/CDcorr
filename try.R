#load packages

library(tidyverse)
library(ggplot2)
#fammi un ggplot con palmer penguins
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  labs(title = "Bill Length vs Bill Depth in Palmer Penguins",
       x = "Bill Length (mm)",
       y = "Bill Depth (mm)") +
  theme_minimal() +
  theme(legend.title = element_blank())
