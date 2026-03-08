#load packages
library(tidyverse)
library(ggplot2)
library(palmerpenguins)

#fammi un ggplot con palmer penguins
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  labs(title = "Bill Length vs Bill Depth in Palmer Penguins",
       x = "Bill Length (mm)",
       y = "Bill Depth (mm)") +
  theme_minimal() +
  theme(legend.title = element_blank())

#fammi un altro plotcon palmer penguins
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  labs(title = "Flipper Length vs Body Mass in Palmer Penguins",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)") +
  theme_minimal() +
  theme(legend.title = element_blank())
