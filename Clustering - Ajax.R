library(tidyverse)
library(ggsoccer)
library(ggplot2)

# Data (Ajax vs NEC)
Allshots_ajax <- allshots %>%
  filter(team.name == "Ajax", opponentTeam.name == "NEC") %>%
  slice(1:24)

# Skaler 0-100 koordinater til StatsBomb-banen (120x80)
Allshots_ajax <- Allshots_ajax %>%
  mutate(
    x_sb = x * 120/100,
    y_sb = y * 80/100
  )

# Clustering (kmeans) pr. side baseret på de skalerede koordinater
set.seed(123)

Allshots_ajax_kmeans <- Allshots_ajax %>%
  mutate(side = if_else(x_sb >= 60, "Højre", "Venstre")) %>%
  group_by(side) %>%
  mutate(cluster = kmeans(cbind(x_sb, y_sb), centers = 4)$cluster) %>%
  ungroup()

# Plot 1: skud (uden clustering) på fuld StatsBomb-bane
ggplot(Allshots_ajax) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "black") +
  geom_point(aes(x = x_sb, y = y_sb), color = "red", size = 2, alpha = 0.6) +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80), expand = FALSE) +
  theme_pitch()

# Plot 2: skud med clustering på fuld StatsBomb-bane
ggplot(Allshots_ajax_kmeans) +
  annotate_pitch(dimensions = pitch_statsbomb, colour = "black") +
  geom_point(
    aes(
      x = x_sb,
      y = y_sb,
      color = as.factor(cluster),
      shape = isGoal
    ),
    size = 2,
    alpha = 0.7
  ) +
  coord_fixed(xlim = c(0, 120), ylim = c(0, 80), expand = FALSE) +
  theme_pitch() +
  scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16))

