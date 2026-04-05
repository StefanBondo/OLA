# -----------------------------
# Opgave 6.1 - Top 10 kvinder
# -----------------------------

#HENT OPGAVE 5 FØR DET VIRKER

# Lav spiller-niveau data (kun kvinder)
player_stats_women <- freeze_res %>%
  filter(gender == "Women") %>%
  group_by(player.name) %>%
  summarise(
    shots = n(),
    total_xg = sum(shot.statsbomb_xg, na.rm = TRUE),
    mean_xg = mean(shot.statsbomb_xg, na.rm = TRUE),
    ego_rate = mean(has_better_option, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(shots >= 5) %>%  # fjern spillere med få skud
  mutate(
    offensive_score = total_xg + shots * 0.1
  ) %>%
  arrange(desc(offensive_score))

top10_women <- player_stats_women %>% slice(1:10)

print(top10_women)

p_top10 <- ggplot(top10_women, aes(x = reorder(player.name, offensive_score), y = offensive_score)) +
  geom_col(fill = "#d81b60") +
  coord_flip() +
  labs(
    title = "Top 10 offensive spillere (kvinder)",
    subtitle = "Baseret på xG og skud",
    x = NULL,
    y = "Offensiv score"
  )

print(p_top10)


##### DEL 2 #####
##### ===== OPGAVE 6.3 – TRACKINGDATA ===== #####

library(jsonlite)
library(dplyr)
library(ggplot2)
library(deldir)

##### DATA #####
tracking <- read.csv("vbob.csv")
colnames(tracking) <- make.names(colnames(tracking))

meta <- fromJSON("vbob-meta.json")


##### 🔹 SPILLERDATA #####
home <- meta$homePlayers[1:11, ] %>%
  select(name, ssiId) %>%
  mutate(team = "Vejle BK")

away <- meta$awayPlayers[1:11, ] %>%
  select(name, ssiId) %>%
  mutate(team = "Odense BK")

players_meta <- bind_rows(home, away)


##### 🔹 FUNKTION #####
frame_to_points <- function(frame_row, players_meta) {
  
  vals <- unlist(frame_row)
  vals <- suppressWarnings(as.numeric(vals))
  vals <- vals[!is.na(vals)]
  
  coords <- tail(vals, 44)
  
  x <- coords[seq(1, 44, by = 2)]
  y <- coords[seq(2, 44, by = 2)]
  
  data.frame(
    x = x,
    y = y,
    player_name = players_meta$name,
    team = players_meta$team
  )
}


##### 🔹 FIND BEDSTE FRAME #####
best_frame <- NA
max_players <- 0

for(i in seq(1, nrow(tracking), by = 200)) {
  pts <- frame_to_points(tracking[i, ], players_meta)
  
  count <- sum(pts$x > 30)
  
  if(count > max_players) {
    max_players <- count
    best_frame <- i
  }
}

best_frame
max_players


##### 🔹 LAV POINTS #####
points <- frame_to_points(tracking[best_frame, ], players_meta)


##### 🔹 DELAUNAY #####
tri <- deldir(points$x, points$y)

ggplot() +
  annotate_pitch(
    dimensions = pitch_impect,
    fill = "#1b5e20",
    colour = "white"
  ) +
  geom_segment(data = tri$dirsgs,
               aes(x = x1, y = y1, xend = x2, yend = y2),
               color = "white") +
  geom_point(data = points,
             aes(x = x, y = y),
             color = "yellow", size = 3) +
  theme_pitch()


##### VORONOI (AREAL) #####
tiles <- tile.list(tri)

points$area <- sapply(tiles, function(tile) tile$area)


##### FILTRÉR TIL FELT #####
field_players <- points %>%
  filter(x > 35)


##### TOP 5 SPILLERE #####
top_field <- field_players %>%
  arrange(desc(area)) %>%
  head(5)

top_field <- field_players %>% 
  mutate(area_pct = area / sum(area)*100)

top_field <- top_field %>% 
  mutate(label = paste(player_name,"(", team,")" ))

top_field


##### 🔹 SØJLEDIAGRAM #####
ggplot(top_field,
       aes(x = reorder(label, area_pct), y = area_pct)) +
  geom_col(fill = "red") +
  geom_text(aes(label = paste0(round(area_pct,1), "%")),
            hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Spillere med procentvis mest plads i offensiven",
    x = "",
    y = "Andel (%)"
  )



#### Tre plots ####

f1 <- 101122
f2 <- 120726
f3 <- 120750

# VBOB/Impect: koordinater er centreret på banens midte (som pitch_impect),
# ikke 0–105 / 0–68 som StatsBomb. Filtrér derfor med meta-banemål.
halfL <- meta$pitchLength / 2
halfW <- meta$pitchWidth / 2

#### Situation 1 ####
pts1 <- frame_to_points(tracking[f1, ], players_meta) %>%
  filter(x >= -halfL, x <= halfL,
         y >= -halfW, y <= halfW)

#### Situation 2 ####
pts2 <- frame_to_points(tracking[f2, ], players_meta) %>%
  filter(x >= -halfL, x <= halfL,
         y >= -halfW, y <= halfW)

#### Situation 3 ####
pts3 <- frame_to_points(tracking[f3, ], players_meta) %>%
  filter(x >= -halfL, x <= halfL,
         y >= -halfW, y <= halfW)

plot_delaunay <- function(pts, titel = "") {
  rw <- c(-halfL, halfL, -halfW, halfW)
  tri <- deldir(pts$x, pts$y, rw = rw)
  
  ggplot() +
    annotate_pitch(
      dimensions = pitch_impect,
      fill = "#1b5e20",
      colour = "white"
    ) +
    geom_segment(
      data = tri$dirsgs,
      aes(x = x1, y = y1, xend = x2, yend = y2),
      color = "white",
      linewidth = 0.5
    ) +
    geom_point(
      data = pts,
      aes(x, y, fill = team),
      shape = 21,
      color = "black",
      size = 5
    ) +
    ggtitle(titel) +
    theme_pitch()
}

plot_delaunay(pts1, "Før situation")
plot_delaunay(pts2, "Angreb")
plot_delaunay(pts3, "Angreb2")
