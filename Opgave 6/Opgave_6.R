# =========================================================
# Opgave 6 - Finaliseret version
# =========================================================
# Denne fil gør to ting:
# 1) Opgave 6.1: Top 10 offensive spillere (kvinder)
#    - kræver objektet `freeze_res` fra Opgave 5
#    - hvis det ikke ligger i miljøet, prøves freeze_res.rds
# 2) Opgave 6.3: Trackinganalyse for VB - OB
#    - læser vbob.csv korrekt uden header
#    - mapper spillere dynamisk via SSI-id (også efter udskiftninger)
#    - bruger samme tre situationsframes som i den uploadede fil
# =========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(jsonlite)
  library(ggsoccer)
  library(deldir)
})

# ---------------------------------------------------------
# Opgave 6.1 - Top 10 kvinder
# ---------------------------------------------------------

if (!exists("freeze_res") && file.exists("freeze_res.rds")) {
  freeze_res <- readRDS("freeze_res.rds")
}

if (exists("freeze_res")) {
  required_cols <- c(
    "gender",
    "player.name",
    "shot.statsbomb_xg",
    "has_better_option"
  )

  missing_cols <- setdiff(required_cols, colnames(freeze_res))
  if (length(missing_cols) > 0) {
    warning(
      "freeze_res mangler disse kolonner: ",
      paste(missing_cols, collapse = ", "),
      ". Opgave 6.1 springes over."
    )
  } else {
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
      filter(shots >= 5) %>%
      mutate(offensive_score = total_xg + 0.1 * shots) %>%
      arrange(desc(offensive_score))

    top10_women <- player_stats_women %>%
      slice_head(n = 10)

    print(top10_women)

    p_top10 <- ggplot(
      top10_women,
      aes(x = reorder(player.name, offensive_score), y = offensive_score)
    ) +
      geom_col(fill = "#d81b60") +
      coord_flip() +
      labs(
        title = "Top 10 offensive spillere (kvinder)",
        subtitle = "Baseret på xG og antal skud",
        x = NULL,
        y = "Offensiv score"
      ) +
      theme_minimal()

    print(p_top10)
  }
} else {
  message(
    "Opgave 6.1 blev ikke kørt: objektet 'freeze_res' blev ikke fundet. ",
    "Kør Opgave 5 først eller gem resultatet som freeze_res.rds."
  )
}

# ---------------------------------------------------------
# Opgave 6.3 - Trackingdata
# ---------------------------------------------------------

tracking <- read.csv(
  "vbob.csv",
  header = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

meta <- fromJSON("vbob-meta.json")
pitch_dims <- pitch_impect

# Sikr center-origo, hvis objektet ikke allerede har origin_x/origin_y.
if (is.null(pitch_dims$origin_x)) pitch_dims$origin_x <- -pitch_dims$length / 2
if (is.null(pitch_dims$origin_y)) pitch_dims$origin_y <- -pitch_dims$width / 2

# Giv de første kolonner meningsfulde navne.
colnames(tracking)[1:2] <- c("frame_idx", "game_clock")
tracking$frame_idx <- as.integer(tracking$frame_idx)
tracking$game_clock <- suppressWarnings(as.numeric(tracking$game_clock))

# Tracking-filen har denne struktur pr. række:
# 1: frame_idx
# 2: game_clock
# 3:13   home SSI-id
# 14:24  home x
# 25:35  home y
# 36:46  away SSI-id
# 47:57  away x
# 58:68  away y
# 69:70  ball x/y (kan være tomme)

player_lookup <- bind_rows(
  meta$homePlayers %>%
    transmute(ssiId, player_name = name, team = "Vejle BK"),
  meta$awayPlayers %>%
    transmute(ssiId, player_name = name, team = "Odense BK")
) %>%
  distinct(ssiId, .keep_all = TRUE)

get_frame_row <- function(frame_id, tracking) {
  row <- tracking %>%
    filter(frame_idx == frame_id)

  if (nrow(row) != 1) {
    stop("Kunne ikke finde præcis én række for frame_idx = ", frame_id)
  }

  row
}

clip_points_to_pitch <- function(pts, pitch_xlim, pitch_ylim, eps = 0.001) {
  pts %>%
    mutate(
      x = pmin(pmax(x, pitch_xlim[1] + eps), pitch_xlim[2] - eps),
      y = pmin(pmax(y, pitch_ylim[1] + eps), pitch_ylim[2] - eps)
    )
}

frame_to_points <- function(frame_row, player_lookup) {
  vals <- as.character(unlist(frame_row[1, ], use.names = FALSE))

  if (length(vals) < 68) {
    stop("Frame-rækken er kortere end forventet.")
  }

  home_ids <- vals[3:13]
  away_ids <- vals[36:46]

  home_x <- suppressWarnings(as.numeric(vals[14:24]))
  home_y <- suppressWarnings(as.numeric(vals[25:35]))
  away_x <- suppressWarnings(as.numeric(vals[47:57]))
  away_y <- suppressWarnings(as.numeric(vals[58:68]))

  pts <- tibble(
    ssiId = c(home_ids, away_ids),
    x = c(home_x, away_x),
    y = c(home_y, away_y)
  ) %>%
    left_join(player_lookup, by = "ssiId") %>%
    mutate(
      player_name = ifelse(is.na(player_name), ssiId, player_name),
      team = ifelse(is.na(team), "Ukendt", team)
    )

  as.data.frame(pts)
}

frame_ball <- function(frame_row) {
  vals <- as.character(unlist(frame_row[1, ], use.names = FALSE))

  ball_x <- if (length(vals) >= 69) suppressWarnings(as.numeric(vals[69])) else NA_real_
  ball_y <- if (length(vals) >= 70) suppressWarnings(as.numeric(vals[70])) else NA_real_

  data.frame(x = ball_x, y = ball_y)
}

# De tre situationsframes beholdes fra den uploadede fil.
f1 <- 101122
f2 <- 120726
f3 <- 120750

# Vi bruger selve angrebsframen som analyseframe til Delaunay/Voronoi.
best_frame <- f2

points <- frame_to_points(get_frame_row(best_frame, tracking), player_lookup)
ball_best <- frame_ball(get_frame_row(best_frame, tracking))

pitch_xlim <- c(pitch_dims$origin_x, pitch_dims$origin_x + pitch_dims$length)
pitch_ylim <- c(pitch_dims$origin_y, pitch_dims$origin_y + pitch_dims$width)
pitch_window <- c(pitch_xlim[1], pitch_xlim[2], pitch_ylim[1], pitch_ylim[2])

points <- clip_points_to_pitch(points, pitch_xlim, pitch_ylim)

# -----------------------------
# Delaunay på analyseframen
# -----------------------------
tri <- deldir(points$x, points$y, rw = pitch_window)

p_delaunay_base <- ggplot() +
  annotate_pitch(
    dimensions = pitch_dims,
    fill = "#1b5e20",
    colour = "white",
    limits = FALSE
  ) +
  geom_segment(
    data = tri$dirsgs,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    color = "white",
    linewidth = 0.5
  ) +
  geom_point(
    data = points,
    aes(x = x, y = y, fill = team),
    shape = 21,
    color = "black",
    size = 4
  ) +
  labs(
    title = paste("Delaunay-triangulering, frame", best_frame),
    fill = "Hold"
  ) +
  theme_pitch(aspect_ratio = NULL) +
  coord_fixed(ratio = 1)

if (all(is.finite(c(ball_best$x, ball_best$y)))) {
  p_delaunay_base <- p_delaunay_base +
    geom_point(
      data = ball_best,
      aes(x = x, y = y),
      inherit.aes = FALSE,
      shape = 21,
      fill = "white",
      color = "black",
      stroke = 1.2,
      size = 4,
      show.legend = FALSE
    )
}

print(p_delaunay_base)

# -----------------------------
# Voronoi-areal på analyseframen
# -----------------------------
tiles <- tile.list(tri)
points$area <- vapply(tiles, function(tile) tile$area, numeric(1))

# Brug den side, hvor bolden befinder sig, som offensiv zone.
attack_threshold <- 35
attack_on_right <- is.finite(ball_best$x) && ball_best$x >= 0

if (attack_on_right) {
  field_players <- points %>%
    filter(x > attack_threshold)
  side_label <- "højre"
} else {
  field_players <- points %>%
    filter(x < -attack_threshold)
  side_label <- "venstre"
}

if (nrow(field_players) == 0) {
  stop("Ingen spillere fundet i den valgte offensive zone.")
}

top_field <- field_players %>%
  mutate(
    area_pct = 100 * area / sum(area, na.rm = TRUE),
    label = paste0(player_name, " (", team, ")")
  ) %>%
  arrange(desc(area_pct)) %>%
  slice_head(n = 5)

print(top_field)

p_area <- ggplot(top_field, aes(x = reorder(label, area_pct), y = area_pct)) +
  geom_col(fill = "red") +
  geom_text(
    aes(label = paste0(round(area_pct, 1), "%")),
    hjust = -0.1
  ) +
  coord_flip() +
  labs(
    title = paste("Top 5 spillere med mest plads i offensiven (", side_label, " side)", sep = ""),
    x = NULL,
    y = "Andel af offensivt Voronoi-areal (%)"
  ) +
  theme_minimal()

print(p_area)

# -----------------------------
# Tre Delaunay-plots for sekvensen
# -----------------------------
pts1 <- frame_to_points(get_frame_row(f1, tracking), player_lookup) %>%
  clip_points_to_pitch(pitch_xlim, pitch_ylim)
ball1 <- frame_ball(get_frame_row(f1, tracking))

pts2 <- frame_to_points(get_frame_row(f2, tracking), player_lookup) %>%
  clip_points_to_pitch(pitch_xlim, pitch_ylim)
ball2 <- frame_ball(get_frame_row(f2, tracking))

pts3 <- frame_to_points(get_frame_row(f3, tracking), player_lookup) %>%
  clip_points_to_pitch(pitch_xlim, pitch_ylim)
ball3 <- frame_ball(get_frame_row(f3, tracking))

plot_delaunay <- function(
  pts,
  titel = "",
  ball = NULL,
  xlim = NULL,
  ylim = NULL,
  zoom_til_data = FALSE
) {
  rw <- c(pitch_xlim[1], pitch_xlim[2], pitch_ylim[1], pitch_ylim[2])
  tri_local <- deldir(pts$x, pts$y, rw = rw)
  seg <- tri_local$dirsgs

  ball_xy <- if (!is.null(ball) && nrow(ball) == 1 && all(is.finite(c(ball$x, ball$y)))) {
    c(ball$x, ball$y)
  } else {
    numeric(0)
  }

  pad_view <- 0
  pad_zoom <- 12

  if (isTRUE(zoom_til_data) && is.null(xlim) && is.null(ylim)) {
    xv <- pts$x
    yv <- pts$y

    if (length(ball_xy) == 2) {
      xv <- c(xv, ball_xy[1])
      yv <- c(yv, ball_xy[2])
    }

    xr <- range(xv, na.rm = TRUE) + c(-pad_zoom, pad_zoom)
    yr <- range(yv, na.rm = TRUE) + c(-pad_zoom, pad_zoom)

    xr[1] <- max(xr[1], pitch_xlim[1])
    xr[2] <- min(xr[2], pitch_xlim[2])
    yr[1] <- max(yr[1], pitch_ylim[1])
    yr[2] <- min(yr[2], pitch_ylim[2])
  } else {
    xr <- if (is.null(xlim)) pitch_xlim + c(-pad_view, pad_view) else xlim
    yr <- if (is.null(ylim)) pitch_ylim + c(-pad_view, pad_view) else ylim
  }

  p <- ggplot() +
    annotate_pitch(
      dimensions = pitch_dims,
      fill = "#1b5e20",
      colour = "white",
      limits = FALSE
    ) +
    geom_segment(
      data = seg,
      aes(x = x1, y = y1, xend = x2, yend = y2),
      color = "white",
      linewidth = 0.5
    ) +
    geom_point(
      data = pts,
      aes(x = x, y = y, fill = team),
      shape = 21,
      color = "black",
      size = 5
    ) +
    ggtitle(titel) +
    theme_pitch(aspect_ratio = NULL) +
    scale_x_continuous(limits = xr, expand = c(0, 0)) +
    scale_y_continuous(limits = yr, expand = c(0, 0)) +
    coord_fixed(ratio = 1)

  if (length(ball_xy) == 2) {
    p <- p +
      geom_point(
        data = ball,
        aes(x = x, y = y),
        inherit.aes = FALSE,
        shape = 21,
        fill = "white",
        color = "black",
        stroke = 1.2,
        size = 4,
        show.legend = FALSE
      )
  }

  p
}

print(plot_delaunay(pts1, "Før situation", ball = ball1, zoom_til_data = FALSE))
print(plot_delaunay(pts2, "Angreb", ball = ball2, zoom_til_data = FALSE))
print(plot_delaunay(pts3, "Efter", ball = ball3, zoom_til_data = FALSE))
