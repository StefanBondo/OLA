library(dplyr)
library(tidyverse)
library(readxl)
library(rpart)
library(randomForest)
library(pROC)

# --------------------------------------------------
# 1. Indlæs data
# --------------------------------------------------
shots_df_all_shots <- read_xlsx("shots_all.xlsx")

# --------------------------------------------------
# 2. Filtrer data
# --------------------------------------------------
shots_df_all_shots <- shots_df_all_shots %>%
  filter(liga == "Superliga") %>%
  filter(match_date >= as.Date("2024-07-01") &
           match_date <= as.Date("2025-06-01"))

# --------------------------------------------------
# 3. Rens SHOTBODYPART
# --------------------------------------------------
shots_df_all_shots <- shots_df_all_shots %>%
  mutate(
    SHOTBODYPART = case_when(
      grepl("^\\[", SHOTBODYPART) & grepl("assist", SHOTBODYPART) ~ "assist",
      grepl("^\\[", SHOTBODYPART) & grepl("goal", SHOTBODYPART) ~ "goal",
      grepl("^\\[", SHOTBODYPART) & grepl("head", SHOTBODYPART) ~ "head_or_other",
      grepl("^\\[", SHOTBODYPART) & grepl("intercep", SHOTBODYPART) ~ "interception",
      grepl("^\\[", SHOTBODYPART) & grepl("opportun", SHOTBODYPART) ~ "opportunity",
      grepl("^\\[", SHOTBODYPART) & grepl("shot_aft", SHOTBODYPART) ~ "shot_after_corner",
      grepl("^\\[", SHOTBODYPART) & grepl("touch_in", SHOTBODYPART) ~ "touch_in_box",
      SHOTBODYPART == "[]" ~ "unknown",
      TRUE ~ SHOTBODYPART
    )
  )

# --------------------------------------------------
# 4. Lav forklarende variable
# --------------------------------------------------

# Hovedstød
shots_df_all_shots <- shots_df_all_shots %>%
  mutate(
    hovedstød = ifelse(SHOTBODYPART == "head_or_other", 1, 0)
  )

# Mål
shots_df_all_shots <- shots_df_all_shots %>%
  mutate(
    Mål = ifelse(!is.na(SECONDARYTYPE1) & SECONDARYTYPE1 == "goal", 1, 0)
  )

# Spilsituation
shots_df_all_shots <- shots_df_all_shots %>%
  mutate(
    Spilsituation = case_when(
      grepl("corner", SECONDARYTYPE1) |
        grepl("corner", SECONDARYTYPE2) |
        grepl("corner", SECONDARYTYPE3) |
        grepl("corner", SECONDARYTYPE4) ~ "Hjørne",
      
      grepl("free_kick", SECONDARYTYPE1) |
        grepl("free_kick", SECONDARYTYPE2) |
        grepl("free_kick", SECONDARYTYPE3) ~ "Frispark",
      
      grepl("throw_in", SECONDARYTYPE1) |
        grepl("throw_in", SECONDARYTYPE2) |
        grepl("throw_in", SECONDARYTYPE3) ~ "Indkast",
      
      grepl("head", SECONDARYTYPE1) |
        grepl("head", SECONDARYTYPE2) ~ "Hovedstød",
      
      TRUE ~ "Åbent spil"
    )
  )

# Skudafstand
shots_df_all_shots <- shots_df_all_shots %>%
  mutate(
    shot_distance = sqrt((100 - LOCATIONX)^2 + (50 - LOCATIONY)^2)
  )

# Skudvinkel i grader
shots_df_all_shots <- shots_df_all_shots %>%
  mutate(
    goal_width = 7.32,
    x = 100 - LOCATIONX,
    y = abs(LOCATIONY - 50),
    shot_angle_deg = atan(goal_width * x / (x^2 + y^2 - (goal_width / 2)^2)),
    shot_angle_deg = ifelse(shot_angle_deg < 0, shot_angle_deg + pi, shot_angle_deg),
    shot_angle_deg = shot_angle_deg * 180 / pi
  )

# Kropsdel til model
shots_df_all_shots <- shots_df_all_shots %>%
  mutate(
    body_part = case_when(
      SHOTBODYPART == "left_foot" ~ "left_foot",
      SHOTBODYPART == "right_foot" ~ "right_foot",
      SHOTBODYPART == "head_or_other" ~ "head_or_other",
      TRUE ~ "other"
    )
  )

# Fjern unødvendige kolonner
shots_df_all_shots <- shots_df_all_shots %>%
  select(-c(SECONDARYTYPE6, SECONDARYTYPE7, goal_width, x, y))

# --------------------------------------------------
# 5. Split i træning og test
# --------------------------------------------------
set.seed(123)

n <- nrow(shots_df_all_shots)
train_index <- sample(1:n, size = 0.8 * n)

train_data <- shots_df_all_shots[train_index, ]
test_data  <- shots_df_all_shots[-train_index, ]

# --------------------------------------------------
# 6. Gør kategoriske variable til faktorer
# --------------------------------------------------
train_data <- train_data %>%
  mutate(
    body_part = as.factor(body_part),
    Spilsituation = as.factor(Spilsituation),
    Mål = as.numeric(Mål)
  )

test_data <- test_data %>%
  mutate(
    body_part = as.factor(body_part),
    Spilsituation = as.factor(Spilsituation),
    Mål = as.numeric(Mål)
  )

