library(dplyr)
library(readxl)
library(tidyverse)

# ---------------------------
# 1. Indlæs data
# ---------------------------
shots <- read_xlsx("shots_all.xlsx")

shots <- shots %>%
  mutate(match_date = as.Date(match_date)) %>%
  filter(liga == "Superliga") %>%
  filter(match_date >= as.Date("2024-07-01"),
         match_date <= as.Date("2025-06-01"))

# ---------------------------
# 2. Feature engineering
# ---------------------------
shots <- shots %>%
  mutate(
    Mål = ifelse(
      (!is.na(SHOTISGOAL) & SHOTISGOAL == 1) |
        (!is.na(SECONDARYTYPE1) & SECONDARYTYPE1 == "goal"),
      1, 0
    ),
    
    # Kun gyldige Wyscout-koder (+ hoved fanget i defekt JSON); opportunity m.m. → NA
    kropsdel_kendt = case_when(
      SHOTBODYPART == "left_foot" ~ "left_foot",
      SHOTBODYPART == "right_foot" ~ "right_foot",
      SHOTBODYPART == "head_or_other" ~ "head",
      grepl("^\\[", coalesce(SHOTBODYPART, "")) &
        grepl("head", SHOTBODYPART, ignore.case = TRUE) ~ "head",
      TRUE ~ NA_character_
    ),
    
    hovedstød = ifelse(grepl("head", coalesce(SHOTBODYPART, "")), 1, 0),
    
    kropsdel_uden_hovedstød = case_when(
      SHOTBODYPART == "left_foot" ~ "left_foot",
      SHOTBODYPART == "right_foot" ~ "right_foot",
      TRUE ~ "other"
    ),
    
    kropsdel_med_hovedstød = case_when(
      SHOTBODYPART == "left_foot" ~ "left_foot",
      SHOTBODYPART == "right_foot" ~ "right_foot",
      grepl("head", coalesce(SHOTBODYPART, "")) ~ "head",
      TRUE ~ "other"
    ),
    
    Spilsituation = case_when(
      grepl("corner", coalesce(SECONDARYTYPE1, "")) |
        grepl("corner", coalesce(SECONDARYTYPE2, "")) |
        grepl("corner", coalesce(SECONDARYTYPE3, "")) |
        grepl("corner", coalesce(SECONDARYTYPE4, "")) ~ "Hjørne",
      
      grepl("free_kick", coalesce(SECONDARYTYPE1, "")) |
        grepl("free_kick", coalesce(SECONDARYTYPE2, "")) |
        grepl("free_kick", coalesce(SECONDARYTYPE3, "")) ~ "Frispark",
      
      TRUE ~ "Åbent spil"
    ),
    
    shot_distance = sqrt((100 - LOCATIONX)^2 + (50 - LOCATIONY)^2),
    
    x = 100 - LOCATIONX,
    y = abs(LOCATIONY - 50),
    shot_angle_deg = atan(7.32 * x / (x^2 + y^2 - (7.32 / 2)^2)),
    shot_angle_deg = ifelse(shot_angle_deg < 0, shot_angle_deg + pi, shot_angle_deg),
    shot_angle_deg = shot_angle_deg * 180 / pi
  )

# ---------------------------
# 3. Data til model
# ---------------------------
model_data <- shots %>%
  select(
    match_date,
    Mål,
    shot_distance,
    shot_angle_deg,
    hovedstød,
    kropsdel_uden_hovedstød,
    kropsdel_med_hovedstød,
    Spilsituation,
    kropsdel_kendt
  ) %>%
  drop_na(
    match_date,
    Mål,
    shot_distance,
    shot_angle_deg,
    hovedstød,
    kropsdel_uden_hovedstød,
    kropsdel_med_hovedstød,
    Spilsituation
  ) %>%
  mutate(
    kropsdel_uden_hovedstød = factor(kropsdel_uden_hovedstød),
    kropsdel_med_hovedstød = factor(kropsdel_med_hovedstød),
    Spilsituation = factor(Spilsituation),
    kropsdel_kendt = factor(
      kropsdel_kendt,
      levels = c("left_foot", "right_foot", "head")
    )
  )

# ---------------------------
# 4. Split train / test
# ---------------------------
set.seed(123)

n <- nrow(model_data)
train_index <- sample(1:n, size = floor(0.8 * n))

train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

test_data$kropsdel_uden_hovedstød <- factor(
  test_data$kropsdel_uden_hovedstød,
  levels = levels(train_data$kropsdel_uden_hovedstød)
)

test_data$kropsdel_med_hovedstød <- factor(
  test_data$kropsdel_med_hovedstød,
  levels = levels(train_data$kropsdel_med_hovedstød)
)

test_data$Spilsituation <- factor(
  test_data$Spilsituation,
  levels = levels(train_data$Spilsituation)
)

