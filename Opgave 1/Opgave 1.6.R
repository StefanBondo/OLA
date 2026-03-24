library(DBI)
library(RMariaDB)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggsoccer)
library(glue)

con <- dbConnect(
  MariaDB(),
  host = "www.talmedos.com",
  port = 3306,
  db = "superliga2",
  user = "dalremote",
  password = "OttoRehagel123456789Long2026!"
)


# ------------------------------------------------------------
# PARAMETRE
# ------------------------------------------------------------
comp_id <- 335
date_from <- as.Date("2025-07-01")
date_to <- as.Date("2026-06-01")

shots_df_all_indværende <- dbGetQuery(con, glue("
  SELECT DISTINCT
    m.MATCH_WYID,
    m.DATE AS match_date,
    m.MATCHLABEL AS match_label,

    s.EVENT_WYID,
    s.PRIMARYTYPE,
    s.SHOTBODYPART,
    s.SHOTISGOAL,
    s.SHOTONTARGET,
    s.SHOTGOALZONE,
    s.SHOTXG,
    s.SHOTPOSTSHOTXG,

    ec.MATCHPERIOD,
    ec.MINUTE,
    ec.SECOND,
    ec.TEAM_WYID,
    ec.PLAYER_WYID,
    ec.LOCATIONX,
    ec.LOCATIONY,

    t.TEAMNAME AS team_name,

    CONCAT(p.FIRSTNAME, ' ', p.LASTNAME) AS player_name,

    f.PLAYERPOSITION AS player_position,

    st.PRIMARYTYPE AS secondary_primarytype,
    st.SECONDARYTYPE1,
    st.SECONDARYTYPE2,
    st.SECONDARYTYPE3,
    st.SECONDARYTYPE4,
    st.SECONDARYTYPE5,
    st.SECONDARYTYPE6,
    st.SECONDARYTYPE7

  FROM wyscout_matches m

  LEFT JOIN wyscout_matchevents_shots s
    ON m.MATCH_WYID = s.MATCH_WYID
   AND s.COMPETITION_WYID = {comp_id}
   AND s.PRIMARYTYPE = 'shot'

  LEFT JOIN wyscout_matchevents_common ec
    ON s.EVENT_WYID = ec.EVENT_WYID

  LEFT JOIN wyscout_teams t
    ON ec.TEAM_WYID = t.TEAM_WYID

  LEFT JOIN wyscout_players p
    ON ec.PLAYER_WYID = p.PLAYER_WYID

  LEFT JOIN wyscout_matchformations f
    ON m.MATCH_WYID = f.MATCH_WYID
   AND ec.PLAYER_WYID = f.PLAYER_WYID
   AND f.COMPETITION_WYID = {comp_id}

  LEFT JOIN wyscout_matchevents_secondarytype st
    ON s.EVENT_WYID = st.EVENT_WYID

  WHERE m.COMPETITION_WYID = {comp_id}
    AND m.DATE >= '{date_from}'
    AND m.DATE <= '{date_to}'
"))

Brøndby_data <- shots_df_all_indværende %>%
  filter(team_name == "Brøndby")

# --------------------------------------------------
# 3. Rens SHOTBODYPART
# --------------------------------------------------
Brøndby_data <- Brøndby_data %>%
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
Brøndby_data <- Brøndby_data %>%
  mutate(
    hovedstød = ifelse(SHOTBODYPART == "head_or_other", 1, 0)
  )

# Mål
Brøndby_data <- Brøndby_data %>%
  mutate(
    Mål = ifelse(!is.na(SECONDARYTYPE1) & SECONDARYTYPE1 == "goal", 1, 0)
  )

# Spilsituation
Brøndby_data <- Brøndby_data %>%
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
Brøndby_data <- Brøndby_data %>%
  mutate(
    shot_distance = sqrt((100 - LOCATIONX)^2 + (50 - LOCATIONY)^2)
  )

# Skudvinkel i grader
Brøndby_data <- Brøndby_data %>%
  mutate(
    goal_width = 7.32,
    x = 100 - LOCATIONX,
    y = abs(LOCATIONY - 50),
    shot_angle_deg = atan(goal_width * x / (x^2 + y^2 - (goal_width / 2)^2)),
    shot_angle_deg = ifelse(shot_angle_deg < 0, shot_angle_deg + pi, shot_angle_deg),
    shot_angle_deg = shot_angle_deg * 180 / pi
  )

# Kropsdel til model
Brøndby_data <- Brøndby_data %>%
  mutate(
    body_part = case_when(
      SHOTBODYPART == "left_foot" ~ "left_foot",
      SHOTBODYPART == "right_foot" ~ "right_foot",
      SHOTBODYPART == "head_or_other" ~ "head_or_other",
      TRUE ~ "other"
    )
  )

# Fjern unødvendige kolonner
Brøndby_data <- Brøndby_data %>%
  select(-c(SECONDARYTYPE6, SECONDARYTYPE7, goal_width, x, y))

# --------------------------------------------------
# 5. Split i træning og test
# --------------------------------------------------
set.seed(123)

n <- nrow(Brøndby_data)
train_index_Brøndby <- sample(1:n, size = 0.8 * n)

train_data_Brønby <- Brøndby_data[train_index, ]
test_data_Brøndby  <- Brøndby_data[-train_index, ]

# --------------------------------------------------
# 6. Gør kategoriske variable til faktorer
# --------------------------------------------------
train_data_Brønby <- Brøndby_data %>%
  mutate(
    body_part = as.factor(body_part),
    Spilsituation = as.factor(Spilsituation),
    Mål = as.numeric(Mål)
  )

test_data_Brøndby <- Brøndby_data %>%
  mutate(
    body_part = as.factor(body_part),
    Spilsituation = as.factor(Spilsituation),
    Mål = as.numeric(Mål)
  )


# --------------------------------------------------
# Logisstik model
# --------------------------------------------------

# Predict sandsynlighed (xG)
test_data_Brøndby$pred_prob <- predict(model_logit, newdata = test_data_Brøndby, type = "response")

# Klassifikation
test_data_Brøndby$pred_class <- ifelse(test_data_Brøndby$pred_prob > 0.5, 1, 0)


results_brondby <- data.frame(
  Accuracy = mean(test_data_Brøndby$pred_class == test_data_Brøndby$Mål),
  AUC = as.numeric(auc(roc(test_data_Brøndby$Mål, test_data_Brøndby$pred_prob))),
  Brier = mean((test_data_Brøndby$pred_prob - test_data_Brøndby$Mål)^2),
  Total_xG = sum(test_data_Brøndby$pred_prob),
  Faktiske_mål = sum(test_data_Brøndby$Mål)
)

results_brondby <- results_brondby %>%
  mutate(
    Accuracy = round(Accuracy * 100, 2),
    AUC = round(AUC, 3),
    Brier = round(Brier, 3),
    Total_xG = round(Total_xG, 2)
  )

results_brondby
