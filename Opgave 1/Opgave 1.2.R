# ==================================================
# OPGAVE 1.2
# Beskrivende statistik + grafer
# ==================================================

# Tabel 1: numeriske variable
beskrivende_num <- model_data %>%
  summarise(
    Gns_distance = mean(shot_distance, na.rm = TRUE),
    Median_distance = median(shot_distance, na.rm = TRUE),
    SD_distance = sd(shot_distance, na.rm = TRUE),
    Min_distance = min(shot_distance, na.rm = TRUE),
    Max_distance = max(shot_distance, na.rm = TRUE),
    
    Gns_angle = mean(shot_angle_deg, na.rm = TRUE),
    Median_angle = median(shot_angle_deg, na.rm = TRUE),
    SD_angle = sd(shot_angle_deg, na.rm = TRUE),
    Min_angle = min(shot_angle_deg, na.rm = TRUE),
    Max_angle = max(shot_angle_deg, na.rm = TRUE)
  ) %>%
  mutate(across(everything(), round, 2))

beskrivende_num

# Tabel 2: body part (kun venstre/højre/hoved — opportunity m.m. tæller ikke med)
bodypart_tabel <- model_data %>%
  filter(!is.na(kropsdel_kendt)) %>%
  count(kropsdel_kendt) %>%
  mutate(
    kropsdel_label = factor(
      recode(
        as.character(kropsdel_kendt),
        left_foot = "Venstre fod",
        right_foot = "Højre fod",
        head = "Hoved"
      ),
      levels = c("Venstre fod", "Højre fod", "Hoved")
    ),
    andel_pct = round(100 * n / sum(n), 2)
  )

# Tabel 3: spilsituation
spil_tabel <- model_data %>%
  count(Spilsituation) %>%
  mutate(andel_pct = round(100 * n / sum(n), 2))

spil_tabel

# Graf 1: gennemsnit distance og angle
plot_data <- model_data %>%
  summarise(
    shot_distance = mean(shot_distance, na.rm = TRUE),
    shot_angle_deg = mean(shot_angle_deg, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  mutate(
    variable = recode(variable,
                      "shot_distance" = "Skudafstand",
                      "shot_angle_deg" = "Skudvinkel")
  )

ggplot(plot_data, aes(x = variable, y = value, fill = variable)) +
  geom_col() +
  geom_text(aes(label = round(value, 1)), vjust = -0.5) +
  labs(
    title = "Gennemsnitlig skudafstand og skudvinkel",
    x = "", 
    y = "Gennemsnit",
    caption = "Kilde: Wyscout og egne beregninger") +
  theme_minimal() +
  theme(legend.position = "none")

# Graf 2: body part (venstre, højre, hoved)
ggplot(bodypart_tabel, aes(x = kropsdel_label, y = andel_pct, fill = kropsdel_label)) +
  geom_col() +
  geom_text(aes(label = andel_pct), vjust = -0.5) +
  labs(
    title = "Flest skud afsluttes med højre fod",
    subtitle = "Fordeling af afslutninger fordelt på kropsdele i sæson 24/25",
    x = "",
    y = "Andel i %",
    caption = "Kilde: Wyscout og egne beregninger") +
  theme_minimal() +
  theme(legend.position = "none")



