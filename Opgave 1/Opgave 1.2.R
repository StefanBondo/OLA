# Vores variabler
#skudafstand
#Vinkel
#hovedstød
#Bodypart
#Secondartype

#Beregner afstabd


library(ggplot2)
library(dplyr)


#Gennemsnit af skud og vinkel
plot_data <- shots_df_all_shots %>%
  summarise(
    shot_distance = mean(shot_distance, na.rm = TRUE),
    shot_angle_deg = mean(shot_angle_deg, na.rm = TRUE)
  ) %>%
  tidyr::pivot_longer(cols = everything(),
                      names_to = "variable",
                      values_to = "value")

plot_data$variable <- recode(plot_data$variable,
                             "shot_distance" = "Skudafstand",
                             "shot_angle_deg" = "Skudvinkel"
)

ggplot(plot_data, aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(value, 1)), vjust = -0.5) +
  labs(
    x = "",
    y = "Gennemsnit",
    title = "Gennemsnitlig skudafstand og vinkel",
    caption = "Kilde: Wyouscout og egne beregninger"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#SKud fordeling af bodypart
library(ggplot2)

bodypart_data <- shots_df_all_shots %>%
  filter(SHOTBODYPART %in% c("left_foot", "right_foot", "head_or_other")) %>% 
  count(SHOTBODYPART) %>%
  mutate(share = round(n / sum(n)*100))

ggplot(bodypart_data, aes(x = SHOTBODYPART, y = share, fill = SHOTBODYPART)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(share, 2)), vjust = -0.5) +
  labs(
    x = "",
    y = "Andel",
    title = "Største delen af skud er med højrefod og venstrefod",
    caption = "Kilde: Wyuosocu og egne beregninger"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

