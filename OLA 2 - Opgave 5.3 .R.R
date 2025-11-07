library(dplyr)
library(tidyr)
library(ggplot2)

# 1. LAV DATA LONG FORMAT (hvis de stadig er wide)
df_med <- df_wide_udvalgte_lande %>%
  pivot_longer(-time, names_to = "geo", values_to = "value") %>%
  mutate(type = "Med corona")

df_uden <- df_wide_uden_corona %>%
  pivot_longer(-time, names_to = "geo", values_to = "value") %>%
  mutate(type = "Uden corona")

# 2. SAMMENSLÅ DATAFRAMES
samlet <- bind_rows(df_med, df_uden)

# 3. BEREGN GENNEMSNIT PR. LAND FOR HVER TYPE
samlet_mean <- samlet %>%
  group_by(geo, type) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

# 4. BEREGN EFFEKTEN (forskel mellem med og uden corona)
effekt <- samlet_mean %>%
  pivot_wider(names_from = type, values_from = mean_value) %>%
  mutate(corona_effekt = `Med corona` - `Uden corona`)

# 5. PLOT – SAMMENLIGN MED OG UDEN CORONA
ggplot(samlet_mean, aes(x = reorder(geo, mean_value), y = mean_value, fill = type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Italien er blevet hårdest ramt af corona",
       subtitle = "Deres gennemsnitligt kvartalsvise realvækst faldet med –0,07 procentpoint",
       x = "Land",
       y = "Gennemsnitlig realvækst (%)",
       fill = "Periode") +
  scale_fill_manual(values = c("Uden corona" = "steelblue", "Med corona" = "firebrick")) +
  theme_minimal()


