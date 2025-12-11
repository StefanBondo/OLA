library(readxl)
library(dplyr)
library(ggplot2)

df <- df %>%
  mutate(
    vurdering_gruppe = case_when(
      finansiering %in% c("Gode", "Meget gode") ~ "Positive = Gode / Meget gode",
      finansiering %in% c("Neutrale")          ~ "Neutrale",
      finansiering %in% c("Dårlige", "Meget dårlige") ~ "Negative = Dårlige / Meget dårlige",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(vurdering_gruppe))

df_pct <- df %>%
  count(vurdering_gruppe) %>%
  mutate(pct = n / sum(n) * 100)

# 3. Sæt rækkefølgen på søjlerne
df_pct$vurdering_gruppe <- factor(
  df_pct$vurdering_gruppe,
  levels = c(
    "Negative = Dårlige / Meget dårlige",
    "Neutrale",
    "Positive = Gode / Meget gode"
  )
)

# 4. Plot: procenter og rigtige labels
ggplot(df_pct, aes(x = vurdering_gruppe, y = pct)) +
  geom_col() +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 4) +
  labs(
    title = "De fleste virksomheder siger, at de har gode muligheder for at låne",
    x = "Svar",
    y = "Pct.",
    caption = "Kilde: DI regnskaber_industri_transport_byg_5_25000_ansatte_anonym"
  ) +
  theme_minimal(base_size = 13)
