library(readxl)
library(dplyr)
library(ggplot2)

df <- data.frame(regnskaber_industri_transport_byg_5_25000_ansatte_anonym)

df_clean <- df %>%
  filter(Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. != "Ved ikke") %>%
  mutate(
    laane_kat = case_when(
      Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. == "Meget dårlige" ~ "Meget dårlige",
      Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. %in% c("Dårlig", "Dårlige") ~ "Dårlige",
      Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. == "Neutrale" ~ "Neutrale",
      Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. == "Gode" ~ "Gode",
      Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. == "Meget gode" ~ "Meget gode"
    ),
    laane_kat = factor(laane_kat,
                       levels = c("Meget dårlige", "Dårlige", "Neutrale", "Gode", "Meget gode"))
  )

ggplot(df_clean, aes(x = laane_kat)) +
  geom_bar() +
  geom_text(stat = "count",
            aes(label = after_stat(count)),
            vjust = -0.5,
            size = 4) +
  labs(
    title = "De fleste virksomheder siger, at de har gode muligheder for at låne",
    x = "Svar",
    caption = "Kilde:DI regnskaber_industri_transport_byg_5_25000_ansatte_anonym",
    y = "Antal"
  )
