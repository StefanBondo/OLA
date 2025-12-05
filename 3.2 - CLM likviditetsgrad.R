
# Pakker
install.packages("ordinal")
library(ordinal)
library(dplyr)

# Indlæs data
df <- data.frame(regnskaber_industri_transport_byg_5_25000_ansatte_anonym)

# Slå "Dårlig" og "Dårlige" sammen
df$laan_raw <- df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.
df$laan_raw[df$laan_raw == "Dårlig"] <- "Dårlige"

# Fjern "Ved ikke"
df <- df %>% filter(laan_raw != "Ved ikke")

# Beregn likviditetsgrad 2016–2020 (gennemsnit)
df$Likviditetsgrad <- rowMeans(df[, c(
  "Likviditetsgrad.2016....",
  "Likviditetsgrad.2017....",
  "Likviditetsgrad.2018....",
  "Likviditetsgrad.2019....",
  "Likviditetsgrad.2020...."
)], na.rm = TRUE)

# Ordinal afhængig variabel
df$laan_muligheder <- factor(
  df$laan_raw,
  levels = c("Meget dårlige", "Dårlige", "Neutrale", "Gode", "Meget gode"),
  ordered = TRUE
)

# Rent datasæt til CLM
df_clm <- df %>%
  select(laan_muligheder, Likviditetsgrad) %>%
  na.omit()

# CLM model – kun Likviditetsgrad
clm_model_likviditetsgrad <- clm(
  laan_muligheder ~ Likviditetsgrad,
  data = df_clm
)

# Resultater
summary(clm_model_likviditetsgrad)

