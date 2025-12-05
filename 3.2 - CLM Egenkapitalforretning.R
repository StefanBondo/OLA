
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

# Beregn egenkapitalforrentning 2016–2020 (gennemsnit)
df$Egenkapitalforrentning <- rowMeans(df[, c(
  "Egenkapital.forrentning.2016....",
  "Egenkapital.forrentning.2017....",
  "Egenkapital.forrentning.2018....",
  "Egenkapital.forrentning.2019....",
  "Egenkapital.forrentning.2020...."
)], na.rm = TRUE)

# Ordinal afhængig variabel
df$laan_muligheder <- factor(
  df$laan_raw,
  levels = c("Meget dårlige", "Dårlige", "Neutrale", "Gode", "Meget gode"),
  ordered = TRUE
)

# Rent datasæt til CLM
df_clm <- df %>%
  select(laan_muligheder, Egenkapitalforrentning) %>%
  na.omit()

# CLM model – kun egenkapitalforrentning
clm_model_egenkapitalforrentning <- clm(
  laan_muligheder ~ Egenkapitalforrentning,
  data = df_clm
)

# Resultater
summary(clm_model_egenkapitalforrentning)
