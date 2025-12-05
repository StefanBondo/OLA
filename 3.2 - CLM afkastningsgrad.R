options(scipen = 999)

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

# Beregn afkastningsgrad 2016–2020 (gennemsnit)
df$afkastningsgrad <- rowMeans(df[, c(
  "Afkastningsgrad.2016....",
  "Afkastningsgrad.2017....",
  "Afkastningsgrad.2018....",
  "Afkastningsgrad.2019....",
  "Afkastningsgrad.2020...."
)], na.rm = TRUE)

# Ordinal afhængig variabel
df$laan_muligheder <- factor(
  df$laan_raw,
  levels = c("Meget dårlige", "Dårlige", "Neutrale", "Gode", "Meget gode"),
  ordered = TRUE
)

# Rent datasæt til CLM
df_clm <- df %>%
  select(laan_muligheder, afkastningsgrad) %>%
  na.omit()

# CLM model KUN med afkastningsgrad
clm_model_afkastningsgrad <- clm(
  laan_muligheder ~ afkastningsgrad,
  data = df_clm
)

summary(clm_model_afkastningsgrad)

