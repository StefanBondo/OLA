
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

# LAV EN ÅRS-VARIABEL
df$år <- factor(
  c("2016", "2017", "2018", "2019", "2020")[max.col(df[, c(
    "Omsætning.2016..1.000.kr.",
    "Omsætning.2017..1.000.kr.",
    "Omsætning.2018..1.000.kr.",
    "Omsætning.2019..1.000.kr.",
    "Omsætning.2020..1.000.kr."
  )], ties.method = "first")],
  levels = c("2016", "2017", "2018", "2019", "2020")
)

# Ordinal afhængig variabel
df$laan_muligheder <- factor(
  df$laan_raw,
  levels = c("Meget dårlige", "Dårlige", "Neutrale", "Gode", "Meget gode"),
  ordered = TRUE
)

# Rent datasæt
df_clm <- df %>% select(laan_muligheder, år) %>% na.omit()

# CLM model med år
clm_model_år <- clm(
  laan_muligheder ~ år,
  data = df_clm
)

summary(clm_model_år)

