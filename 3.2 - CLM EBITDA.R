
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

# Beregn EBITDA 2016–2020 (gennemsnit)
df$EBITDA <- rowMeans(df[, c(
  "EBITDA.2016..1.000.kr.",
  "EBITDA.2017..1.000.kr.",
  "EBITDA.2018..1.000.kr.",
  "EBITDA.2019..1.000.kr.",
  "EBITDA.2020..1.000.kr."
)], na.rm = TRUE)

# Log-transformér EBITDA (meget vigtigt pga. store tal)
df$log_EBITDA <- log(df$EBITDA)

# Ordinal afhængig variabel
df$laan_muligheder <- factor(
  df$laan_raw,
  levels = c("Meget dårlige", "Dårlige", "Neutrale", "Gode", "Meget gode"),
  ordered = TRUE
)

# Rent datasæt til CLM
df_clm <- df %>%
  select(laan_muligheder, log_EBITDA) %>%
  na.omit()

# CLM model – kun log(EBITDA)
clm_model_EBITDA <- clm(
  laan_muligheder ~ log_EBITDA,
  data = df_clm
)

# Resultater
summary(clm_model_EBITDA)

