library(dplyr)

# Filtrér perioden 2000–2025
data_perioden <- FORV1_kvartal_DST %>%
  filter(År >= 2000 & År <= 2025)

# Beregn gennemsnit for begge spørgsmål
resultater <- data_perioden %>%
  summarise(
    gennemsnit_forbrugsgoder = mean(`Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket`, na.rm = TRUE),
    gennemsnit_spare_op = mean(`Anser det som fornuftigt at spare op i den nuværende økonomiske situation`, na.rm = TRUE)
  )

resultater
