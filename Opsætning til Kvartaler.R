library(dplyr)
library(lubridate)

# 1. Lav Måned og Kvartal (YYYYQX)
FORV1 <- FORV1 %>%
  mutate(
    Måned = floor_date(År, "month"),          # bevarer måneds-datoen
    Kvartal = paste0(year(År), "Q", quarter(År))
  )


# 2. Beregn kvartalsgennemsnit
FORV1 <- FORV1 %>%
  group_by(Kvartal) %>%
  summarise(across(
    -c(År, Måned), 
    \(x) mean(x, na.rm = TRUE)
  ))







