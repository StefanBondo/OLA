library(tidyverse)

NKN1 <- data.frame(NKN1_test)

NKN1 <- NKN1 %>%
  rename(Privat_forbrug = P31S1MD.P.31.Privatforbrug)


NKN1 <- NKN1 %>%
  mutate(
    TID = paste0(year(TID), "Q", quarter(TID))   # overskriv TID med kvartal
  )

NKN1 <- NKN1 %>%
  rename(År = TID)


NKN1 <- NKN1 %>%
  arrange(År) %>%
  mutate(
    real_vækst_pct = (Privat_forbrug - lag(Privat_forbrug, 4)) / 
      lag(Privat_forbrug, 4) * 100
  )



#Fjerner år 1999 på FORV1 og NKN1
#NKN1 <- NKN1 %>%
#filter(!grepl("^1999", Kvartal))




