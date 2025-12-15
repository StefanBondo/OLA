#Forbrugertillid API
library(dkstat)
library(tidyverse)

Forbrugertillidindikator_meta <- dst_meta(table = "FORV1", lang = "da")
Forbrugertillidindikator_meta$values


my_query <- list(
  INDIKATOR = "*",
  TID = "*"
)

FORV1_test <- dst_get_data(
  table = "FORV1",
  query = my_query
)

FORV1_test <- FORV1_test %>% 
  filter(TID >= as.Date("1996-01-01"))


library(tidyr)
library(dplyr)

FORV1_test <- FORV1_test %>%
  select(INDIKATOR, TID, value) %>%
  pivot_wider(
    names_from = INDIKATOR,
    values_from = value
  )

#Indlæs
FORV1 <- data.frame(FORV1_test)

names(FORV1) <- gsub("^F[0-9]+\\s*", "", names(FORV1))
names(FORV1) <- sub("^\\s+", "", names(FORV1))

FORV1 <- FORV1 %>% 
  rename(År = TID)

str(FORV1)

#Alle kolonner bortset år numeric
FORV1[ , -1] <- lapply(FORV1[ , -1], as.numeric)

#tjek
str(FORV1)

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


#Beregn FTI

FORV1_kvartal_DI <- data.frame(FORV1)

FORV1_kvartal_DST <- data.frame(FORV1)

fjern_punktummer <- function(df) {
  names(df) <- gsub("\\.", " ", names(df))   # fjern punktummer
  names(df) <- sub("^\\s+", "", names(df))   # fjern mellemrum i starten
  return(df)
}


FORV1_kvartal_DST <- fjern_punktummer(FORV1_kvartal_DST)
FORV1_kvartal_DI  <- fjern_punktummer(FORV1_kvartal_DI)


FORV1_kvartal_DST <- FORV1_kvartal_DST %>%
  mutate(
    Forbrugertillid = rowMeans(across(c(
      "Familiens økonomiske situation i dag  sammenlignet med for et år siden",
      "Familiens økonomiske  situation om et år  sammenlignet med i dag",
      "Danmarks økonomiske situation i dag  sammenlignet med for et år siden",
      "Danmarks økonomiske situation om et år  sammenlignet med i dag",
      "Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket"
    )), na.rm = TRUE)
  )


FORV1_kvartal_DI <- FORV1_kvartal_DI %>%
  mutate(
    DI_FTI = rowMeans(across(c(
      "Familiens økonomiske situation i dag  sammenlignet med for et år siden",
      "Danmarks økonomiske situation i dag  sammenlignet med for et år siden",
      "Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket",
      "Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr "
    )), na.rm = TRUE)
  )

#-------------------------------------
#Forbrugergrupper API
#-------------------------------------

Forbrugegrupper_meta <- dst_meta(table = "NKHC021", lang = "da")

Forbrugegrupper_meta$values


my_query <- list(
  FORMAAAL = "*",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

NKHC021_test <- dst_get_data(
  table = "NKHC021",
  query = my_query
)

NKHC021_test <-  NKHC021_test %>% 
  slice(-c(1:143))

NKHC021_test <- NKHC021_test %>%
  select(-PRISENHED, -SÆSON)

NKHC021_test <- NKHC021_test %>%
  select(FORMAAAL, TID, value) %>%
  pivot_wider(
    names_from = FORMAAAL,
    values_from = value
  )

NKHC021_test <-  NKHC021_test %>% 
  slice(-c(1:24))

Forbruggrupper <- data.frame(NKHC021_test)
names(Forbruggrupper) <- gsub("^CP[A-Z]\\.", "", names(Forbruggrupper))
Forbruggrupper <- Forbruggrupper %>% rename(År = TID)

#-------------------------------------
#Forbrug API
#-------------------------------------

Forbrug_meta <- dst_meta(table = "NKN1", lang = "da")

Forbrug_meta$values

my_query <- list(
  TRANSAKT = "P.31 Privatforbrug",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

NKN1_test <- dst_get_data(
  table = "NKN1",
  query = my_query
)

NKN1_test <- NKN1_test %>%
  select(-PRISENHED, -SÆSON)

NKN1_test <- NKN1_test %>%
  select(TRANSAKT, TID, value) %>%
  pivot_wider(
    names_from = TRANSAKT,
    values_from = value
  )

NKN1_test <-  NKN1_test %>% 
  slice(-c(1:24))

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

