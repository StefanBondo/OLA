#Forbrugertillid API

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


#Forbrugergrupper API
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

#Forbrug API

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
