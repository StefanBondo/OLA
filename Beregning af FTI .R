FORV1_kvartal_DI <- data.frame(FORV1)

FORV1_kvartal_DST <- data.frame(FORV1)

fjern_punktummer <- function(df) {
  names(df) <- gsub("\\.", " ", names(df))
  return(df)
}

FORV1_kvartal_DST <- fjern_punktummer(FORV1_kvartal_DST)
FORV1_kvartal_DI  <- fjern_punktummer(FORV1_kvartal_DI)


FORV1_kvartal_DST <- FORV1_kvartal_DST %>%
  mutate(
    Forbrugertillid = rowMeans(across(c(
      " Familiens økonomiske situation i dag  sammenlignet med for et år siden",
      " Familiens økonomiske  situation om et år  sammenlignet med i dag",
      " Danmarks økonomiske situation i dag  sammenlignet med for et år siden",
      " Danmarks økonomiske situation om et år  sammenlignet med i dag",
      " Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket"
    )), na.rm = TRUE)
  )


FORV1_kvartal_DI <- FORV1_kvartal_DI %>%
  mutate(
    DI_FTI = rowMeans(across(c(
      " Familiens økonomiske situation i dag  sammenlignet med for et år siden",
      " Danmarks økonomiske situation i dag  sammenlignet med for et år siden",
      " Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket",
      " Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr "
    )), na.rm = TRUE)
  )


#Fjerner 1999
#FORV1_kvartal_DST <- FORV1_kvartal_DST %>%
  filter(!grepl("^1999", Kvartal))

#FORV1_kvartal_DI <- FORV1_kvartal_DI %>%
  filter(!grepl("^1999", Kvartal))


