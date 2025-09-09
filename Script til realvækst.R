nchar(Tillidsundersøgelse$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`)

dftillid <- as.data.frame(Tillidsundersøgelse)

dfforbrug <- as.data.frame(Udgifter_Forbrug_my)

df <- Tillidsundersøgelse
total <- rep(0, 6)
result <- data.frame()
for (i in 1:nrow(df)) {
  total <- total + as.numeric(df[i, 2:7])
  if(i %% 3 == 0) {
    row <- data.frame(kvartal = i/3, t(total/3), check.names = FALSE)
    colnames(row)[-1] <- colnames(df)[2:7]
    result <- rbind(result, row)
    total <- rep (0,6)
  }
}

#rund alle kolonner fra nr. 2 til slut til 2 decimaler
result_backup[ , 2:ncol(result)] <- round(result_backup[ , 2:ncol(result)], 2)

result_backup <- as.data.frame(result)

result_backup$kvartal <- with(
  result_backup,
  paste(
    ((kvartal - 1) %/% 4) + 2000,
    paste0("Q", ((kvartal - 1) %% 4) + 1)
  )
)

names(dfforbrug)

names(dfforbrug)[names(dfforbrug) == "...2"] <- "Forbrugertillid"


dfforbrug <- subset(dfforbrug, select = -1)

dfsamlet <- cbind(result_backup, dfforbrug)

my_model <- (diff(log(as.numeric(dfforbrug$Forbrugertillid)),lag=4*100

summary(my_model)
                  