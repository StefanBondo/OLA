#Indlæs
FORV1 <- data.frame(FORV1_test)

names(FORV1) <- gsub("^F[0-9]+\\s*", "", names(FORV1))

FORV1 <- FORV1 %>% 
  rename(År = TID)

#Alle kolonner bortset år numeric
FORV1[ , -1] <- lapply(FORV1[ , -1], as.numeric)

#tjek
str(FORV1)



