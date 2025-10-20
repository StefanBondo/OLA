#Opgav 1 OLA 2

Folketal_meta <- dst_meta(table = "BY3", lang = "da")


Folketal_meta$values


library(dplyr)
library(statsDK) # hvis du bruger dette package

# Eksempel på query
my_query <- list(
  BYER = "*", 
  FOLKARTAET = "Folketal",
  TID = "*"     
)

# Hent datasæt (eksempel med fiktiv tabelkode)
BY3 <- dst_get_data(table = "BY3", query = my_query)

# Filtrer "del af" væk
BY3 <- BY3 %>% 
  filter(!grepl("Uden fast bopæl", BYER))

# Fjern alt der står i starten af strengen indtil første bynavn
BY3$BYER <- gsub("^[0-9\\-]+\\s+[0-9\\-]*\\s*", "", BY1$BYER)

library(dplyr)

BY3 <- BY3 %>%
  filter(TID == "2025-01-01")


mybreaks <- c(0, 250, 1000, 2500, 10000, 50000, Inf) 
mylabs   <- c("landsby", "lille by", "almindelig by", "større by", "storby", "meget stor by")
BY3$bystørrelse <- cut(BY3$value,labels = mylabs, breaks = mybreaks)



# Funktion til at lave første bogstav stort i alle ord
capitalize_words <- function(x) {
  sapply(strsplit(x, " "), function(y) {
    paste(toupper(substring(y, 1, 1)), tolower(substring(y, 2)), sep = "")
  }, USE.NAMES = FALSE)
}

# Anvend funktionen på din kolonne
dfnybolig$zipmunic <- capitalize_words(dfnybolig$zipmunic)



# Join BY3 ind i dfnybolig baseret på bynavn
df_joined <- dfnybolig %>% 
  left_join(BY3, by = c("zipmunic" = "BYER"))

BY3 <-  BY3 %>% rename(
  Befolkningstal = value
)


df_joined_test <- df_joined %>%
  group_by(bystørrelse) %>%
  summarise(sqmpris = mean(sqmpris, na.rm = TRUE), .groups = "drop")
