
#1) Research goal - er boliger dyerer omkring storbyer end mnidre / provins?

#2) retrieval 
dfbolig23 <- readRDS("boligcl2.rds")

rm(boligcl2)

#3) data preparation
#3.1) Extrapolate

skim(dfbolig)

dfbolig23_backup$grund <- gsub("\\", "", dfbolig23_backup$grund)

dfbolig23$ejerudg <- as.numeric(dfbolig23$ejerudg)

class(dfbolig23_backup$ejerudg)

boxplot(dfbolig$alder)

summary(dfbolig$alder)

lim=(1976-1928)*1.5-1928

#look out for outliners
dfejerudg <- filter(dfbolig23, ejerudg > )


summary(dfbolig23_backup$ejerudg)


dfbolig23_backup <- as.data.frame(dfbolig23)

boxplot(dfboligc1$alder)
boxplot(dfbolig$alder)
    
#data transformation, FE af alder
hist(dfboligc1$alder)
summary(dfboligc1$alder)
mybreaks=c(1850,1930,1970,2000,2010,2026)
mylabs=c("meget gammel", "ældre", "nyere", "ny", "helt ny")
dfboligc1$alderskat <- cut(dfboligc1$alder,labels=mylabs, breaks = mybreaks)

barplot(table(dfboligc1$alderskat))

summary(dfejerud$ejerudg)

dfejerud <- filter(dfbolig23_backup, ejerudg > 4.000 & ejerudg < 8.000)

mybreaks=c(4.000,4.500,5.000,5.500,6.000,8.000)
mylabs=c("Smaat", "mindre smaat", "mellemstort", "stort", "kæmpe stort")
dfejerud$beskrivelse <- cut(dfejerud$ejerudg,labels=mylabs, breaks = mybreaks)


#--------------------------------------------------------

#Opgav 1 OLA 2

Folketal_meta <- dst_meta(table = "BY3", lang = "da")


Folketal_meta$values


library(dplyr)
library(statsDK) # hvis du bruger dette package

# Eksempel på query
my_query <- list(
  BYER = "*",   # hent alle byer først
  TID = "*"     # alle tidspunkter
)

# Hent datasæt (eksempel med fiktiv tabelkode)
BY1 <- dst_get_data(table = "BY1", query = my_query)

# Filtrer "del af" væk
BY1 <- BY1 %>% 
  filter(!grepl("Uden fast bopæl", BYER))
  
# Fjern alt der står i starten af strengen indtil første bynavn
BY1$BYER <- gsub("^[0-9\\-]+\\s+[0-9\\-]*\\s*", "", BY1$BYER)

library(dplyr)

BY1 <- BY1 %>%
  filter(TID == "2025-01-01")



table(BY1$)

mybreaks <- c(0, 250, 1000, 2500, 10000, 50000, Inf) 
mylabs   <- c("landsby", "lille by", "almindelig by", "større by", "storby", "meget stor by")
BY1$bystørrelse <- cut(BY1$value,labels = mylabs, breaks = mybreaks)



# Funktion til at lave første bogstav stort i alle ord
capitalize_words <- function(x) {
  sapply(strsplit(x, " "), function(y) {
    paste(toupper(substring(y, 1, 1)), tolower(substring(y, 2)), sep = "")
  }, USE.NAMES = FALSE)
}

# Anvend funktionen på din kolonne
dfnybolig$zipmunic <- capitalize_words(dfnybolig$zipmunic)



# Join BY1 ind i dfnybolig baseret på bynavn
df_joined <- dfnybolig %>% 
  left_join(BY1, by = c("zipmunic" = "BYER"))

df_joined <-  df_joined %>% rename(
  Befolkningstal = value
)

table_bystørrelse <-  table(BY1$bystørrelse)

hist(x=table_bystørrelse, y = df_joined$sqmpris)

ggplot(df_joined, aes(x = bystørrelse, y = sqmpris, fill = bystørrelse)) +
  geom_col()

options(scipen = 999)

df_joined <- df_joined %>% drop_na(TID,value,bystørrelse)

df_joined_test <- df_joined %>%
  group_by(bystørrelse) %>%
  summarise(sqmpris = mean(sqmpris, na.rm = TRUE), .groups = "drop")

ggplot(df_joined_test, aes(x = bystørrelse, y = sqmpris, fill = bystørrelse)) +
  geom_col() +
  labs(x = NULL, y = "sqmpris", fill = "bystørrelse") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

write_xlsx(dfnybolig, "dfnybolig.xlsx")
