# 1 dag senere end original scraping
new_scrapedate <- as.POSIXct(max(colldf$Sys.time..)) + 60*60*24

# kopi af original scraping
simdf <- colldf


### ---------------------------------------------------------
### 1) Tilføj 2 nye biler
### ---------------------------------------------------------

newcars <- data.frame(
  price          = c("129900", "99900"),
  makemodel      = c("Volvo V70 2.0", "Volvo V70 2.4"),
  details        = c("2010_230000km", "2007_260000km"),
  properties     = c("Benzin", "Benzin"),
  description    = c("Ny bil tilføjet", "Ny bil tilføjet"),
  location       = c("København", "Aarhus"),
  link           = c("newcar1", "newcar2"),
  carid          = c("NEW1", "NEW2"),
  seller_name    = c("Ny Sælger", "Ny Sælger"),
  seller_address = c("Testvej 1", "Testvej 2"),
  seller_cvr     = c("11111111", "22222222"),
  Sys.time..     = rep(new_scrapedate, 2)
)

simdf <- rbind(simdf, newcars)


### ---------------------------------------------------------
### 2) Opdater pris på 3 eksisterende biler
### ---------------------------------------------------------

change_ids <- sample(colldf$carid, 3)

# Fjern evt. tegn i pris og gang med fx 0.95
simdf$price[simdf$carid %in% change_ids] <-
  as.numeric(gsub("\\D", "", simdf$price[simdf$carid %in% change_ids])) * 0.95

# Opdater scrapedato på disse
simdf$Sys.time..[simdf$carid %in% change_ids] <- new_scrapedate


### ---------------------------------------------------------
### 3) Fjern 5 biler (simuler solgte)
### ---------------------------------------------------------

remove_ids <- sample(setdiff(colldf$carid, change_ids), 5)

simdf <- simdf[!simdf$carid %in% remove_ids, ]



### ---------------------------------------------------------
### 4) Brug anti_join til at se hvad der har ændret sig
### ---------------------------------------------------------


diffgamle <- anti_join(colldf, simdf, by = "carid")
diffpris <- anti_join(simdf, colldf, by = "price")
