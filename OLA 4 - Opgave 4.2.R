#🔹 1. Vælg ét fly fra din liste
icao <- statedf$icao24[1]
icao

#🔹 2. Hent track for flyet
baseurl <- "https://opensky-network.org/api/tracks/all"
token <- getToken()  # Hvis du har funktionen fra util.R – ellers kan du droppe Authorization

turl <- paste0(baseurl, "?icao24=", icao, "&time=0")

res <- httr::GET(turl)
track_raw <- httr::content(res, as = "text")
track_json <- jsonlite::fromJSON(track_raw)
trackdf <- as.data.frame(track_json)

#🔹 3. Gør data læsbar
colnames(trackdf) <- c("icao24", "callsign", "startTime", "endTime", 
                       "time", "lat", "lng", "alt", "crs", "grd")

#🔹 4. Hent et cirklende fly fra .rds filer
circdf <- readRDS("circjet3.rds")

#🔹 5. Plot begge fly
library(leaflet)

leaflet() %>% 
  addTiles() %>% 
  addPolylines(data = trackdf, lng = ~lng, lat = ~lat, color = "blue", weight = 5, label = "Normalt fly") %>%
  addPolylines(data = circdf, lng = ~lng, lat = ~lat, color = "red", weight = 2, label = "Cirklende fly")




