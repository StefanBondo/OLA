

# ---------------------------------------------------------
# Funktion: detect_circle()
# Vores egen algoritme der afgør om et fly cirkler
# ---------------------------------------------------------
detect_circle <- function(df){
  
  # Fjern rækker med manglende lat/lng
  df <- df %>% filter(!is.na(lat), !is.na(lng))
  
  # Hvis der er for få datapunkter → kan ikke beregne noget
  if(nrow(df) < 15){
    return(data.frame(
      lmres = 1,         # R² sættes højt = ser lineært ud
      myOwnAlg = 0,      # ingen krydsninger
      isOff = 0          # ikke cirklende
    ))
  }
  
  # ---------------------------------------------------------
  # 1) R² fra lineær regression: flyver det i en lige linje?
  #    lat forklares ud fra lng
  # ---------------------------------------------------------
  r2 <- summary(lm(lat ~ lng, data = df))$r.squared
  
  # ---------------------------------------------------------
  # 2) Kryds-spor algoritme:
  #    vi tjekker om flyet nærmer sig tidligere punkter
  # ---------------------------------------------------------
  crossings <- 0
  for(i in 10:nrow(df)){  # starter ved punkt 10 for at have historik
    
    # afstanden i lat og lng til tidligere punkter
    close_lat <- abs(df$lat[i] - df$lat[1:(i-1)]) < 0.001
    close_lng <- abs(df$lng[i] - df$lng[1:(i-1)]) < 0.001
    
    # hvis både lat og lng er tæt på → flyet krydser sit eget spor
    if(any(close_lat & close_lng, na.rm = TRUE)){
      crossings <- crossings + 1
    }
  }
  
  # ---------------------------------------------------------
  # Beslutning:
  # Fly er cirklende hvis R² < 0.30 eller crossings > 5
  # ---------------------------------------------------------
  data.frame(
    lmres = r2,
    myOwnAlg = crossings,
    isOff = ifelse(r2 < 0.30 | crossings > 5, 1, 0)
  )
}

# ---------------------------------------------------------
# LOOP: hent tracks for alle friske fly og kør algoritmen
# ---------------------------------------------------------

fresh_results <- lapply(results_fresh$icao, function(code){
  
  # Hent track-data fra OpenSky for én ICAO
  url <- paste0("https://opensky-network.org/api/tracks/all?icao24=", code, "&time=0")
  r <- httr::GET(url)
  
  # Hvis API ikke returnerer OK → skip flyet
  if(r$status_code != 200) return(NULL)
  
  # Konverter JSON → dataframe
  track_raw <- content(r, as = "text")
  track_json <- fromJSON(track_raw)
  df <- as.data.frame(track_json)
  
  # Hvis der ikke er noget track → skip
  if(nrow(df) == 0) return(NULL)
  
  # Sæt korrekte kolonnenavne
  colnames(df) <- c("icao24","callsign","startTime","endTime",
                    "time","lat","lng","alt","crs","grd")
  
  # Kør vores cirkle-algoritme på flyets track
  alg <- detect_circle(df)
  
  # Beregn standardafvigelse af kurs (til sammenligning)
  alg$sdcourse <- sd(df$crs, na.rm = TRUE)
  
  # Tilføj ICAO til resultatet
  alg$icao <- code
  
  alg
})

# ---------------------------------------------------------
# Saml alle fly-resultater i én samlet dataframe
# ---------------------------------------------------------

fresh_results <- bind_rows(fresh_results)

# Se resultatet
fresh_results

