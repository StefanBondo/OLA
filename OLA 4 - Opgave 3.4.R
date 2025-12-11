

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

fresh_circle_results <- lapply(statedf$icao24, function(code){
  
  df <- get_track(code)
  if(is.null(df)) return(NULL)
  
  alg <- detect_circle(df)
  alg$icao <- code
  alg
})

fresh_circle_results <- dplyr::bind_rows(fresh_circle_results)

fresh_circle_results



