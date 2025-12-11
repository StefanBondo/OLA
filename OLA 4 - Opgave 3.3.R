library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(ggplot2)
library(purrr)
library(dplyr)


# Hent frisk liste af fly
baseurl <- "https://opensky-network.org/api"
endpointS <- "/states/all"
lamin <- 52.055129
lamax <- 56.196869
lomin <- -6.065140
lomax <- 4.305954

fullurl <- paste0(baseurl, endpointS, "?lamin=", lamin, "&lomin=", lomin, "&lamax=", lamax, "&lomax=", lomax)

res <- GET(fullurl)
rescontent <- content(res, as = "text")
resretval <- fromJSON(rescontent)
statedf <- as.data.frame(resretval$states)

colnames(statedf) <- c("icao24","callsign","origin_country","time_position","last_contact",
                       "longitude","latitude","baro_altitude","on_ground","velocity",
                       "heading","vertical_rate","sensors","geo_altitude","squawk",
                       "spi","position_source")

# ICAO-liste
icao_vec <- na.omit(statedf$icao24)

# Funktion til at hente track og beregne metrics
get_metrics <- function(icao){
  turl <- paste0(baseurl, "/tracks/all?icao24=", icao, "&time=0")
  res <- GET(turl)
  if(res$status_code != 200) return(NULL)
  data <- fromJSON(content(res, as = "text"))
  df <- as.data.frame(data)
  if(nrow(df) == 0) return(NULL)
  
  # kolonner
  colnames(df) <- c("icao24","callsign","startTime","endTime","time","lat","lng","alt","crs","grd")
  
  # beregn SD og R2
  sd_crs <- sd(df$crs, na.rm = TRUE)
  r2 <- summary(lm(lat ~ lng, data = df))$r.squared
  
  tibble(icao = icao, sd_crs = sd_crs, r2 = r2)
}

# Loop igennem alle fly
results_fresh <- map_dfr(icao_vec, possibly(get_metrics, NULL))

# -------------------------
#træningsdata
# -------------------------
fl <- list.files(path = "./train", pattern = "*.rds", full.names = TRUE)
train_metrics <- map_dfr(fl, function(f){
  df <- readRDS(f)
  sd_crs <- sd(df$crs, na.rm = TRUE)
  r2 <- summary(lm(lat ~ lng, data = df))$r.squared
  tibble(file = basename(f), sd_crs = sd_crs, r2 = r2)
})

# Se resultaterne
head(results_fresh)

# -------------------------
# Træningsfly
# -------------------------

library(dplyr)

# hent alle rds filer
fl <- list.files(pattern = "circ.*\\.rds$", full.names = TRUE)

train_metrics <- lapply(fl, function(f){
  df <- readRDS(f)
  
  sd_crs <- sd(df$crs, na.rm = TRUE)
  r2 <- summary(lm(lat ~ lng, data = df))$r.squared
  
  data.frame(
    file = basename(f),
    sd_crs = sd_crs,
    r2 = r2
  )
})

# Brug bind_rows til at samle dem
train_metrics <- dplyr::bind_rows(train_metrics)

train_metrics


# -------------------------
# Graf til at vise forskel
# -------------------------

#Træningsfly


ggplot(train_metrics, aes(x = file, y = r2)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Det er bemærkelsesværdigt at trænings flyene har fløjet så meget lige, 
at det lille udsving/cirkel i slutningen af ruten ikke påvirker R2.  ",
    x = "Fil",
    y = "R²",
    caption = "Kilde: Opensky API"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Friske fly

lowest_r2 <- results_fresh %>%
  arrange(r2) %>%
  slice(1:10)   # viser de 10 laveste R²

ggplot(lowest_r2, aes(x = icao, y = r2)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "43c8b9 er det friske fly med den Laveste R²",
    x = "ICAO",
    y = "R²",
    caption = "Kilde: Opensky API"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



