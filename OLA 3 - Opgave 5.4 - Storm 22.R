#--------------------------------------------
# Storm i november 2022 – Vindretning (Aarhus & Anholt)
#--------------------------------------------

# Pakker
library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)

# DMI API nøgle
ak <- "67c70653-0af3-419d-a40e-c09392803a77"

# Basisoplysninger
baseurl <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?"
fd <- "2022-11-01T00:00:00Z"
td <- "2022-11-30T00:00:00Z"

# Stationer
station_aar <- "06074"   # Aarhus
station_an  <- "06079"   # Anholt

#--------------------------------------------
# Funktion til at hente vindretning fra DMI
#--------------------------------------------
get_dmi_data <- function(station, parameter) {
  url <- paste0(
    baseurl,
    "parameterId=", parameter,
    "&stationId=", station,
    "&datetime=", fd, "/", td,
    "&api-key=", ak
  )
  res <- httr::GET(url)
  raw <- httr::content(res, type = "text")
  df  <- fromJSON(raw)
  df  <- as.data.frame(df[2])
  return(df)
}

#--------------------------------------------
# Hent kun vindretning
#--------------------------------------------
vind_dir_aar <- get_dmi_data(station_aar, "wind_dir_past1h")
vind_dir_an  <- get_dmi_data(station_an,  "wind_dir_past1h")

#--------------------------------------------
# Saml og klargør data
#--------------------------------------------
vind_dir_df <- bind_rows(vind_dir_aar, vind_dir_an) %>%
  unnest_wider(features.properties) %>%
  transmute(
    stationId   = as.character(stationId),
    observed    = ymd_hms(observed, tz = "UTC"),
    value_dir   = as.numeric(value),
    parameterId = parameterId
  ) %>%
  filter(parameterId == "wind_dir_past1h") %>%
  distinct(stationId, observed, .keep_all = TRUE) %>%
  arrange(stationId, observed)

# Opret kategorier for hovedretninger
vind_dir_df <- vind_dir_df %>%
  mutate(retning_kat = case_when(
    value_dir >= 315 | value_dir < 45   ~ "Fra nord",
    value_dir >= 45  & value_dir < 135  ~ "Fra øst",
    value_dir >= 135 & value_dir < 225  ~ "Fra syd",
    value_dir >= 225 & value_dir < 315  ~ "Fra vest"
  ))

#--------------------------------------------
# Plot vindretning over tid
#--------------------------------------------
# Kompasplot over vindretning (polarplot)
ggplot(vind_dir_df, aes(x = value_dir, fill = stationId)) +
  geom_histogram(binwidth = 10, color = "white", alpha = 0.8, position = "dodge") +
  coord_polar(start = 0, direction = -1) +
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 330, 30)) +
  labs(
    title = "Kompasplottet viser, at vinden under stormen kom fra syd og sydøst",
    subtitle = "Aarhus (06074) og Anholt (06079)",
    x = "Vindretning (grader fra nord)",
    y = "Antal observationer",
    fill = "Station",
    caption = "Kilde: DMI Open Data API"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

#--------------------------------------------
# Ekstra: Se hyppigst

vind_dir_df %>%
  mutate(retning_kat = case_when(
    value_dir >= 315 | value_dir < 45   ~ "Fra nord",
    value_dir >= 45  & value_dir < 135  ~ "Fra øst",
    value_dir >= 135 & value_dir < 225  ~ "Fra syd",
    value_dir >= 225 & value_dir < 315  ~ "Fra vest"
  )) %>%
  count(stationId, retning_kat)

#kompasplottet kan du aflæse vindretningen som grader fra nord (0° = nord, 90° = øst, 180° = syd, 270° = vest).
#Både Aarhus (rød) og Anholt (blå) har deres længste søjler omkring 120–160 grader, altså fra sydøst til syd.
#Det betyder, at vinden kom fra sydlige og sydøstlige retninger under stormen i november 2022.
#👉 Konklusion:
# Under stormen kom vinden hovedsageligt fra syd og sydøst, hvilket passer med typiske lavtryk over Nordsøen, hvor vinden blæser ind mod Danmark fra sydvestlige luftmasser.
