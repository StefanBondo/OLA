library(httr)
library(stringr)
library(jsonlite)


clientId=
clientSecret=

#Testurl
testurl="https://opensky-network.org/api/states/all?&icao24=44cdc9"

#Hent data
retval = GET(url = testurl)
retval$status_code

#Få det i rawcontent
rawcontent=httr::content(retval, as="text")
rawcontent

#Lav om til JSON format
jsContent=fromJSON(rawcontent)

#JSON filformat i dataframe
fldf=as.data.frame(jsContent)


#hente alle fly der flyver lige nu ------------

# Installér kun én gang:
# install.packages(c("httr","jsonlite","dplyr","tibble"))

=== OpenSky API: Live Aircraft States (OAuth2 Auth) ===
  
install.packages(c("httr", "jsonlite", "dplyr","keyring"))

library(httr)
library(jsonlite)
library(dplyr)


#URL til alle fly
testurl="https://opensky-network.org/api/states/all?"

#Hente data
allefly <- GET(url = "https://opensky-network.org/api/states/all?")
allefly$status_code

#omdøb til rawcontetn
Rawcontentallefly= httr::content(allefly, as="text")

#Få det json format
jscontentallefly <- fromJSON(Rawcontentallefly)

#Få det i dataframe
alleflyDf <- as.data.frame(jscontentallefly$states)

#Filter på SAA
sas_fly <- alleflyDf %>%
  filter(grepl("^SAS", V2, ignore.case = TRUE))

  
  
# med token ----------
  nyturl="https://opensky-network.org/api/flights/aircraft?icao24=3c675a&begin=1517184000&end=1517270400"
  
  retval_ny = GET(url = nyurl)
  retval$status_code
  
  rawcontent=httr::content(retval, as="text")
  rawcontent
  jsContent=fromJSON(rawcontent)
  fldf=as.data.frame(jsContent)