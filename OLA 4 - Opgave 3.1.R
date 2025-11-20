library(httr)
library(jsonlite)

baseurl <- "https://opensky-network.org/api/states/all"
lamin <- 52.055129
lamax <- 56.196869
lomin <- -6.065140
lomax <- 4.305954

fullurl <- paste0(baseurl, "?lamin=", lamin, "&lomin=", lomin, "&lamax=", lamax, "&lomax=", lomax)

res <- httr::GET(fullurl)
rescontent <- httr::content(res, as="text")
resretval <- jsonlite::fromJSON(rescontent)
statedf <- as.data.frame(resretval$states)

colnames(statedf) <- c("icao24","callsign","origin_country","time_position","last_contact",
                       "longitude","latitude","baro_altitude","on_ground","velocity",
                       "heading","vertical_rate","sensors","geo_altitude","squawk",
                       "spi","position_source")

library(dplyr)

fly_pr_land <- statedf %>%
  group_by(origin_country) %>%
  summarise(antal = n()) %>%
  arrange(desc(antal))

library(ggplot2)

ggplot(fly_pr_land, aes(x = reorder(origin_country, antal), y = antal)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Der er flest fly fra UK over Nordsøen",
       x = "Land",
       caption = "Kilde:Opensky",
       y = "Antal fly") +
  theme_minimal()

