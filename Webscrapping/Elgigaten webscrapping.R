#Case: 
#CEO’en for Power vil gerne kunne se dagens priser fra Elgiganten på køleskabe og frysere
#Du har nu en time til at forberede et oplæg hvor du forklarer ham hvad der skal til ved


#Lav en prototype, hvor du viser hvordan man henter pris og varebetegnelse for én side

#https://www.elgiganten.dk/hvidevarer/koleskabe-fryseskabe/koleskab

library(rvest)
library(httr)
library(dplyr)
library(jsonlite)

køleskab_url <- "https://www.elgiganten.dk/hvidevarer/koleskabe-fryseskabe"

rawres <- GET(køleskab_url, add_headers('User-Agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36'))
rawres$status_code
rawcontent <- httr::content(rawres,as="text")

#Transform text til HTML nodes
page <- read_html(rawcontent)

#tag liste
itemlist = page %>% html_elements("li[data-cro='product-item']")

#test
testkøle <- itemlist[[1]]

pricetag <- "div[data-primary-price]" 
nametag <- "h2.text-balance.break-words.font-bold.font-regular.text-lg.leading-6.line-clamp-3.lg\\:line-clamp-2" 
tilbudtag <- "div.line-clamp-2.text-xs"
lagertag <- "span.font-body.font-bold.text-accent-200"
varenummertag <- "span.sku"


cn <- c("Pris","Navn","Tilbud","Lager","Varenummer")
colldf <- data.frame(matrix(NA, ncol = length(cn)))
colnames(colldf) <- cn

#Loop til få et køleskab i DF
for (i in 1:1) {
  price <- testkøle %>% html_element(pricetag) %>% html_text(trim = TRUE)
  name <- testkøle %>% html_element(nametag) %>% html_text(trim = TRUE)
  tilbud <- testkøle %>% html_element(tilbudtag) %>% html_text(trim = TRUE)
  lager <- testkøle %>% html_element(lagertag) %>% html_text(trim = TRUE)
  varenummer <- testkøle %>% html_element(varenummertag) %>% html_text(trim = TRUE)
  elgigantenDF <- data.frame(price, name, tilbud, lager, varenummer, Sys.time())
}

class(testkøle)

####-----------------------------------------------------------
#Hele siden
####-----------------------------------------------------------

elgiganten_fleredf <- data.frame()  # tom dataframe

for (i in seq_along(itemlist)) {
  produkt <- itemlist[[i]]  # ét produkt ad gangen
  price <- produkt %>% html_element(pricetag) %>% html_text(trim = TRUE)
  name <- produkt %>% html_element(nametag) %>% html_text(trim = TRUE)
  tilbud <- produkt %>% html_element(tilbudtag) %>% html_text(trim = TRUE)
  lager <- produkt %>% html_element(lagertag) %>% html_text(trim = TRUE)
  varenummer <- produkt %>% html_element(varenummertag) %>% html_text(trim = TRUE)
  
  elgiganten_fleredf <- rbind(
    elgiganten_fleredf,
    data.frame(price, name, tilbud, lager, varenummer, Sys.time())
  )
}

#


#Der er udviklere tilstede så du skal vise koden du bruger til at lave listen af items
#Afgør om det er muligt at ”page” sig igennem udbuddet
#Det kan man godt
#Forklar hvilke risici der er forbundet med webscraping
#Man risikiere at ens IP bliver opdaget og så bliver man greylisted og så kan man ikke tilgå deres hjemmeside
