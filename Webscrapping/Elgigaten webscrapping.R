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

rawres <- GET(url=køleskab_url, add_headers('User-Agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36'))
rawres$status_code
rawcontent <- httr::content(rawres,as="text")

#Transform text til HTML nodes
page <- read_html(rawcontent)

#tag liste

itemlist = page %>% html_elements("li[data-cro='product-item']")
testkøle <- itemlist[[1]]
pricetag = testkøle %>% html_element("div[data-primary-price]") %>% html_text()
varnummer <- page %>% html_element("li[data-sku]") %>% html_text()
varnummer

cn <- c("Pris", "Varenummer")
colldf <- data.frame(matrix(NA, ncol = length(cn)))
colnames(colldf) <- cn


for(køleskab in itemlist) {
  price <- itemlist %>% html_element(pricetag) %>% html_text()
  varnummer <- itemlist %>% html_element(varnummer) %>% html_text()
  tmpdf <- data.frame(price,varnummer,Sys.time())
}

rm(køleskab)
#Der er udviklere tilstede så du skal vise koden du bruger til at lave listen af items
#Afgør om det er muligt at ”page” sig igennem udbuddet
#Forklar hvilke risici der er forbundet med webscraping
