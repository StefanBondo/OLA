install.packages("rvest")
library(rvest)
library(httr)
library(dplyr)
library(jsonlite)

# Vi har valgt volvo benzin biler herunder alle V70 modeller

####-----------------------------------------------------------
# En side med link, sælger info og ID
####-----------------------------------------------------------

startlink <- "https://www.bilbasen.dk/brugt/bil/volvo/v70?fuel=1&includeengroscvr=true&includeleasing=false"
rawres <- GET(startlink, add_headers(
  "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
  "Accept-Language" = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7",
  "Accept" = "text/html,application/xhtml+xml",
  "Connection" = "keep-alive"
))

rawres$status_code
rawcontent <- httr::content(rawres, as="text")
page <- read_html(rawcontent)

# extract car elements from page - henter alle biler
carlist <- page %>% html_elements("article")

# Tags
Price_tag        <- "[class^='Listing_price']"
Makemodel_tag    <- "[class^='Listing_makeModel']"
detail_tag       <- "[class^='Listing_details']"
Properties_tag   <- "[class^='Listing_properties']"
description_tag  <- "[class^='Listing_description']"
location_tag     <- "[class^='Listing_location']"
Link_tag         <- "[class^='Listing_link']"      # <-- HER ER under fanger den ID-TAG

# Forhandler-tags
Seller_name_tag    <- "div[aria-label='bil sælger']"
Seller_address_tag <- "[data-e2e='seller-address']"
Seller_cvr_tag     <- "[class^='bas-MuiSellerInfoComponent-cvr']"

# Dataframe
cn = c("price","makemodel","details","properties","description",
       "location","link","carid","seller_name","seller_address","seller_cvr","scrapdate")

colldf = as.data.frame(matrix(NA, nrow=0, ncol=12))
colnames(colldf) = cn

## Loop – nu MED ID
for (i in seq_along(carlist)) {
  
  car <- carlist[[i]]
  
  price       <- car %>% html_element(Price_tag) %>% html_text(trim=TRUE)
  makemodel   <- car %>% html_element(Makemodel_tag) %>% html_text(trim=TRUE)
  details     <- car %>% html_elements(detail_tag) %>% html_text(trim=TRUE) %>% paste0(collapse="_")
  properties  <- car %>% html_element(Properties_tag) %>% html_text(trim=TRUE)
  description <- car %>% html_element(description_tag) %>% html_text(trim=TRUE)
  location    <- car %>% html_element(location_tag) %>% html_text(trim=TRUE)
  link        <- car %>% html_element(Link_tag) %>% html_attr("href")          # <-- LINK
  
  carid       <- sub(".*/", "", link)                                          # <-- Her får vi CAR ID ud fra URL link
  
  # hent underside for netop denne bil
  rawres_bil  <- GET(link, add_headers("User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)",
                                       "Accept-Language" = "da-DK,da;q=0.9,en-US;q=0.8,en;q=0.7",
                                       "Accept" = "text/html,application/xhtml+xml",
                                       "Connection" = "keep-alive"))
  
  bilside     <- read_html(content(rawres_bil, as="text"))
  
  seller_name    <- bilside %>% html_elements(Seller_name_tag) %>% html_element("h2") %>% html_text(trim=TRUE)
  seller_address <- bilside %>% html_element(Seller_address_tag) %>% html_text(trim=TRUE)
  seller_cvr     <- bilside %>% html_element(Seller_cvr_tag) %>% html_text(trim=TRUE)
  
  
  tmpdf <- data.frame(
    price, makemodel, details, properties,
    description, location, link, carid,                    # <-- ID med i df
    seller_name, seller_address, seller_cvr,
    Sys.time()
  )
  
  colldf <- rbind(colldf, tmpdf)
}

colldf


