library(httr)
library(rvest)
library(dplyr)

get_station <- function(group, code){
  
  # URL til siden der indeholder token
  token_page_url <- paste0(
    "https://envs2.au.dk/Luftdata/Presentation/table/",
    group, "/", code
  )
  
  # Hent token-siden (HTML)
  token_page <- httr::GET(token_page_url)
  html <- read_html(httr::content(token_page, as = "text"))
  
  # Udtræk CSRF-token
  token <- html %>% 
    html_node("input[name='__RequestVerificationToken']") %>% 
    html_attr("value")
  
  if(is.na(token)) {
    message("Kunne ikke hente token for ", code)
    return(NULL)
  }
  
  # Lav POST med token til MainTable-endpoint
  post_url <- paste0(
    "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/",
    group, "/", code
  )
  
  res <- httr::POST(
    post_url,
    body = list("__RequestVerificationToken" = token),
    encode = "form"
  )
  
  # Konverter til HTML
  html <- httr::content(res, "text")
  doc  <- read_html(html)
  
  # Parse tabellen
  tables <- html_table(doc, fill = TRUE)
  
  if(length(tables) == 0){
    message("Ingen tabel retur for ", code)
    return(NULL)
  }
  
  df <- tables[[1]]
  return(df)
}

HCAB  <- get_station("Copenhagen", "HCAB")
ANHO  <- get_station("Rural", "ANHO")
RISOE <- get_station("Rural", "RISOE")
AARH3 <- get_station("Aarhus", "AARH3")

