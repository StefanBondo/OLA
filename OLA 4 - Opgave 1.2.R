#Renser description for Emoji og ugyldige tegn
colldf$description <- colldf$description %>%
  gsub("\n", ". ", .) %>%                    # newline → ". "
  gsub("[^A-Za-z0-9ÆØÅæøå., ]", " ", .) %>%  # fjern alt der ikke er bogstaver/tal/., 
  gsub(" +", " ", .) %>%                     # flere mellemrum → ét mellemrum
  trimws()                                   # fjern mellemrum i start/slut

#Renser CVR kolonne for CVR-nr
colldf$seller_cvr <- gsub("CVR-nr.:?\\s*", "", colldf$seller_cvr)
