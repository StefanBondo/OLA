suppressPackageStartupMessages({library(pdftools)
  library(tidytext)
  library(textdata)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(ggplot2)
  library(stopwords)
  library(dplyr)
  library(tidyr)
  library(reshape2)
  library(wordcloud)
  library(janeaustenr)
  library(stringr)
})

test2 <- readRDS("hpworksdf_med_titler.rds")

HarryPotter <- pdf_text("harrypotter.pdf")

df <- as.data.frame(HarryPotter)

Bog_navne <- c(
  "Harry Potter and the Sorcerer’s Stone",
  "Harry Potter and the Chamber of Secrets",
  "Harry Potter and the Prisoner of Azkaban",
  "Harry Potter and the Goblet of Fire",
  "Harry Potter and the Order of the Phoenix",
  "Harry Potter and the Half-Blood Prince",
  "Harry Potter and the Deathly Hallows"
)

start_kapitler <- c(
  "THE BOY WHO LIVED",
  "THE WORST BIRTHDAY",
  "OWL POST",
  "THE RIDDLE HOUSE",
  "DUDLEY DEMENTED",
  "THE OTHER MINISTER",
  "THE DARK LORD ASCENDING"
)

start_rækker <- integer(length(start_kapitler))
seneste_start <- 0

for (i in seq_along(start_kapitler)) {
  match_rækker <- which(grepl(start_kapitler[i], df$HarryPotter, fixed = TRUE))
  match_rækker <- match_rækker[match_rækker > seneste_start]
  start_rækker[i] <- match_rækker[1]
  seneste_start <- start_rækker[i]
}

df <- df %>%
  mutate(
    række_nr = row_number(),
    Bog = case_when(
      række_nr >= start_rækker[1] & række_nr < start_rækker[2] ~ Bog_navne[1],
      række_nr >= start_rækker[2] & række_nr < start_rækker[3] ~ Bog_navne[2],
      række_nr >= start_rækker[3] & række_nr < start_rækker[4] ~ Bog_navne[3],
      række_nr >= start_rækker[4] & række_nr < start_rækker[5] ~ Bog_navne[4],
      række_nr >= start_rækker[5] & række_nr < start_rækker[6] ~ Bog_navne[5],
      række_nr >= start_rækker[6] & række_nr < start_rækker[7] ~ Bog_navne[6],
      række_nr >= start_rækker[7] ~ Bog_navne[7],
      TRUE ~ NA_character_
    )
  ) %>%
  select(-række_nr)

test <- df %>% filter(!is.na(Bog))

