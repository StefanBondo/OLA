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
})


text <- pdf_text("Strategisk vækstplan - Hovedopgave.pdf")

# 🔥 Gør som i slides: én linje pr. række
df <- data.frame(text = text) %>%
  separate_rows(text, sep = "\n")

# 🔤 Tokenization (ét ord pr. række)
words <- df %>%
  unnest_tokens(word, text)

# 🔢 Optælling af ord
word_count <- words %>%
  count(word, sort = TRUE)

# Se top 20 ord
head(word_count, 20)

# 🚫 Fjern stopord (dansk + engelsk)
data("stop_words")
da_stop_words <- tibble(word = stopwords("da", source = "snowball"))
all_stop_words <- bind_rows(stop_words %>% select(word), da_stop_words) %>%
  distinct()

words_clean <- words %>%
  anti_join(all_stop_words, by = "word")

word_count_clean <- words_clean %>%
  count(word, sort = TRUE)

# Se top 20 "vigtige" ord
head(word_count_clean, 20)

# 📊 Plot (som på slide)
word_count_clean %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 mest brugte ord", x = "", y = "Antal")

# ------------------------------
# Sidste opgave: Sentiment med AFINN
# ------------------------------

# Dansk AFINN (ord \t score)
afinn_da <- read_tsv(
  "https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-da-32.txt",
  col_names = c("word", "value"), show_col_types = FALSE)

sentiment_words <- words_clean %>%
  inner_join(afinn_da, by = "word")

# Hvor mange unikke ord fra AFINN findes i teksten?
n_unique_afinn_words <- sentiment_words %>%
  distinct(word) %>%
  nrow()

# Hvor mange totale tokens i teksten har en sentiment-score?
n_total_afinn_tokens <- nrow(sentiment_words)

cat("Unikke AFINN-ord i teksten:", n_unique_afinn_words, "\n")
cat("Totale tokens med AFINN-score:", n_total_afinn_tokens, "\n")

# Samlet sentiment-score for dokumentet
total_sentiment_score <- sentiment_words %>%
  summarise(total_score = sum(value, na.rm = TRUE)) %>%
  pull(total_score)

cat("Samlet sentiment-score:", total_sentiment_score, "\n")

# Validering: se de mest hyppige positive/negative ord i dine data
sentiment_validation <- sentiment_words %>%
  count(word, value, sort = TRUE)

cat("\nTop 10 negative ord:\n")
print(sentiment_validation %>% filter(value < 0) %>% slice_max(n, n = 10))

cat("\nTop 10 positive ord:\n")
print(sentiment_validation %>% filter(value > 0) %>% slice_max(n, n = 10))

# ------------------------------
# LM: Hvilke ord haenger sammen? (bigrams)
# ------------------------------

bigrams <- df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_clean <- bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% all_stop_words$word) %>%
  filter(!word2 %in% all_stop_words$word) %>%
  filter(str_detect(word1, "^[a-zæøå]+$")) %>%
  filter(str_detect(word2, "^[a-zæøå]+$")) %>%
  unite(bigram, word1, word2, sep = " ")

bigram_count <- bigrams_clean %>%
  count(bigram, sort = TRUE)

cat("\nTop 20 ordpar (bigrams):\n")
print(head(bigram_count, 20))

# Plot af de 10 mest hyppige ordpar
bigram_count %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 ordpar der haenger sammen", x = "", y = "Antal")

