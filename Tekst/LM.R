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

# ------------------------------
# Simpel LM: Bigram Language Model
# ------------------------------

bigrams_clean <- bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(str_detect(word1, "^[a-zæøå]+$")) %>%
  filter(str_detect(word2, "^[a-zæøå]+$"))

# 3) Lær sandsynlighed for naeste ord: P(word2 | word1)
bigram_lm <- bigrams_clean %>%
  count(word1, word2, sort = TRUE) %>%
  group_by(word1) %>%
  mutate(prob = n / sum(n)) %>%
  ungroup()

cat("Antal laerte ordpar:", nrow(bigram_lm), "\n")

# 4) Funktion: foreslaa naeste ord
predict_next_words <- function(input_word, top_n = 5) {
  bigram_lm %>%
    filter(word1 == input_word) %>%
    arrange(desc(prob)) %>%
    select(word2, n, prob) %>%
    slice_head(n = top_n)
}

# Eksempel: hvilke ord kommer typisk efter "viabill"?
cat("\nTop naeste ord efter 'viabill':\n")
print(predict_next_words("viabill", top_n = 10))

# 5) Simpel tekst-generering med modellen
generate_text <- function(start_word, steps = 12) {
  current_word <- start_word
  output_words <- c(start_word)
  
  for (i in 1:steps) {
    next_candidates <- bigram_lm %>%
      filter(word1 == current_word)
    
    if (nrow(next_candidates) == 0) {
      break
    }
    
    next_word <- sample(next_candidates$word2, size = 1, prob = next_candidates$prob)
    output_words <- c(output_words, next_word)
    current_word <- next_word
  }
  
  paste(output_words, collapse = " ")
}

cat("\nGenereret tekst fra 'viabill':\n")
cat(generate_text("viabill", steps = 15), "\n")