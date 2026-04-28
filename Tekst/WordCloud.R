library(dplyr)
library(tidyr)
library(reshape2)
library(wordcloud)

# Forventet fra Tekst.R: sentiment_words med kolonnerne word og value
sentiment_validation <- sentiment_words %>% 
  mutate(sentiment = if_else(value > 0, "positive", "negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(sentiment = factor(sentiment, levels = c("negative", "positive"))) %>%
  complete(word, sentiment, fill = list(n = 0))

comparison_matrix <- sentiment_validation %>%
  acast(word ~ sentiment, value.var = "n", fill = 0)

colnames(comparison_matrix) <- c("Negative", "Positive")

comparison.cloud(
  comparison_matrix,
  max.words = 150,
  colors = c("red", "darkgreen"),
  title.size = 1.1
)

# Tegn labels manuelt i sort, så begge er tydelige
usr <- par("usr")
x_mid <- mean(usr[1:2])
y_top <- usr[4] - 0.04 * diff(usr[3:4])
y_bottom <- usr[3] + 0.04 * diff(usr[3:4])
text(x_mid, y_top, "Negative", col = "black")
text(x_mid, y_bottom, "Positive", col = "black")
