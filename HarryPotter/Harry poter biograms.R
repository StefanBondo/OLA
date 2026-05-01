# Harry Potter bigrams: harry vs hermione

hp_source <- test %>%
  filter(Bog == "Harry Potter and the Prisoner of Azkaban")

text_col <- if ("text" %in% names(hp_source)) "text" else "HarryPotter"

flist <- c("harry", "hermione")

hpbigrams <- hp_source %>%
  unnest_tokens(bigrams, all_of(text_col), token = "ngrams", n = 2) %>%
  separate(bigrams, c("w1", "w2"), sep = " ") %>%
  filter(w1 %in% flist) %>%
  filter(!w2 %in% stop_words$word) %>%
  filter(!(w1 == "harry" & w2 == "potter"))

hpCharacterCount <- hpbigrams %>%
  group_by(w1) %>%
  count(w2)

# dobbelt barplot: for hvert ord en søjle for Harry Potter og Hermione Granger
hpCharacterCountWide <- hpCharacterCount %>%
  mutate(w1 = recode(w1,
    "harry" = "harry",
    "hermione" = "hermione"
  )) %>%
  group_by(w2, w1) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = w1, values_from = n, values_fill = 0)

hpCharacterCountTop10 <- hpCharacterCountWide %>%
  mutate(total = harry + hermione) %>%
  arrange(desc(total)) %>%
  slice_head(n = 10)

# ord kun brugt efter enten Harry Potter eller Hermione Granger (to paneler)
hpOnlyOneCharacter <- hpCharacterCountWide %>%
  mutate(
    harry_only = if_else(hermione == 0, harry, 0),
    hermione_only = if_else(harry == 0, hermione, 0)
  ) %>%
  select(w2, harry_only, hermione_only) %>%
  pivot_longer(cols = c(harry_only, hermione_only), names_to = "character", values_to = "antal") %>%
  mutate(character = recode(character,
    harry_only = "Harry",
    hermione_only = "Hermione"
  )) %>%
  filter(antal > 0) %>%
  group_by(character) %>%
  slice_max(order_by = antal, n = 10, with_ties = FALSE) %>%
  ungroup()

ggplot(hpOnlyOneCharacter, aes(x = antal, y = reorder(w2, antal), fill = character)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~character, scales = "free_y") +
  scale_fill_manual(values = c("Harry" = "#E76F51", "Hermione" = "#00A7A7")) +
  labs(
    title = "Ord kun brugt efter Harry og Hermione i 
Harry Potter and the Prisoner of Azkaban",
    x = "Antal",
    y = "Ord"
  ) +
  theme_minimal()



# ord som både Harry og Hermione har sagt efter sig
hpSharedWordsTop10 <- hpCharacterCountWide %>%
  filter(harry > 0, hermione > 0) %>%
  mutate(total = harry + hermione) %>%
  arrange(desc(total)) %>%
  slice_head(n = 10)

hpSharedWordsTop10Long <- hpSharedWordsTop10 %>%
  mutate(w2 = factor(w2, levels = rev(w2))) %>%
  pivot_longer(
    cols = c(harry, hermione),
    names_to = "character",
    values_to = "antal"
  ) %>%
  mutate(character = recode(character, harry = "Harry", hermione = "Hermione"))

ggplot(hpSharedWordsTop10Long, aes(x = w2, y = antal, fill = character)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("Harry" = "steelblue", "Hermione" = "tomato")) +
  labs(
    title = "Top 10 ord som begge bruger efter Harry/Hermione i 
Harry Potter and the Prisoner of Azkaban",
    x = "Ord",
    y = "Antal",
    fill = "Karakter"
  ) +
  theme_minimal()

