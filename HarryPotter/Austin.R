#Austin
# jane aust
jsss <- books %>% filter(book=="Sense & Sensibility")
# bigrams
jsswords <- jsss %>% unnest_tokens(words,text,token = "words")
jsswordscount <- jsswords %>% count(words, sort = T)
stweng=stop_words
jssbigrams <- jsss %>% unnest_tokens(bigrams,text,token = "ngrams", n=2)
jssbigrams <- jssbigrams %>% separate(bigrams, c("w1","w2"))

# filter on gender He,S
flist=c("He","he","She","she")
jssbigramsGender <- jssbigrams %>% filter(w1 %in% flist)
jssbigramsGenderCount <- jssbigramsGender %>% group_by(w1) %>% count(w2)

# dobbelt barplot: for hvert ord en søjle for he og she
jssbigramsGenderCountWide <- jssbigramsGenderCount %>%
  mutate(w1 = tolower(w1)) %>%
  group_by(w2, w1) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  pivot_wider(names_from = w1, values_from = n, values_fill = 0)

jssbigramsGenderCountTop10 <- jssbigramsGenderCountWide %>%
  mutate(total = he + she) %>%
  arrange(desc(total)) %>%
  slice_head(n = 10)

jssbigramsGenderCountTop10Long <- jssbigramsGenderCountTop10 %>%
  mutate(w2 = factor(w2, levels = rev(w2))) %>%
  pivot_longer(cols = c(he, she), names_to = "gender", values_to = "antal")

ggplot(jssbigramsGenderCountTop10Long, aes(x = w2, y = antal, fill = gender)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("he" = "steelblue", "she" = "tomato")) +
  labs(
    title = "Top 10 ord efter he/she",
    x = "Ord",
    y = "Antal",
    fill = "Pronomen"
  )

# ord kun brugt efter enten he eller she (to paneler som i eksemplet)
jssbigramsOnlyOneGender <- jssbigramsGenderCountWide %>%
  mutate(
    he_only = if_else(she == 0, he, 0),
    she_only = if_else(he == 0, she, 0)
  ) %>%
  select(w2, he_only, she_only) %>%
  pivot_longer(cols = c(he_only, she_only), names_to = "gender", values_to = "antal") %>%
  mutate(gender = recode(gender, he_only = "he", she_only = "she")) %>%
  filter(antal > 0) %>%
  group_by(gender) %>%
  slice_max(order_by = antal, n = 10, with_ties = FALSE) %>%
  ungroup()

ggplot(jssbigramsOnlyOneGender, aes(x = antal, y = reorder(w2, antal), fill = gender)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~gender, scales = "free_y") +
  scale_fill_manual(values = c("he" = "#E76F51", "she" = "#00A7A7")) +
  labs(
    title = "Ord kun brugt efter 'he' og 'she'",
    x = "Antal",
    y = "Ord"
  ) +
  theme_minimal()
