dfforbrug <- get_eurostat_dsd("tipsho41")


unique(dfforbrug$concept)
dfforbrug %>% filter(concept=="freq")
dfforbrug %>% filter(concept=="unit")
dfforbrug %>% filter(concept=="s_adj")
dfforbrug %>% filter(concept=="na_item")
dfforbrug %>% filter(concept=="geo")

dfgas_ny

udvalgte_EUforbug <- get_eurostat_data("tipsho41",
                              filters=list(
                                freq="Q",
                                unit="PD10_NAC",
                                s_adj="NSA Unadjusted data",
                                na_item="P31_S14_S15",
                                time=">1999",
                                geo=c("DK", "BE", "NL","SE","AT", "DE","FR","IT","ES")))


udvalgte_EUforbug <- udvalgte_EUforbug %>%
  group_by(geo) %>%
  mutate(
    real_vækst = (values / lag(values, 4) - 1) * 100
  ) %>%
  ungroup()


udvalgte_EUforbug$real_vækst <-round(udvalgte_EUforbug$real_vækst,2)


udvalgte_EUforbug <-  udvalgte_EUforbug %>% 
  rename(
    real_vækst_pct = "real_vækst"
  )


library(dplyr)
library(tidyr)
library(zoo)   # kun for pæn sortering af kvartaler

# Antag at din data.frame hedder df
udvalgte_EUlande <- c("DK","BE","NL","SE","AT","DE","FR","IT","ES")

df_wide_udvalgte_lande <- udvalgte_EUforbug %>%
  # (valgfrit) sortér og filtrér periode/lande
  mutate(time_q = as.yearqtr(time, format = "%Y-Q%q")) %>%
  filter(
    geo %in% udvalgte_EUlande,
    time_q >= as.yearqtr("2000 Q1"),
    time_q <= as.yearqtr("2024 Q2")
  ) %>%
  # behold kun det vi skal bruge til pivot
  select(time, geo, real_vækst_pct) %>%
  distinct() %>%
  # pivoter: kvartal i rækker, lande i kolonner
  pivot_wider(
    names_from  = geo,
    values_from = real_vækst_pct
  ) %>%
  # sortér rækker kronologisk
  arrange(as.yearqtr(time, format = "%Y-Q%q"))

# Kig
head(df_wide_udvalgte_lande)

summary(df_wide_udvalgte_lande)

# lav barplot

df_long_udvalgte_lande <- df_wide_udvalgte_lande %>%
  pivot_longer(-time, names_to = "geo", values_to = "value")

df_long_udvalgte_lande %>%
  group_by(geo) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(geo, mean_value), y = mean_value, fill = mean_value)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "steelblue", high = "red") +
  labs(title = "Beligum er det land med højeste gennemsnitligtkvartalsvise 
årlige realvækst i husholdningernes forbrugsudgift",
       x = "Land",
       caption = "Kilde: EUROSTAT API",
       y = "Gennemsnitlig værdi") +
  theme_minimal()

