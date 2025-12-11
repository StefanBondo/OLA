library(eurostat)
library(restatapi)
library(dplyr)
library(tidyr)
library(zoo)


dfforbrug <- get_eurostat_dsd("tipsho41")


unique(dfforbrug$concept)
dfforbrug %>% filter(concept=="freq")
dfforbrug %>% filter(concept=="unit")
dfforbrug %>% filter(concept=="s_adj")
dfforbrug %>% filter(concept=="na_item")
dfforbrug %>% filter(concept=="geo")

dfgas_ny

EUforbug <- get_eurostat_data("tipsho41",
                              filters=list(
                                freq="Q",
                                unit="PD10_NAC",
                                s_adj="NSA Unadjusted data",
                                na_item="P31_S14_S15",
                                time=">1999",
                                geo=c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
                                      "DE","EL","HU","IS","IE","IT","LV","LI","LT","LU",
                                      "MT","NL","NO","PL","PT","RO","SK","SI","ES","SE",
                                      "CH","UK","AL","BA","ME","MK","RS","TR")))

                              
EUforbug <- EUforbug %>%
  group_by(geo) %>%
  mutate(
    real_vækst = (values / lag(values, 4) - 1) * 100
  ) %>%
  ungroup()
                                

EUforbug$real_vækst <-round(EUforbug$real_vækst,2)


EUforbug <-  EUforbug %>% 
  rename(
    real_vækst_pct = "real_vækst"
  )

   # kun for pæn sortering af kvartaler

# Antag at din data.frame hedder df
lande <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR",
             "DE","EL","HU","IS","IE","IT","LV","LI","LT","LU",
             "MT","NL","NO","PL","PT","RO","SK","SI","ES","SE",
             "CH","UK","AL","BA","ME","MK","RS","TR")

df_wide <- EUforbug %>%
  # (valgfrit) sortér og filtrér periode/lande
  mutate(time_q = as.yearqtr(time, format = "%Y-Q%q")) %>%
  filter(
    geo %in% lande,
    time_q >= as.yearqtr("2000 Q1"),
    time_q <= as.yearqtr("2025 Q3")
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
head(df_wide)

df_wide_med_corona_til_2024 <- df_wide %>% 
  slice(c(81:98))


#Finde den mindste værdi
means <- colMeans(df_wide_med_corona_til_2024[ , -1], na.rm = TRUE)
min_land <- names(means)[which.min(means)]
min_vaerdi <- min(means)

paste("Mindste mean:", min_vaerdi, "for", min_land)


#Lav barplot
library(tidyr)
library(dplyr)
library(ggplot2)

df_long <- df_wide_med_corona_til_2024 %>%
  pivot_longer(-time, names_to = "geo", values_to = "value")

df_long %>%
  group_by(geo) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(geo, mean_value), y = mean_value, fill = mean_value)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "steelblue", high = "red") +
  labs(title = "Grækenland er det EU land med laveste gennemsnitligtkvartalsvise 
årlige realvækst i husholdningernes forbrugsudgift",
       x = "Land",
       caption = "Kilde: EUROSTAT API",
       y = "Gennemsnitlig værdi") +
  theme_minimal()





