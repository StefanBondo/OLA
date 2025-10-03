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
                                geo=c("DK", "BE", "NL","SE","AT", "DE","FR","IT","ES")))
                                

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


library(dplyr)
library(tidyr)
library(zoo)   # kun for pæn sortering af kvartaler

# Antag at din data.frame hedder df
lande <- c("DK","BE","NL","SE","AT","DE","FR","IT","ES")

df_wide <- EUforbug %>%
  # (valgfrit) sortér og filtrér periode/lande
  mutate(time_q = as.yearqtr(time, format = "%Y-Q%q")) %>%
  filter(
    geo %in% lande,
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
head(df_wide)

df_wide_med_corona_til_2024 <- df_wide %>% 
  slice(c(81:98))

summary(df_wide_2020Q1_til_2024)

df_wide_2020Q1_til_2024 <- as.data.frame(df_wide_med_corona_til_2024)


