#Få data udfra 2000Q1 til 2025Q2
Forbrugesgrupper_2000Q1_2025Q2 <- Forbrugsgrupper %>% slice(c(5:106))
di_tal_2000Q1_2025Q2 <- di_tal_2000_2025Q3 %>% slice(c(1:102))

##1 Fødevarer LM
fødevarer_di_2025 <- lm(di_tal_2000Q1_2025Q2$gennemsnit ~ 
                          Forbrugesgrupper_2000Q1_2025Q2$`Fødevarer mv.`)
summary(fødevarer_di_2025)

##2 Drikkevarer og tobak
drikkevarer_tobak_di_2025 <- lm(di_tal_2000Q1_2025Q2$gennemsnit ~ 
                                  Forbrugesgrupper_2000Q1_2025Q2$`Drikkevarer og tobak mv.`)
summary(drikkevarer_tobak_di_2025)

##3 Beklædning og fodtøj
Beklædning_fodtøj_di_2025 <- lm(di_tal_2000Q1_2025Q2$gennemsnit ~ 
                                  Forbrugesgrupper_2000Q1_2025Q2$`Beklædning og fodtøj`)
summary(Beklædning_fodtøj_di_2025)

##4 Boligbenyttelse
Boligbenyttelse_di_2025 <- lm(di_tal_2000Q1_2025Q2$gennemsnit ~ 
                                Forbrugesgrupper_2000Q1_2025Q2$Boligbenyttelse)
summary(Boligbenyttelse_di_2025)

##5 Elektricitet, fjernvarme og andet brændsel
Elektricitet_fjernvarme_andet_brændsel_di_2025 <- lm(di_tal_2000Q1_2025Q2$gennemsnit ~ 
                                                       Forbrugesgrupper_2000Q1_2025Q2$`Elektricitet, fjernvarme og andet brændsel`)
summary(Elektricitet_fjernvarme_andet_brændsel_di_2025)

##6 Boligudstyr, husholdningstjenester mv.
Boligudstyr_husholdningstjenester_mv_di_2025 <- lm(di_tal_2000Q1_2025Q2$gennemsnit ~ 
                                                     Forbrugesgrupper_2000Q1_2025Q2$`Boligudstyr, husholdningstjenester mv.`)
summary(Boligudstyr_husholdningstjenester_mv_di_2025)

##7 Køb af køretøjer
Køb_af_køretøjer_di_2025 <- lm(di_tal_2000Q1_2025Q2$gennemsnit ~ 
                                 Forbrugesgrupper_2000Q1_2025Q2$`Køb af køretøjer`)
summary(Køb_af_køretøjer_di_2025)

##8 Drift af køretøjer og transporttjenester
Drift_af_køretøjer_transporttjenester_di_2025 <- lm(di_tal_2000Q1_2025Q2$gennemsnit ~ 
                                                      Forbrugesgrupper_2000Q1_2025Q2$`Drift af køretøjer og transporttjenester`)
summary(Drift_af_køretøjer_transporttjenester_di_2025)

##9 Fritid, sport og kultur
Fritid_sport_kultur_di_2025 <- lm(di_tal_2000Q1_2025Q2$gennemsnit ~ 
                                    Forbrugesgrupper_2000Q1_2025Q2$`Fritid, sport og kultur`)
summary(Fritid_sport_kultur_di_2025)

##10 Restauranter og hoteller
Restauranter_hoteller_di_2025 <- lm(di_tal_2000Q1_2025Q2$gennemsnit ~ 
                                      Forbrugesgrupper_2000Q1_2025Q2$`Restauranter og hoteller`)
summary(Restauranter_hoteller_di_2025)

##11 Forsikring og finansielle tjenester
Forsikring_finansielle_tjenester_di_2025 <- lm(di_tal_2000Q1_2025Q2$gennemsnit ~ 
                                                 Forbrugesgrupper_2000Q1_2025Q2$`Forsikring og finansielle tjenester`)
summary(Forsikring_finansielle_tjenester_di_2025)


#laver den rigtige tidsperiode
dst_tal_2000Q1_2025Q2 <- dst_tal_2000_2025Q3 %>% slice(c(1:102))

#LM
##1
Fødevarer_DST_2025 <- lm(dst_tal_2000Q1_2025Q2$gennemsnit ~ 
                           Forbrugesgrupper_2000Q1_2025Q2$`Fødevarer mv.`)
summary(Fødevarer_DST_2025)

##2
Drikkevarer_Tobak_DST_2025 <- lm(dst_tal_2000Q1_2025Q2$gennemsnit ~ 
                                   Forbrugesgrupper_2000Q1_2025Q2$`Drikkevarer og tobak mv.`)
summary(Drikkevarer_Tobak_DST_2025)

##3
Beklædning_fodtøj_DST_2025 <- lm(dst_tal_2000Q1_2025Q2$gennemsnit ~ 
                                   Forbrugesgrupper_2000Q1_2025Q2$`Beklædning og fodtøj`)
summary(Beklædning_fodtøj_DST_2025)

##4
Boligbenyttelse_DST_2025 <- lm(dst_tal_2000Q1_2025Q2$gennemsnit ~ 
                                 Forbrugesgrupper_2000Q1_2025Q2$`Boligbenyttelse`)
summary(Boligbenyttelse_DST_2025)

##5
Elektricitet_fjernvarme_andet_brændsel_DST_2025 <- lm(dst_tal_2000Q1_2025Q2$gennemsnit ~ 
                                                        Forbrugesgrupper_2000Q1_2025Q2$`Elektricitet, fjernvarme og andet brændsel`)
summary(Elektricitet_fjernvarme_andet_brændsel_DST_2025)

##6
Boligudstyr_husholdningstjenester_mv_DST_2025 <- lm(dst_tal_2000Q1_2025Q2$gennemsnit ~ 
                                                      Forbrugesgrupper_2000Q1_2025Q2$`Boligudstyr, husholdningstjenester mv.`)
summary(Boligudstyr_husholdningstjenester_mv_DST_2025)

##7
Køb_af_køretøjer_transport_DST_2025 <- lm(dst_tal_2000Q1_2025Q2$gennemsnit ~ 
                                            Forbrugesgrupper_2000Q1_2025Q2$`Køb af køretøjer`)
summary(Køb_af_køretøjer_transport_DST_2025)

##8
Drift_af_køretøjer_transporttjenester_DST_2025 <- lm(dst_tal_2000Q1_2025Q2$gennemsnit ~ 
                                                       Forbrugesgrupper_2000Q1_2025Q2$`Drift af køretøjer og transporttjenester`)
summary(Drift_af_køretøjer_transporttjenester_DST_2025)

##9
Fritid_sport_kultur_DST_2025 <- lm(dst_tal_2000Q1_2025Q2$gennemsnit ~ 
                                     Forbrugesgrupper_2000Q1_2025Q2$`Fritid, sport og kultur`)
summary(Fritid_sport_kultur_DST_2025)

##10
Restauranter_hoteller_DST_2025 <- lm(dst_tal_2000Q1_2025Q2$gennemsnit ~ 
                                       Forbrugesgrupper_2000Q1_2025Q2$`Restauranter og hoteller`)
summary(Restauranter_hoteller_DST_2025)

##11
Forsikring_finansielle_tjenester_DST_2025 <- lm(dst_tal_2000Q1_2025Q2$gennemsnit ~ 
                                                  Forbrugesgrupper_2000Q1_2025Q2$`Forsikring og finansielle tjenester`)
summary(Forsikring_finansielle_tjenester_DST_2025)

# Finder alle objekter i Global Environment hvis navn slutter på _DST_2025 og _di_2025
modelsdst_2025 <- mget(ls(pattern = "_DST_2025$"), inherits = TRUE)
modelsdi_2025  <- mget(ls(pattern = "_di_2025$"),  inherits = TRUE)

modelsall_2025 <- c(modelsdst_2025, modelsdi_2025)

# Summary-lister
summariesdst_2025 <- lapply(modelsdst_2025, summary)
summariesdi_2025  <- lapply(modelsdi_2025,  summary)
summariesall_2025 <- lapply(modelsall_2025, summary)


summariesdi_2025
summariesdst_2025
summariesall_2025

library(dplyr)
library(purrr)
library(tidyr)
library(tibble)

# --- 1) Model-niveau tal (R2, Adj R2, residual SD, F-test) ---
model_metrics_2025 <- imap_dfr(summariesall_2025, ~{
  s <- .x
  f <- s$fstatistic
  f_p <- if (!is.null(f)) pf(f[1], f[2], f[3], lower.tail = FALSE) else NA_real_
  tibble(
    Model   = .y,
    R2      = s$r.squared,
    Adj_R2  = s$adj.r.squared,
    Sigma   = s$sigma,
    F_stat  = unname(if (!is.null(f)) f[1] else NA_real_),
    DF1     = unname(if (!is.null(f)) f[2] else NA_real_),
    DF2     = unname(if (!is.null(f)) f[3] else NA_real_),
    F_pval  = f_p
  )
})

# --- 2) Alle koefficienter (estimate, std.error, t, p) ---
coef_table_2025 <- imap_dfr(summariesall_2025, ~{
  s  <- .x
  cf <- as.data.frame(s$coefficients)
  tibble(Model = .y, term = rownames(cf)) |>
    bind_cols(as_tibble(cf)) |>
    rename(
      estimate  = Estimate,
      std_error = `Std. Error`,
      statistic = `t value`,
      p_value   = `Pr(>|t|)`
    )
})

# Resultater
model_metrics_2025
coef_table_2025

options(scipen = 999)

write_xlsx(coef_table_2025, "coef_table_2025.xlsx")
