FORV1_kvartal_DI_slice <- FORV1_kvartal_DI %>% slice(-120)

FORV1_kvartal_DST_slice <- FORV1_kvartal_DST %>% slice(-120)

## -----------------------------
##  DST: Forbrugertillid (y)
## -----------------------------

## 1 Fødevarer mv.
lm_fødevarer_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                         Forbruggrupper$`Fødevarer.mv.`)
summary(lm_fødevarer_DST)

## 2 Drikkevarer og tobak mv.
lm_drikkevarer_tobak_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                                 Forbruggrupper$`Drikkevarer.og.tobak.mv.`)
summary(lm_drikkevarer_tobak_DST)

## 3 Beklædning og fodtøj
lm_beklædning_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                          Forbruggrupper$`Beklædning.og.fodtøj`)
summary(lm_beklædning_DST)

## 4 Boligbenyttelse
lm_boligbenyttelse_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                               Forbruggrupper$`Boligbenyttelse`)
summary(lm_boligbenyttelse_DST)

## 5 Elektricitet, fjernvarme og andet brændsel
lm_energi_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                      Forbruggrupper$`Elektricitet..fjernvarme.og.andet.brændsel`)
summary(lm_energi_DST)

## 6 Boligudstyr, husholdningstjenester mv.
lm_boligudstyr_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                           Forbruggrupper$`Boligudstyr..husholdningstjenester.mv.`)
summary(lm_boligudstyr_DST)

## 7 Medicin, lægeudgifter o.l.
lm_medicin_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                       Forbruggrupper$`Medicin..lægeudgifter.o.l.`)
summary(lm_medicin_DST)

## 8 Køb af køretøjer
lm_køb_køretøjer_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                             Forbruggrupper$`Køb.af.køretøjer`)
summary(lm_køb_køretøjer_DST)

## 9 Drift af køretøjer og transporttjenester
lm_drift_køretøjer_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                               Forbruggrupper$`Drift.af.køretøjer.og.transporttjenester`)
summary(lm_drift_køretøjer_DST)

## 10 Information og kommunikation
lm_info_kom_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                        Forbruggrupper$`Information.og.kommunikation`)
summary(lm_info_kom_DST)

## 11 Fritid, sport og kultur
lm_fritid_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                      Forbruggrupper$`Fritid..sport.og.kultur`)
summary(lm_fritid_DST)

## 12 Undervisning
lm_undervisning_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                            Forbruggrupper$`Undervisning`)
summary(lm_undervisning_DST)

## 13 Restauranter og hoteller
lm_restauranter_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                            Forbruggrupper$`Restauranter.og.hoteller`)
summary(lm_restauranter_DST)

## 14 Forsikring og finansielle tjenester
lm_forsikring_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                          Forbruggrupper$`Forsikring.og.finansielle.tjenester`)
summary(lm_forsikring_DST)

## 15 Andre varer og tjenester
lm_andrevarer_DST <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ 
                          Forbruggrupper$`Andre.varer.og.tjenester`)
summary(lm_andrevarer_DST)



## -----------------------------
##  DI: DI_FTI (y)
## -----------------------------

## 1 Fødevarer mv.
lm_fødevarer_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                        Forbruggrupper$`Fødevarer.mv.`)
summary(lm_fødevarer_DI)

## 2 Drikkevarer og tobak mv.
lm_drikkevarer_tobak_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                                Forbruggrupper$`Drikkevarer.og.tobak.mv.`)
summary(lm_drikkevarer_tobak_DI)

## 3 Beklædning og fodtøj
lm_beklædning_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                         Forbruggrupper$`Beklædning.og.fodtøj`)
summary(lm_beklædning_DI)

## 4 Boligbenyttelse
lm_boligbenyttelse_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                              Forbruggrupper$`Boligbenyttelse`)
summary(lm_boligbenyttelse_DI)

## 5 Elektricitet, fjernvarme og andet brændsel
lm_energi_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                     Forbruggrupper$`Elektricitet..fjernvarme.og.andet.brændsel`)
summary(lm_energi_DI)

## 6 Boligudstyr, husholdningstjenester mv.
lm_boligudstyr_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                          Forbruggrupper$`Boligudstyr..husholdningstjenester.mv.`)
summary(lm_boligudstyr_DI)

## 7 Medicin, lægeudgifter o.l.
lm_medicin_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                      Forbruggrupper$`Medicin..lægeudgifter.o.l.`)
summary(lm_medicin_DI)

## 8 Køb af køretøjer
lm_køb_køretøjer_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                            Forbruggrupper$`Køb.af.køretøjer`)
summary(lm_køb_køretøjer_DI)

## 9 Drift af køretøjer og transporttjenester
lm_drift_køretøjer_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                              Forbruggrupper$`Drift.af.køretøjer.og.transporttjenester`)
summary(lm_drift_køretøjer_DI)

## 10 Information og kommunikation
lm_info_kom_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                       Forbruggrupper$`Information.og.kommunikation`)
summary(lm_info_kom_DI)

## 11 Fritid, sport og kultur
lm_fritid_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                     Forbruggrupper$`Fritid..sport.og.kultur`)
summary(lm_fritid_DI)

## 12 Undervisning
lm_undervisning_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                           Forbruggrupper$`Undervisning`)
summary(lm_undervisning_DI)

## 13 Restauranter og hoteller
lm_restauranter_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                           Forbruggrupper$`Restauranter.og.hoteller`)
summary(lm_restauranter_DI)

## 14 Forsikring og finansielle tjenester
lm_forsikring_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                         Forbruggrupper$`Forsikring.og.finansielle.tjenester`)
summary(lm_forsikring_DI)

## 15 Andre varer og tjenester
lm_andrevarer_DI <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ 
                         Forbruggrupper$`Andre.varer.og.tjenester`)
summary(lm_andrevarer_DI)



#--------------------------------------
#liste
#----------------------------------

library(broom)
library(writexl)

# 1. Tom liste til Excel
excel_list <- list()

# 2. Kolonnenavne for forbrugsgrupper
forbrugs_kolonner <- c(
  "Fødevarer.mv.",
  "Drikkevarer.og.tobak.mv.",
  "Beklædning.og.fodtøj",
  "Boligbenyttelse",
  "Elektricitet..fjernvarme.og.andet.brændsel",
  "Boligudstyr..husholdningstjenester.mv.",
  "Medicin..lægeudgifter.o.l.",
  "Køb.af.køretøjer",
  "Drift.af.køretøjer.og.transporttjenester",
  "Information.og.kommunikation",
  "Fritid..sport.og.kultur",
  "Undervisning",
  "Restauranter.og.hoteller",
  "Forsikring.og.finansielle.tjenester",
  "Andre.varer.og.tjenester"
)

# 3. Loop: lav 30 regressioner og gem både coef + R2
for (kol in forbrugs_kolonner) {
  
  ## ----------------- DST -----------------
  model_dst <- lm(FORV1_kvartal_DST_slice$Forbrugertillid ~ Forbruggrupper[[kol]])
  coef_dst  <- tidy(model_dst)          # Intercept + Beta
  r2_dst    <- glance(model_dst)        # R² og adj. R²
  
  # kombiner i én tabel
  out_dst <- cbind(coef_dst, r2 = r2_dst$r.squared, adj_r2 = r2_dst$adj.r.squared)
  
  excel_list[[paste0("DST_", kol)]] <- out_dst
  
  
  ## ----------------- DI -----------------
  model_di <- lm(FORV1_kvartal_DI_slice$DI_FTI ~ Forbruggrupper[[kol]])
  coef_di  <- tidy(model_di)
  r2_di    <- glance(model_di)
  
  out_di <- cbind(coef_di, r2 = r2_di$r.squared, adj_r2 = r2_di$adj.r.squared)
  
  excel_list[[paste0("DI_", kol)]] <- out_di
}

# 4. Eksportér hele pakken
write_xlsx(excel_list, "Regressioner_DST_DI_med_R2.xlsx")


