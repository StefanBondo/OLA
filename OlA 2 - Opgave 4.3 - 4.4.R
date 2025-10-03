#Dansk industri LM

##1 Fødevarer LM
fødevarer_di <- lm(di_tal_2000Q1_2024Q2$DI_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Fødevarer mv.`)
summary(fødevarer_di)

##2 Drikkevarer og tobak
drikkevarer_tobak_di <- lm(di_tal_2000Q1_2024Q2$DI_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Drikkevarer og tobak mv.`)
summary(drikkevarer_tobak_di)

##3 Beklædning og fodtøj
Beklædning_fodtøj_di <- lm(di_tal_2000Q1_2024Q2$DI_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Beklædning og fodtøj`)
summary(Beklædning_fodtøj_di)

##4 boligbenyttelse
Boligbenyttelse_di <- lm(di_tal_2000Q1_2024Q2$DI_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$Boligbenyttelse)
summary(Boligbenyttelse_di)

##5 Elektricitet, fjernvarme og andet brændsel
Elektricitet_fjernvarme_andet_brændsel_di <- lm(di_tal_2000Q1_2024Q2$DI_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Elektricitet, fjernvarme og andet brændsel`)
summary(Elektricitet_fjernvarme_andet_brændsel_di)

##6 Boligudstyr, husholdningstjenester mv.
Boligudstyr_husholdningstjenester_mv_di <- lm(di_tal_2000Q1_2024Q2$DI_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Boligudstyr, husholdningstjenester mv.`)
summary(Boligudstyr_husholdningstjenester_mv_di)

##7 Køb af køretøjer
Køb_af_køretøjer_di <- lm(di_tal_2000Q1_2024Q2$DI_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Køb af køretøjer`)
summary(Køb_af_køretøjer_di)

##8 Drift af køretøjer og transporttjenester
Drift_af_køretøjer_transporttjenester_di <- lm(di_tal_2000Q1_2024Q2$DI_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Drift af køretøjer og transporttjenester`)
summary(Drift_af_køretøjer_transporttjenester_di)

##9 Fritid, sport og kultur
Fritid_sport_kultur_di <- lm(di_tal_2000Q1_2024Q2$DI_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Fritid, sport og kultur`)
summary(Fritid_sport_kultur_di)

##10 Restauranter og hoteller
Restauranter_hoteller_di <- lm(di_tal_2000Q1_2024Q2$DI_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Restauranter og hoteller`)
summary(Restauranter_hoteller_di)

##11 Forsikring og finansielle tjenester
Forsikring_finansielle_tjenester_di <- lm(di_tal_2000Q1_2024Q2$DI_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Forsikring og finansielle tjenester`)
summary(Forsikring_finansielle_tjenester_di)


# Danmark statisstik

##1
Fødevarer_DST <- lm(dst_tal_2000Q1_2024Q2$dst_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Fødevarer mv.`)
summary(FødevarerDST)

##2
Drikkevarer_Tobak_DST <- lm(dst_tal_2000Q1_2024Q2$dst_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Drikkevarer og tobak mv.`)
summary(Drikkevarer_Tobak_DST)

##3
Beklædning_fodtøj_DST <- lm(dst_tal_2000Q1_2024Q2$dst_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Beklædning og fodtøj`)
summary(Beklædning_fodtøj_DST)

##4
Boligbenyttelse_DST <- lm(dst_tal_2000Q1_2024Q2$dst_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Boligbenyttelse`)
summary(Boligbenyttelse_DST)

##5
Elektricitet_fjernvarme_andet_brændsel_DST <- lm(dst_tal_2000Q1_2024Q2$dst_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Elektricitet, fjernvarme og andet brændsel`)
summary(Elektricitet_fjernvarme_andet_brændsel_DST)

##6
Boligudstyr_husholdningstjenester_mv_DST <- lm(dst_tal_2000Q1_2024Q2$dst_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Boligudstyr, husholdningstjenester mv.`)
summary(Boligudstyr_husholdningstjenester_mv_DST)

##7
Køb_af_køretøjer_transport_DST <- lm(dst_tal_2000Q1_2024Q2$dst_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Køb af køretøjer`)
summary(Køb_af_køretøjer_transport_DST)

##8
Drift_af_køretøjer_transporttjenester_DST <- lm(dst_tal_2000Q1_2024Q2$dst_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Drift af køretøjer og transporttjenester`)
summary(Drift_af_køretøjer_transporttjenester_DST)

##9
Fritid_sport_kultur_DST <- lm(dst_tal_2000Q1_2024Q2$dst_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Fritid, sport og kultur`)
summary(Fritid_sport_kultur_DST)

##10
Restauranter_hoteller_DST <- lm(dst_tal_2000Q1_2024Q2$dst_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Restauranter og hoteller`)
summary(Restauranter_hoteller_DST)

##11
Forsikring_finansielle_tjenester_DST <- lm(dst_tal_2000Q1_2024Q2$dst_Forbrugertillidindikator ~ Forbrugergrupper_2000Q1_2024Q2$`Forsikring og finansielle tjenester`)
summary(Forsikring_finansielle_tjenester_DST)



#------------------

Finder alle objekter i Global Environment hvis navn slutter på _DST og _DI
modelsdst <- mget(ls(pattern = "_DST$"), inherits = TRUE)
modelsdi  <- mget(ls(pattern = "_di$"),  inherits = TRUE)

modelsall <- c(modelsdst, modelsdi)

Summary-lister (22 stk hvis du har 11 DST + 11 DI)
summariesdst <- lapply(modelsdst, summary)
summariesdi  <- lapply(modelsdi,  summary)
summariesall <- lapply(modelsall, summary)


summariesdi
summariesdst
summariesall


