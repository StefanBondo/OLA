#Find den bedste indikator, der alene består af mikroøkonomiske spørgsmål i forbrugertillidsundersøgelsen og 
#sammenlign indikatoren med jeres tidligere svar i opgave 1.

# 1) lav nyt frame til mikro spørgsmål ---------------------------------------------------------
indikator_df_mikro <- data.frame()  # tom data frame til alt output

kolonner <- c(3, 4, 7, 13, 14, 11)  # 

for (i in seq_len(nrow(tillid_mikro))) {
  # Hent værdier for de valgte kolonner i dette kvartal
  x <- as.numeric(tillid_mikro[i, kolonner, drop = FALSE])
  
  # Gå igennem alle kombinationsstørrelser
  for (k in 1:length(x)) {
    # Lav kombinationer for denne størrelse
    mat <- combn(x, k, simplify = TRUE)
    
    # Beregn gennemsnit for hver kolonne (hver kombination)
    means_k <- colMeans(mat)
    
    # Opret midlertidig data.frame med resultaterne
    df_k <- data.frame(
      kvartal = tillid_mikro$kvartal[i],
      k = k,
      komb_nr = seq_along(means_k),
      mean = means_k
    )
    
    # Tilføj til hoved-dataframe
    indikator_df_mikro <- rbind(indikator_df_mikro, df_k)
  }
}

# 2) Rens kvartal og join datasæt ----------------------------------------------
indikator_df_mikro <- indikator_df_mikro %>%
  mutate(kvartal = gsub(" ", "", kvartal))

samlet_2000_2025 <- samlet %>%
  mutate(Kvartal = gsub(" ", "", Kvartal))

# Sammenflet på kvartal
merged_mikro <- indikator_df_mikro %>%
  inner_join(samlet_2000_2025, by = c("kvartal" = "Kvartal"))

# 3) Beregn R² for hver kombination (komb_nr) ----------------------------------
r2_resultater_mikro <- merged_mikro %>%
  group_by(komb_nr) %>%
  summarise(
    R2 = {
      if (n() > 2 && var(mean, na.rm = TRUE) > 0) {
        summary(lm(real_vaekst_pct ~ mean, data = cur_data()))$r.squared
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  ) %>%
  arrange(desc(R2))

#Komb_r 18 var den stærkeste kombination

# 3) Få ale kombr 18 ud af dataframe ----------------------------------

kombr_18 <- as.data.frame(indikator_df_mikro %>% filter(near(komb_nr,18)))


## 7) Identificere hvilke spørgsmål som er i den kombination --------------------------------------------

kolonner <- c(3, 4, 7, 13, 14, 11)
kolonnenavne <- names(tillid_mikro)[kolonner]

best_komb <- 18  # ud fra din R2-beregning
k_best <- indikator_df_mikro %>% 
  filter(komb_nr == best_komb) %>% 
  pull(k) %>% 
  unique()

bedste_spørgsmål <- combn(kolonnenavne, k_best)[, best_komb]
print(bedste_spørgsmål)

#print(bedste_spørgsmål)
#[1] "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"
#[2] "Regner med at kunne spare op i de kommende 12 måneder"           
#[3] "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr." 


