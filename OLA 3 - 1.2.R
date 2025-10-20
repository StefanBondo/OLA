#Opave 1.2 R2 og forbrugertillidsindikatorer

## 1) Indlæs data ------------------------------------------------------------
indikator <- read_excel("indikator.xlsx")          # kolonner: kvartal, k, komb_nr, mean
samlet    <- read_excel("samlet_2000_2025.xlsx")   # kolonner: Kvartal, real_vaekst_pct, mv.

## 2) Rens kvartal-felt (fjern mellemrum) og join ---------------------------
indikator <- indikator %>%
  mutate(kvartal = gsub(" ", "", kvartal))

samlet <- samlet %>%
  mutate(Kvartal = gsub(" ", "", Kvartal))

merged <- indikator %>%
  inner_join(samlet, by = c("kvartal" = "Kvartal"))

# Tjek at de centrale kolonner findes
stopifnot(all(c("komb_nr", "mean", "real_vaekst_pct") %in% names(merged)))

## 3) Funktion til R2 pr. gruppe --------------------------------------------
r2_fun <- function(df) {
  # Beskyt mod for få punkter eller konstant varians
  if (nrow(df) < 3 || var(df$mean, na.rm = TRUE) == 0) return(NA_real_)
  summary(lm(real_vaekst_pct ~ mean, data = df))$r.squared
}

## 4) Få R2 resultater i dataframe --------------------------------------------
r2_resultater <- merged %>%
  group_by(komb_nr) %>%
  summarise(R2 = r2_fun(cur_data()), .groups = "drop") %>%
  arrange(desc(R2))


## 5) Få ale kombr 811 ud af dataframe --------------------------------------------
kombr_811 <- as.data.frame(indikator %>% filter(near(komb_nr,811)))


## 6) Identificere hvilke spørgsmål som er i den kombination --------------------------------------------
kolonner <- c(3:14)
kolonnenavne <- names(tillid_mikro)[kolonner]

best_komb <- 811 
k_best <- indikator_df %>% 
  filter(komb_nr == best_komb) %>% 
  pull(k) %>% 
  unique()

bedste_spørgsmål <- combn(kolonnenavne, k_best)[, best_komb]
print(bedste_spørgsmål)

#> print(bedste_spørgsmål)
#[1] "Danmarks økonomiske situation i dag, sammenlignet med for et år siden"    
#[2] "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket"         
#[3] "Priser om et år, sammenlignet med i dag"                                  
#[4] "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."          
#[5] "Anser det som fornuftigt at spare op i den nuværende økonomiske situation"
#[6] "Regner med at kunne spare op i de kommende 12 måneder"    
