#Opgave 1.4 – Forudsigelser med afsæt i jeres indikatorer 
#Forudsig udviklingen i husholdningernes forbrugsudgift i 3. og 4. kvartal 2024. 

#Forudsigelse af lineær regression herunder estimate og error
lm_mikroQ3 <- lm(forbrug_mikro$real_vaekst_pct ~
                   tillid_mikro$Forbrugertillidsindikatoren,
                 data = tillid_mikro)

summary(lm_mikroQ3)

mikroQ3 <- c(tillid_mikro$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
               tillid_mikro$`Familiens økonomiske  situation om et år, sammenlignet med i dag`+
               tillid_mikro$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`+
               tillid_mikro$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`+
               tillid_mikro$`Regner med at kunne spare op i de kommende 12 måneder`+
               tillid_mikro$`Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener`)/6
mikroq3same <- sum(mikroQ3)/1

mikroq3pfv <- 0.18523 + 0.02582*mikroq3same

print(mikroq3pfv)

#---------------------------------------------------------------------------

#ved hjælp af predict funktion
# 1. Lav den logisitiske model hvor vi skal beskirve realvækst udefra gennemsnit
vækstQ3glm <- glm(real_vaekst_pct ~ gennemsnit,
  data = mikrosamlet,
  family = gaussian()
)

# 2.  Beregn gennemsnit 
mikrosamlet <- mikrosamlet %>%
  rowwise() %>%
  mutate(gennemsnit = mean(c_across(c(3,4,7,13,14,11)), na.rm = TRUE))

mikrosamlet$gennemsnit <- rowMeans(mikrosamlet[,c(3,4,7,13,14,11)], na.rm = TRUE)

mikrosamlet$gennemsnit <- round(mikrosamlet$gennemsnit, 2)


# 3. Lav et nyt lille data frame med samme variabelnavn herunder gennemsnit af de relevante mikro spørgsmål. For at kunne forudsige Q4 så brugte vi 2024Q4 som udgangspunkt
Gennemsnit2025Q2:1.86
Gennensnit2024Q4:3.36 
minif <- data.frame(gennemsnit = c(1.86, 3.36))


# 4. Benyt predict funktion til forudsige de to kvartaler
predict_mikroQ3 <- predict(vækstQ3glm, newdata = minif, type = "response")

# 5. Se resultatet
print(predict_mikroQ3)

#print(predict_mikroQ3)
#1          2 
#-1.0900100 -0.5872307 

#2025Q3: -1,09
#2025Q4: -0,58



