#Opgave 3.2
#Vi laver join så vi får NKN1 og DI sammen
data_logit <- NKN1 %>%
  left_join(FORV1_kvartal_DI, by = "Kvartal") %>%
  select(Kvartal, stigning_dummy, 
         DI_FTI, 
         ` Familiens økonomiske situation i dag  sammenlignet med for et år siden`,
         ` Danmarks økonomiske situation i dag  sammenlignet med for et år siden`,
         ` Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket`,
         ` Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `)

Forudsigelse2025Q4 <- glm(
  stigning_dummy ~ 
    DI_FTI +
    ` Familiens økonomiske situation i dag  sammenlignet med for et år siden` +
    ` Danmarks økonomiske situation i dag  sammenlignet med for et år siden` +
    ` Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket` +
    ` Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `,
  data = data_logit,
  family = binomial
)


row_Q3_2025 <- data_logit %>%
  filter(Kvartal == "2025Q3")

row_Q3_2025_pred <- row_Q3_2025 %>%
  select(-stigning_dummy)

p_Q4 <- predict(Forudsigelse2025Q4, newdata = row_Q3_2025_pred, type = "response")
p_Q4
#> p_Q4
#1 
#0.3298567 

if (p_Q4 > 0.5) "OP" else "NED"
#> if (p_Q4 > 0.5) "OP" else "NED"
#[1] "NED"


