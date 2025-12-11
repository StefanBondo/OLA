
library(caret)


#-------------------------------------------
  #🎯 SCENARIE 1 – 4 variabler, threshold = 0.6
#--------------------------------------------

data_logit_clean <- data_logit %>% 
  filter(!is.na(stigning_dummy))

model_s1 <- glm(
  stigning_dummy ~ 
    DI_FTI +
    ` Familiens økonomiske situation i dag  sammenlignet med for et år siden` +
    ` Danmarks økonomiske situation i dag  sammenlignet med for et år siden` +
    ` Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket` +
    ` Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `,
  data = data_logit_clean,
  family = binomial
)

data_logit_clean$pred_s1 <- ifelse(
  predict(model_s1, type = "response") > 0.6, 1, 0
)

  
#3. Confusion matrix
cm_s1 <- confusionMatrix(
    factor(data_logit_clean$pred_s1),
    factor(data_logit_clean$stigning_dummy)
  )

cm_s1



#--------------------------------------------
 # 🎯 SCENARIE 2 – 2 variabler, threshold = 0.5
#-------------------------------------------
#  1. Model med 2 spørgsmål
model_s2 <- glm(
  stigning_dummy ~ 
    ` Danmarks økonomiske situation i dag  sammenlignet med for et år siden` +
    ` Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `,
  data = data_logit_clean,
  family = binomial
)

#2. Predict alle kvartaler
data_logit_clean$pred_s2 <- ifelse(
  predict(model_s2, type = "response") > 0.5, 1, 0
)

#3. Confusion matrix
cm_s2 <- confusionMatrix(
  factor(data_logit_clean$pred_s2),
  factor(data_logit_clean$stigning_dummy)
)
cm_s2
