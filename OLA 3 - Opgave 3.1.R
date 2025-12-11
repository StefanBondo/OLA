library(tidyverse)

FORV1_kvartal_DI_slice <- FORV1_kvartal_DI %>% slice(-120)
FORV1_kvartal_DST_slice <- FORV1_kvartal_DST %>% slice(-120)


NKN1_jul <- NKN1 %>%
  mutate(
    stigning_dummy = as.integer(real_vækst_pct > 0)
  )

#---------------------------------------------
# 1. Lav datasæt til GLM (DST + NKN1)
#---------------------------------------------
data_logit_DST <- NKN1_jul %>%
  left_join(FORV1_kvartal_DST_slice, by = "Kvartal") %>%
  select(
    Kvartal,
    stigning_dummy,
    Forbrugertillid,
    ` Familiens økonomiske situation i dag  sammenlignet med for et år siden`,
    ` Danmarks økonomiske situation i dag  sammenlignet med for et år siden`,
    ` Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket`,
    ` Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `
  )

#---------------------------------------------
# 2. GLM model (Machine Learning)
#---------------------------------------------
Model_DST <- glm(
  stigning_dummy ~ 
    Forbrugertillid,
  data = data_logit_DST,
  family = binomial
)

summary(Model_DST)

#---------------------------------------------
# 3. Hent 2025Q3 som input til prediction
#---------------------------------------------
row_Q3_2025_DST <- data_logit_DST %>%
  filter(Kvartal == "2025Q3")

row_Q3_2025_pred_DST <- row_Q3_2025_DST %>%
  select(-stigning_dummy)

#---------------------------------------------
# 4. Forudsig om julehandlen Q4 2025 stiger
#---------------------------------------------
p_Q4_DST <- predict(
  Model_DST,
  newdata = row_Q3_2025_pred_DST,
  type = "response"
)

p_Q4_DST
#0.2781394 

if (p_Q4_DST > 0.5) "OP" else "NED"
#[1] "NED"


#3.2 validering af model


library(lattice)

DST_pred_prob <- predict(
  Model_DST,
  newdata = data_logit_DST,
  type = "response"
)

DST_pred_class <- ifelse(DST_pred_prob > 0.5, 1, 0)

confusionMatrix(
  factor(DST_pred_class),
  factor(data_logit_DST$stigning_dummy),
  positive = "1"
)

#Confusion Matrix and Statistics

Reference
Prediction  0  1
0  9  5
1 19 82

Accuracy : 0.7913          
95% CI : (0.7056, 0.8615)
No Information Rate : 0.7565          
P-Value [Acc > NIR] : 0.225904        

Kappa : 0.3178          

Mcnemar's Test P-Value : 0.007963        
                                          
            Sensitivity : 0.9425          
            Specificity : 0.3214          
         Pos Pred Value : 0.8119          
         Neg Pred Value : 0.6429          
             Prevalence : 0.7565          
         Detection Rate : 0.7130          
   Detection Prevalence : 0.8783          
      Balanced Accuracy : 0.6320          
                                          
       'Positive' Class : 1    
