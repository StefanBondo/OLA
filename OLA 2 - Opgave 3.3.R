#Baslinemodel
library(lattice)

#Opgave 3.3
data_logit$predicted_prob <- predict(Forudsigelse2025Q4, 
                                     newdata = data_logit, 
                                     type = "response")

data_logit$predicted_dummy <- ifelse(data_logit$predicted_prob > 0.5, 1, 0)

confusionMatrix(
  factor(data_logit$predicted_dummy),
  factor(data_logit$stigning_dummy)
)

#Den logistiske model forudsiger, at den årlige reale vækst i husholdningernes forbrugsudgift stiger i ca. 96% af kvartalerne.
#Når modellen forudsiger en stigning, er det faktisk tilfældet i ca. 81% af observationerne.#

#Confusion Matrix and Statistics


Reference
Prediction  0  1
0 13  4
1 12 74

Accuracy : 0.8447        
95% CI : (0.76, 0.9085)
No Information Rate : 0.7573        
P-Value [Acc > NIR] : 0.02156       

Kappa : 0.5259        

Mcnemar's Test P-Value : 0.08012       
                                        
            Sensitivity : 0.5200        
            Specificity : 0.9487        
         Pos Pred Value : 0.7647        
         Neg Pred Value : 0.8605        
             Prevalence : 0.2427        
         Detection Rate : 0.1262        
   Detection Prevalence : 0.1650        
      Balanced Accuracy : 0.7344        
                                        
       'Positive' Class : 0                
                               