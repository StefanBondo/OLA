
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
