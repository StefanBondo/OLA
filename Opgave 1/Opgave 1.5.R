#Tabel
glm_tabel <- summary(model_logit)$coefficients

glm_tabel <- data.frame(
  Variabel = rownames(glm_tabel),
  Estimate = glm_tabel[, "Estimate"],
  P_value = glm_tabel[, "Pr(>|z|)"]
)

glm_tabel <- glm_tabel %>%
  mutate(
    Estimate = round(Estimate, 3),
    P_value = round(P_value, 4),
    Signifikans = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01  ~ "**",
      P_value < 0.05  ~ "*",
      TRUE ~ ""
    )
  )



#Graf
library(pROC)

plot(roc(test_data$Mål, pred_logit_prob),
     col = "blue",
     main = "ROC kurve for logistisk regression")