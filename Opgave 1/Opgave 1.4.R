# --------------------------------------------------
# 7. Logistisk regression
# --------------------------------------------------
model_logit <- glm(
  Mål ~ shot_distance + shot_angle_deg + hovedstød + body_part + Spilsituation,
  data = train_data,
  family = "binomial"
)

pred_logit_prob <- predict(model_logit, newdata = test_data, type = "response")
pred_logit_class <- ifelse(pred_logit_prob > 0.5, 1, 0)

accuracy_logit <- mean(pred_logit_class == test_data$Mål, na.rm = TRUE) * 100
auc_logit <- as.numeric(auc(roc(test_data$Mål, pred_logit_prob)))
brier_logit <- mean((pred_logit_prob - test_data$Mål)^2, na.rm = TRUE)

# --------------------------------------------------
# 8. Decision Tree
# --------------------------------------------------
model_tree <- rpart(
  as.factor(Mål) ~ shot_distance + shot_angle_deg + hovedstød + body_part + Spilsituation,
  data = train_data,
  method = "class"
)

pred_tree_prob <- predict(model_tree, newdata = test_data, type = "prob")[, 2]
pred_tree_class <- ifelse(pred_tree_prob > 0.5, 1, 0)

accuracy_tree <- mean(pred_tree_class == test_data$Mål, na.rm = TRUE) * 100
auc_tree <- as.numeric(auc(roc(test_data$Mål, pred_tree_prob)))
brier_tree <- mean((pred_tree_prob - test_data$Mål)^2, na.rm = TRUE)

# --------------------------------------------------
# 9. Random Forest
# --------------------------------------------------
train_rf <- train_data %>%
  select(Mål, shot_distance, shot_angle_deg, hovedstød, body_part, Spilsituation) %>%
  na.omit()

test_rf <- test_data %>%
  select(Mål, shot_distance, shot_angle_deg, hovedstød, body_part, Spilsituation) %>%
  na.omit()

train_rf <- train_rf %>%
  mutate(
    Mål = as.factor(Mål),
    body_part = as.factor(body_part),
    Spilsituation = as.factor(Spilsituation)
  )

test_rf <- test_rf %>%
  mutate(
    Mål = as.numeric(as.character(Mål)),
    body_part = as.factor(body_part),
    Spilsituation = as.factor(Spilsituation)
  )

model_rf <- randomForest(
  Mål ~ shot_distance + shot_angle_deg + hovedstød + body_part + Spilsituation,
  data = train_rf
)

pred_rf_prob <- predict(model_rf, newdata = test_rf, type = "prob")[, 2]
pred_rf_class <- ifelse(pred_rf_prob > 0.5, 1, 0)

accuracy_rf <- mean(pred_rf_class == test_rf$Mål, na.rm = TRUE) * 100
auc_rf <- as.numeric(auc(roc(test_rf$Mål, pred_rf_prob)))
brier_rf <- mean((pred_rf_prob - test_rf$Mål)^2, na.rm = TRUE)

# --------------------------------------------------
# 10. Saml resultater
# --------------------------------------------------
results <- data.frame(
  Model = c("Logistisk regression", "Decision Tree", "Random Forest"),
  Accuracy = c(accuracy_logit, accuracy_tree, accuracy_rf),
  AUC = c(auc_logit, auc_tree, auc_rf),
  Brier = c(brier_logit, brier_tree, brier_rf)
)

results <- results %>%
  mutate(
    Accuracy = round(Accuracy, 2),
    AUC = round(AUC, 3),
    Brier = round(Brier, 3)
  )

results
