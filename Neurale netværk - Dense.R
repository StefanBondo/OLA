# Kør kun install.packages(...) manuelt ved behov.
library(reticulate)
library(ggplot2)
library(plotly)

# Tving R til at bruge den Python, hvor TensorFlow er installeret
use_virtualenv("~/.virtualenvs/r-tensorflow", required = TRUE)

library(tensorflow)

# Henter pixel_matrix og labels fra dataforberedelse
#source("Matrixer.R")

set.seed(123)
antal_billeder <- nrow(pixel_matrix)
shuffle_index <- sample(seq_len(antal_billeder))

X <- pixel_matrix[shuffle_index, ]
y <- labels[shuffle_index]

traen_andel <- 0.8
traen_antal <- floor(traen_andel * antal_billeder)

X_train <- X[1:traen_antal, ]
y_train <- matrix(y[1:traen_antal], ncol = 1)
X_test <- X[(traen_antal + 1):antal_billeder, ]
y_test <- matrix(y[(traen_antal + 1):antal_billeder], ncol = 1)

tf <- tensorflow::tf

model <- tf$keras$Sequential(list(
  tf$keras$layers$Input(shape = tuple(as.integer(ncol(X_train)))),
  tf$keras$layers$Dense(units = 32L, activation = "relu"),
  tf$keras$layers$Dense(units = 16L, activation = "relu"),
  tf$keras$layers$Dense(units = 1L, activation = "sigmoid")
))

model$compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = list("accuracy")
)

cat("Traener TensorFlow-model...\n")
historik <- model$fit(
  X_train,
  y_train,
  epochs = 20L,
  batch_size = 8L,
  validation_split = 0.2,
  verbose = 1L
)

eval_resultat <- model$evaluate(X_test, y_test, verbose = 0L)
eval_tal <- as.numeric(eval_resultat)
cat("Test loss:", round(eval_tal[1], 4), "\n")
cat("Test accuracy:", round(eval_tal[2], 4), "\n")

# Graf 1: udvikling i accuracy og loss under traening
epoch_numre <- seq_along(historik$history$loss)
historik_df <- data.frame(
  epoch = epoch_numre,
  train_accuracy = as.numeric(historik$history$accuracy),
  val_accuracy = as.numeric(historik$history$val_accuracy),
  train_loss = as.numeric(historik$history$loss),
  val_loss = as.numeric(historik$history$val_loss)
)

# Interaktiv figur i samme stil som din vens: loss oeverst og accuracy nederst
p_loss <- plot_ly(historik_df, x = ~epoch) |>
  add_lines(
    y = ~train_loss,
    name = "loss",
    line = list(color = "#4EA3FF"),
    marker = list(color = "#4EA3FF"),
    hovertemplate = "Epoch: %{x}<br>loss: %{y:.4f}<extra></extra>"
  ) |>
  add_lines(
    y = ~val_loss,
    name = "val_loss",
    line = list(color = "#67C56B"),
    marker = list(color = "#67C56B"),
    hovertemplate = "Epoch: %{x}<br>val_loss: %{y:.4f}<extra></extra>"
  ) |>
  layout(
    title = list(text = "Loss", font = list(color = "white")),
    xaxis = list(title = "Epoch", color = "white"),
    yaxis = list(title = "Loss", color = "white"),
    plot_bgcolor = "#1E1E1E",
    paper_bgcolor = "#1E1E1E",
    legend = list(font = list(color = "white"))
  )

p_accuracy <- plot_ly(historik_df, x = ~epoch) |>
  add_lines(
    y = ~train_accuracy,
    name = "accuracy",
    line = list(color = "#4EA3FF"),
    marker = list(color = "#4EA3FF"),
    hovertemplate = "Epoch: %{x}<br>accuracy: %{y:.4f}<extra></extra>"
  ) |>
  add_lines(
    y = ~val_accuracy,
    name = "val_accuracy",
    line = list(color = "#67C56B"),
    marker = list(color = "#67C56B"),
    hovertemplate = "Epoch: %{x}<br>val_accuracy: %{y:.4f}<extra></extra>"
  ) |>
  layout(
    title = list(text = "Accuracy - Dense", font = list(color = "white")),
    xaxis = list(title = "Epoch", color = "white"),
    yaxis = list(title = "Accuracy", color = "white"),
    plot_bgcolor = "#1E1E1E",
    paper_bgcolor = "#1E1E1E",
    legend = list(font = list(color = "white"))
  )

subplot(p_loss, p_accuracy, nrows = 2, shareX = TRUE, titleY = TRUE)
