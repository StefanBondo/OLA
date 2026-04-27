# Kør kun install.packages(...) manuelt ved behov.
library(reticulate)
library(tensorflow)
library(plotly)

# Tving R til at bruge den Python, hvor TensorFlow er installeret
use_virtualenv("~/.virtualenvs/r-tensorflow", required = TRUE)

# Henter pixel_matrix, labels, target_width, target_height fra dataforberedelse
#source("Matrixer.R")

set.seed(123)
antal_billeder <- nrow(pixel_matrix)
shuffle_index <- sample(seq_len(antal_billeder))

X <- pixel_matrix[shuffle_index, ]
y <- labels[shuffle_index]

# Gør flad pixel-vektor om til 2D-billede med 1 kanal
X_billeder <- array(0, dim = c(antal_billeder, target_height, target_width, 1))
for (i in seq_len(antal_billeder)) {
  X_billeder[i, , , 1] <- matrix(
    X[i, ],
    nrow = target_height,
    ncol = target_width,
    byrow = TRUE
  )
}

traen_andel <- 0.8
traen_antal <- floor(traen_andel * antal_billeder)

X_train <- X_billeder[1:traen_antal, , , , drop = FALSE]
y_train <- matrix(y[1:traen_antal], ncol = 1)
X_test <- X_billeder[(traen_antal + 1):antal_billeder, , , , drop = FALSE]
y_test <- matrix(y[(traen_antal + 1):antal_billeder], ncol = 1)

tf <- tensorflow::tf

# CNN med convolution + pooling
model <- tf$keras$Sequential(list(
  tf$keras$layers$Input(shape = tuple(as.integer(target_height), as.integer(target_width), 1L)),
  tf$keras$layers$Conv2D(filters = 16L, kernel_size = tuple(3L, 3L), activation = "relu"),
  tf$keras$layers$MaxPooling2D(pool_size = tuple(2L, 2L)),
  tf$keras$layers$Conv2D(filters = 32L, kernel_size = tuple(3L, 3L), activation = "relu"),
  tf$keras$layers$MaxPooling2D(pool_size = tuple(2L, 2L)),
  tf$keras$layers$Flatten(),
  tf$keras$layers$Dense(units = 32L, activation = "relu"),
  tf$keras$layers$Dense(units = 1L, activation = "sigmoid")
))

model$compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = list("accuracy")
)

cat("Traener CNN-model (convolution + pooling)...\n")
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

# Simpel graf over laering
epoch_numre <- seq_along(historik$history$loss)
historik_df <- data.frame(
  epoch = epoch_numre,
  train_accuracy = as.numeric(historik$history$accuracy),
  val_accuracy = as.numeric(historik$history$val_accuracy),
  train_loss = as.numeric(historik$history$loss),
  val_loss = as.numeric(historik$history$val_loss)
)

loss_plot <- plot_ly(historik_df, x = ~epoch) |>
  add_lines(
    y = ~train_loss,
    name = "loss",
    line = list(color = "steelblue"),
    hovertemplate = "Epoch: %{x}<br>loss: %{y:.4f}<extra></extra>"
  ) |>
  add_lines(
    y = ~val_loss,
    name = "val_loss",
    line = list(color = "seagreen"),
    hovertemplate = "Epoch: %{x}<br>val_loss: %{y:.4f}<extra></extra>"
  ) |>
  layout(title = "CNN loss pr. epoch", xaxis = list(title = "Epoch"), yaxis = list(title = "Loss"))

accuracy_plot <- plot_ly(historik_df, x = ~epoch) |>
  add_lines(
    y = ~train_accuracy,
    name = "accuracy",
    line = list(color = "darkorange"),
    hovertemplate = "Epoch: %{x}<br>accuracy: %{y:.4f}<extra></extra>"
  ) |>
  add_lines(
    y = ~val_accuracy,
    name = "val_accuracy",
    line = list(color = "firebrick"),
    hovertemplate = "Epoch: %{x}<br>val_accuracy: %{y:.4f}<extra></extra>"
  ) |>
  layout(title = "CNN accuracy pr. epoch", xaxis = list(title = "Epoch"), yaxis = list(title = "Accuracy"))

subplot(loss_plot, accuracy_plot, nrows = 2, shareX = TRUE, titleY = TRUE)