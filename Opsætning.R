# Kør kun install.packages(...) manuelt ved behov.
library(imager)

base_dir <- "Billeder"
med_objekt_dir <- file.path(base_dir, "Med objekt(TRUE)")
uden_objekt_dir <- file.path(base_dir, "Uden objekt(False)")

img_pattern <- "\\.(jpg|jpeg|png|bmp|tif|tiff)$"

med_objekt_filer <- list.files(
  med_objekt_dir,
  pattern = img_pattern,
  full.names = TRUE,
  ignore.case = TRUE
)

uden_objekt_filer <- list.files(
  uden_objekt_dir,
  pattern = img_pattern,
  full.names = TRUE,
  ignore.case = TRUE
)

# --------------------------
# Trin 1.5: RGB-måling på ét billede
# --------------------------
rgb_billede <- file.path(med_objekt_dir, "IMG_8844.jpg")
img_rgb <- load.image(rgb_billede)

img_dims <- dim(img_rgb)
img_w <- img_dims[1]
img_h <- img_dims[2]



# Labels til klassifikation:
# TRUE  = objekt findes på billedet
# FALSE = objekt findes ikke på billedet
billede_labels <- data.frame(
  filsti = c(med_objekt_filer, uden_objekt_filer),
  objekt = c(
    rep(TRUE, length(med_objekt_filer)),
    rep(FALSE, length(uden_objekt_filer))
  ),
  stringsAsFactors = FALSE
)


# --------------------------
# Trin 2: Træn en første model
# --------------------------
target_width <- 16
target_height <- 16

udtraek_features <- function(filsti, target_width, target_height) {
  img <- load.image(filsti)
  img_small <- resize(img, size_x = target_width, size_y = target_height)

  r_small <- as.numeric(R(img_small))
  g_small <- as.numeric(G(img_small))
  b_small <- as.numeric(B(img_small))

  img_gray <- grayscale(img_small)
  grad <- imgradient(img_gray, "xy")
  grad_mag <- sqrt((grad$x)^2 + (grad$y)^2)

  gray_small <- as.numeric(img_gray)
  grad_small <- as.numeric(grad_mag)

  # Farve- og tekstur-statistik giver modellen robuste signaler
  statistik <- c(
    mean(r_small), sd(r_small),
    mean(g_small), sd(g_small),
    mean(b_small), sd(b_small),
    mean(gray_small), sd(gray_small),
    mean(grad_small), sd(grad_small)
  )

  c(r_small, g_small, b_small, gray_small, grad_small, statistik)
}

cat("Udtrækker features fra billeder...\n")
feature_liste <- lapply(
  billede_labels$filsti,
  udtraek_features,
  target_width = target_width,
  target_height = target_height
)

X <- do.call(rbind, feature_liste)
y <- as.integer(billede_labels$objekt) # TRUE -> 1, FALSE -> 0

alle_data <- as.data.frame(X)
colnames(alle_data) <- paste0("f_", seq_len(ncol(alle_data)))
alle_data$objekt <- y

# Stratificeret split så begge klasser er med i train/test
set.seed(42)
test_andel <- 0.2

idx_pos <- which(alle_data$objekt == 1)
idx_neg <- which(alle_data$objekt == 0)

antal_pos_test <- max(1, floor(length(idx_pos) * test_andel))
antal_neg_test <- max(1, floor(length(idx_neg) * test_andel))

test_idx <- c(
  sample(idx_pos, size = antal_pos_test),
  sample(idx_neg, size = antal_neg_test)
)
train_idx <- setdiff(seq_len(nrow(alle_data)), test_idx)

traenings_data <- alle_data[train_idx, , drop = FALSE]
test_data <- alle_data[test_idx, , drop = FALSE]

model <- glm(objekt ~ ., data = traenings_data, family = binomial())

# --------------------------
# Trin 3: Test af model
# --------------------------

test_prob <- predict(model, newdata = test_data, type = "response")
test_pred <- ifelse(test_prob >= 0.5, 1L, 0L)
test_true <- test_data$objekt

accuracy <- mean(test_pred == test_true)
cat("Test accuracy:", round(accuracy, 4), "\n")

conf_mat <- table(
  Predicted = factor(test_pred, levels = c(0, 1)),
  Actual = factor(test_true, levels = c(0, 1))
)
print(conf_mat)

