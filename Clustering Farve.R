# Kør kun install.packages(...) manuelt ved behov.
library(imager)

base_dir <- "Billeder"
med_objekt_dir <- file.path(base_dir, "Med objekt(TRUE)")
img_pattern <- "\\.(jpg|jpeg|png|bmp|tif|tiff)$"

med_objekt_filer <- list.files(
  med_objekt_dir,
  pattern = img_pattern,
  full.names = TRUE,
  ignore.case = TRUE
)

# Vi bruger ét billede som eksempel.
valgt_billede <- med_objekt_filer[1]
img <- load.image(valgt_billede)
img_gray <- grayscale(img)

width <- dim(img_gray)[1]
height <- dim(img_gray)[2]

pixel_values <- as.numeric(img_gray)
pixel_df <- data.frame(intensitet = pixel_values)

set.seed(123)
kmeans_resultat <- kmeans(pixel_df, centers = 2, nstart = 25)

cluster_matrix <- matrix(
  kmeans_resultat$cluster,
  nrow = height,
  ncol = width,
  byrow = TRUE
)

# Antag at flasken er mørkere end baggrunden.
cluster_middel <- tapply(pixel_values, kmeans_resultat$cluster, mean)
flaske_cluster <- as.integer(names(which.min(cluster_middel)))
flaske_maske <- cluster_matrix == flaske_cluster

# Lav et farvelagt billede:
# Baggrund = rød, flaske = blå.
r_mat <- ifelse(flaske_maske, 0, 1)
g_mat <- ifelse(flaske_maske, 0, 0)
b_mat <- ifelse(flaske_maske, 1, 0)

r_img <- as.cimg(r_mat)
g_img <- as.cimg(g_mat)
b_img <- as.cimg(b_mat)

farve_billede <- imappend(list(r_img, g_img, b_img), "c")

cat("Farvelagt eksempel kørt på:\n")
cat(basename(valgt_billede), "\n")

plot(farve_billede, main = "Eksempel: baggrund rød, flaske blå")
