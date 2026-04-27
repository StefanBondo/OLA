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

# Clustering pa kun et billede (pixel-segmentering)
valgt_billede <- med_objekt_filer[1]

img_single <- load.image(valgt_billede)

# Brug original størrelse for mindre blur.
# Saet brug_resize_til <- TRUE hvis du vil tvinge en fast størrelse.
brug_resize_til <- FALSE
single_width <- 128
single_height <- 128

if (brug_resize_til) {
  img_arbejde <- resize(img_single, size_x = single_width, size_y = single_height)
} else {
  img_arbejde <- img_single
  single_width <- dim(img_single)[1]
  single_height <- dim(img_single)[2]
}

img_single_gray <- grayscale(img_arbejde)
img_single_gray_lodret <- imrotate(img_single_gray, 90)

pixel_single <- as.numeric(img_single_gray)
pixel_single_df <- data.frame(intensitet = pixel_single)

set.seed(123)
kmeans_single <- kmeans(pixel_single_df, centers = 2, nstart = 25)

cluster_single_matrix <- matrix(
  kmeans_single$cluster,
  nrow = single_height,
  ncol = single_width,
  byrow = TRUE
)
cluster_single_img <- as.cimg(cluster_single_matrix)

# Vi antager her at objektet er den moerkeste af de to clusters.
cluster_middel <- tapply(pixel_single, kmeans_single$cluster, mean)
objekt_cluster <- as.integer(names(which.min(cluster_middel)))
objekt_maske <- ifelse(cluster_single_matrix == objekt_cluster, 1, 0)
objekt_maske_img <- as.cimg(objekt_maske)

cat("\nEnkelt-billede clustering koert pa:\n")
cat(basename(valgt_billede), "\n")
cat("Pixel-antal:", length(pixel_single), "\n")

plot(img_single_gray_lodret, main = "Valgt billede (steelblue, lodret)")
