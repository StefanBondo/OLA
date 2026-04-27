# Kør kun install.packages(...) manuelt ved behov.
library(imager)
library(ggplot2)

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

alle_filer <- c(med_objekt_filer, uden_objekt_filer)
labels <- c(rep(1, length(med_objekt_filer)), rep(0, length(uden_objekt_filer)))

target_width <- 16
target_height <- 16

lav_pixel_vektor <- function(filsti, target_width, target_height) {
  img <- load.image(filsti)
  img_small <- resize(img, size_x = target_width, size_y = target_height)
  img_gray <- grayscale(img_small)

  as.numeric(img_gray)
}

cat("Indlaeser billeder og bygger matrix...\n")
pixel_liste <- lapply(
  alle_filer,
  lav_pixel_vektor,
  target_width = target_width,
  target_height = target_height
)

# Raekker = billeder, kolonner = pixels
pixel_matrix <- do.call(rbind, pixel_liste)

kolonne_gennemsnit <- colMeans(pixel_matrix)

cat("Antal billeder (observationer):", nrow(pixel_matrix), "\n")
cat("Antal kolonner (pixels):", ncol(pixel_matrix), "\n")


# Valgfrit: saml resultat i data frame med billednavn
pixel_df <- as.data.frame(pixel_matrix)
colnames(pixel_df) <- paste0("pixel_", seq_len(ncol(pixel_df)))
pixel_df <- data.frame(
  filnavn = basename(alle_filer),
  label = labels,
  billede_gennemsnit = rowMeans(pixel_matrix),
  pixel_df,
  stringsAsFactors = FALSE
)

# Gennemsnit for hver af de 16 vandrette raekker i billedet
for (raekke_nr in seq_len(target_height)) {
  start_kolonne <- ((raekke_nr - 1) * target_width) + 1
  slut_kolonne <- raekke_nr * target_width
  kolonnenavn <- paste0("raekke_", raekke_nr, "_gennemsnit")
  pixel_df[[kolonnenavn]] <- rowMeans(pixel_matrix[, start_kolonne:slut_kolonne])
}

# Flyt raekke-gennemsnit ind mellem pixel-blokke (16 og 16)
ny_kolonne_raekkefoelge <- c("filnavn", "label", "billede_gennemsnit")

for (raekke_nr in seq_len(target_height)) {
  start_kolonne <- ((raekke_nr - 1) * target_width) + 1
  slut_kolonne <- raekke_nr * target_width
  pixel_kolonner <- paste0("pixel_", start_kolonne:slut_kolonne)
  raekke_kolonne <- paste0("raekke_", raekke_nr, "_gennemsnit")
  ny_kolonne_raekkefoelge <- c(ny_kolonne_raekkefoelge, pixel_kolonner, raekke_kolonne)
}

pixel_df <- pixel_df[, ny_kolonne_raekkefoelge]

# Barplot af gennemsnit for hvert billede
plot_data <- pixel_df[, c("filnavn", "billede_gennemsnit")]
plot_data$filnavn <- factor(plot_data$filnavn, levels = plot_data$filnavn)

gennemsnit_plot <- ggplot(plot_data, aes(x = filnavn, y = billede_gennemsnit)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Gennemsnitlig pixelvaerdi pr. billede",
    x = "Filnavn",
    y = "Billede-gennemsnit (0-1)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

plot(gennemsnit_plot)

