library(dplyr)

# Træk år ud
FORV1_kvartal_DST$År <- substr(FORV1_kvartal_DST$Kvartal, 1, 4)

# Beregn gennemsnit pr år
dst_år <- FORV1_kvartal_DST %>%
  group_by(År) %>%
  summarise(Forbrugertillid = mean(Forbrugertillidsindikatoren, na.rm = TRUE))

# Plot
ggplot(dst_år, aes(x = År, y = Forbrugertillid, group = 1)) +
  geom_line(color = "blue") +
  labs(title = "Forbrugertillid i Danmark (år)",
       x = "År",
       caption = "Kilde: DST.DK",
       y = "Forbrugertillidsindikatoren") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
