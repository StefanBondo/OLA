library(ggplot2)

best_R2 <- -Inf
best_series <- NULL
best_name <- NULL

for (q in names(comliste2)) {
  for (k_name in names(comliste2[[q]])) {
    
    mat <- comliste2[[q]][[k_name]]
    
    # spring over hvis det ikke er en matrix
    if (!is.matrix(mat)) next
    
    # gennemsnit af kombinationer
    series <- colMeans(mat)
    
    # længden skal passe
    if (length(series) != nrow(plot_df)) next
    
    mod <- lm(plot_df$real_vækst_pct ~ series)
    R2 <- summary(mod)$r.squared
    
    if (R2 > best_R2) {
      best_R2 <- R2
      best_series <- series
      best_name <- k_name
    }
  }
}

plot_df$VORES <- best_series

scale_factor <- 3

ggplot(plot_df, aes(x = Kvartal)) +
  
  # SØJLER: årlig realvækst
  geom_col(
    aes(
      y = real_vækst_pct * scale_factor,
      fill = "Årlig realvækst (pct.)"
    )
  ) +
  
  # LINJE: DI
  geom_line(
    aes(y = DI, colour = "DI's indikator", group = 1),
    linewidth = 0.6
  ) +
  
  # LINJE: VORES
  geom_line(
    aes(y = VORES, colour = "Vores indikator", group = 1),
    linewidth = 0.6
  ) +
  
  # Y-AKSER
  scale_y_continuous(
    name = "Forbrugertillidsindikator (nettotal)",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Årlig realvækst (pct.)"
    )
  ) +
  
  # X-AKSE: ét label pr. år (Q1)
  scale_x_discrete(
    breaks = plot_df$Kvartal[grepl("Q1", plot_df$Kvartal)],
    labels = substr(plot_df$Kvartal[grepl("Q1", plot_df$Kvartal)], 3, 4)
  ) +
  
  # FARVER
  scale_fill_manual(
    values = "skyblue",
    name = NULL
  ) +
  scale_color_manual(
    values = c(
      "DI's indikator" = "grey30",
      "Vores indikator" = "limegreen"
    ),
    name = NULL
  ) +
  
  # TEKST
  labs(
    x = NULL,
    title = "DI's indikator og vores indikator ift. realvækst i privatforbrug",
    caption = "Kilde: Danmarks Statistik"
  ) +
  
  # TEMA
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0)
  )


spm_names <- colnames(spm)

k_valg <- "k = 2"
j <- 1

spm_i_komb <- combn(spm_names, 2)[, j]
spm_i_komb



