# Afgræns periode
DST <- FORV1_kvartal_DST[
  FORV1_kvartal_DST$Kvartal >= "2000Q1" &
    FORV1_kvartal_DST$Kvartal <= "2025Q3",
]

spm <- DST[, 3:14]   # 12 spørgsmål

#vend fortegn
# spg 6 "Priser i dag  sammenlignet med for et år siden" 
# spg 7 ""Priser om et år  sammenlignet med i dag"         
# spg 8 "Arbejdsløsheden om et år  sammenlignet med i dag"   
# spg 10 ""Anser det som fornuftigt at spare op i den nuværende økonomiske situation"

library(tidyverse)
spm <- spm |>
  mutate(across(c(6, 7, 8, 10), ~ -.x))


comliste2 <- list()
indikatorliste <- list()

for (i in seq_len(nrow(spm))) {
  
  # værdier for ét kvartal
  x <- as.numeric(spm[i, ])
  
  # alle kombinationer (k = 1..12)
  combs_all <- lapply(1:length(x), function(k) combn(x, k))
  
  # gennemsnit for hver kombination
  means_all <- lapply(combs_all, function(mat) colMeans(mat))
  
  # gem
  comliste2[[i]] <- combs_all
  indikatorliste[[i]] <- means_all
}

# navngiv efter kvartal
names(comliste2) <- DST$Kvartal
names(indikatorliste) <- DST$Kvartal

indikator_df <- data.frame()

for (i in seq_len(nrow(spm))) {
  
  x <- as.numeric(spm[i, ])
  
  for (k in 1:length(x)) {
    
    mat <- combn(x, k)
    means_k <- colMeans(mat)
    
    df_k <- data.frame(
      Kvartal = DST$Kvartal[i],
      k = k,
      komb_nr = seq_along(means_k),
      mean = means_k
    )
    
    indikator_df <- rbind(indikator_df, df_k)
  }
}

indikator_df$mean <- round(indikator_df$mean, 2)

merged_df <- indikator_df %>%
  inner_join(
    plot_df %>% select(Kvartal, real_vækst_pct),
    by = "Kvartal"
  )

r2_fun <- function(df) {
  if (nrow(df) < 3 || var(df$mean, na.rm = TRUE) == 0) return(NA_real_)
  summary(lm(real_vækst_pct ~ mean, data = df))$r.squared
}


library(dplyr)
library(purrr)
library(tibble)

# Indeks for de 12 spørgsmål
spm_idx   <- 3:14
spm_navne <- colnames(DST)[spm_idx]

# Opslagskatalog: k, komb_nr → hvilke spørgsmål
combo_labels <- map2_dfr(
  .x = lapply(1:length(spm_idx), function(k) combn(spm_idx, k, simplify = FALSE)),
  .y = 1:length(spm_idx),
  .f = function(lst, k) {
    tibble(
      k = k,
      komb_nr = seq_along(lst),
      col_idx = lst
    )
  }
) %>%
  mutate(
    spm_navne = map(col_idx, ~ spm_navne[match(.x, spm_idx)]),
    label = map_chr(spm_navne, ~ paste(.x, collapse = " + "))
  )


r2_resultater <- merged_df %>%
  group_by(k, komb_nr) %>%
  summarise(R2 = r2_fun(cur_data()), .groups = "drop") %>%
  inner_join(combo_labels, by = c("k", "komb_nr")) %>%
  arrange(desc(R2))


vinder <- r2_resultater %>% slice(5)

vinder$k
vinder$komb_nr
vinder$spm_navne[[1]]


#Plot

k_vinder <- vinder$k        # 5
j_vinder <- vinder$komb_nr  # 630

VORES <- numeric(nrow(DST))

for (i in 1:nrow(DST)) {
  VORES[i] <- mean(
    comliste2[[i]][[k_vinder]][, j_vinder]
  )
}



spm_valgt <- c(
  "Danmarks økonomiske situation i dag  sammenlignet med for et år siden",
  "Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket",
  "Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr ",
  "Regner med at kunne spare op i de kommende 12 måneder",
  "Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener"
)

plot_df$VORES <- rowMeans(
  DST[, spm_valgt],
  na.rm = TRUE
)

plot_df$VORES_smooth <- stats::filter(
  plot_df$VORES,
  rep(1/4, 4),
  sides = 2
)


scale_factor <- 3

ggplot(plot_df, aes(x = Kvartal)) +
  
  # SØJLER: årlig realvækst
  geom_col(
    aes(
      y = real_vækst_pct * scale_factor,
      fill = "Årlig realvækst (pct.)"
    )
  ) +
  
  # DI
  geom_line(
    aes(y = DI, colour = "DI's indikator", group = 1),
    linewidth = 0.6
  ) +
  
  # VORES (ny vinder, glattet)
  geom_line(
    aes(y = VORES, colour = "Vores indikator", group = 1),
    linewidth = 0.6,
    na.rm = TRUE
  ) +
  scale_y_continuous(
    name = "Forbrugertillidsindikator (nettotal)",
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = "Årlig realvækst (pct.)"
    )
  ) +
  scale_x_discrete(
    breaks = plot_df$Kvartal[grepl("Q1", plot_df$Kvartal)],
    labels = substr(plot_df$Kvartal[grepl("Q1", plot_df$Kvartal)], 3, 4)
  ) +
  
  scale_fill_manual(
    values = "skyblue",
    name = NULL
  ) +
  scale_color_manual(
    values = c(
      "DI's indikator"  = "grey30",
      "Vores indikator" = "limegreen"
    ),
    name = NULL
  ) +
  
  labs(
    x = NULL,
    title = "DI's indikator og vores bedste indikator ift. realvækst i privatforbrug",
    caption = "Kilde: Danmarks Statistik"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0) 
  )









