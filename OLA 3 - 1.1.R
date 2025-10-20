#Opgave 1.1 – Kombinationsalgoritme i R
com2 <- combn(tillid_mikro[1,3:ncol(tillid_mikro)],2)

combn(tillid_mikro[1,3:ncol(tillid_mikro)],2)
combn(tillid_mikro[2,3:ncol(tillid_mikro)],2)
combn(tillid_mikro[3,3:ncol(tillid_mikro)],2)
combn(tillid_mikro[4,3:ncol(tillid_mikro)],2)
combn(tillid_mikro[i,3:ncol(tillid_mikro)],2)

y <- colMeans(combn(as.numeric(tillid_mikro[3,3:ncol(tillid_mikro)]),2))


#Opbygning af dataframe med alle kombinationer
indikator_df <- data.frame()  # tom data frame til alt output

for (i in seq_len(nrow(tillid_mikro))) {
  # Hent værdier for ét kvartal
  x <- as.numeric(tillid_mikro[i, 3:ncol(tillid_mikro), drop = FALSE])
  
  # Gå igennem alle kombinationsstørrelser
  for (k in 1:length(x)) {
    # Lav kombinationer for denne størrelse
    mat <- combn(x, k, simplify = TRUE)
    
    # Beregn gennemsnit for hver kolonne (hver kombination)
    means_k <- colMeans(mat)
    
    # Opret midlertidig data.frame med resultaterne
    df_k <- data.frame(
      kvartal = tillid_mikro$kvartal[i],
      k = k,
      komb_nr = seq_along(means_k),
      mean = means_k
    )
    
    # Tilføj til hoved-dataframe
    indikator <- rbind(indikator_df, df_k)
  }
}

#afrunding til 2 decimaler
indikator$mean <- round(indikator$mean,2)


