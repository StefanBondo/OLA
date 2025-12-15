DST <- FORV1_kvartal_DST[
  FORV1_kvartal_DST$Kvartal >= "2000Q1" &
    FORV1_kvartal_DST$Kvartal <= "2025Q3",
]

spm <- DST[, 3:14]
com2 <- combn(spm[1, ], 2)

y <- colMeans(combn(as.numeric(spm[1, ]), 2))
y

comliste <- list()

comliste2 <- list()

for (i in 1:nrow(spm)) {
  kvartal_list <- list()
  
  for (k in 1:12) {
    kvartal_list[[k]] <- combn(as.numeric(spm[i, ]), k)
  }
  
  names(kvartal_list) <- paste0("k = ", 1:12)
  comliste2[[i]] <- kvartal_list
}

names(comliste2) <- DST$Kvartal



antal_spm <- ncol(spm)   # = 12

k <- 1:antal_spm
antal_komb <- choose(antal_spm, k)

oversigt <- data.frame(
  "Antal spørgsmål (k)" = k,
  "Antal kombinationer" = antal_komb
)

oversigt
sum(antal_komb)  # = 4095

comliste2[["2000Q1"]]

