#-------------------------------------------------------
# Nye ændringer efter vi er kommet tilbage til den
#-------------------------------------------------------
# Indlæs backup
newhomes <- as.data.frame(newhomes_backup4)

# Fjerne alle NA værdier
newhomes <- na.omit(newhomes)

# Rydder op i Energimærke
newhomes <- subset(newhomes, energi != "Intet energimærke")
newhomes$energi <- trimws(newhomes$energi)
newhomes$energi[newhomes$energi %in% c("A2", "A2010", "A2015", "A2020")] <- "A"

# Rydder op i type
newhomes$type <- trimws(newhomes$type)
newhomes$type <- sapply(strsplit(newhomes$type, "/"), `[`, 1)

# ---------------------------
# FJERN OUTLIERS I GRUND
# ---------------------------
Q1_g <- quantile(newhomes$grund, 0.25, na.rm = TRUE)
Q3_g <- quantile(newhomes$grund, 0.75, na.rm = TRUE)
IQR_g <- Q3_g - Q1_g

lower_g <- Q1_g - 1.5 * IQR_g
upper_g <- Q3_g + 1.5 * IQR_g

newhomes <- newhomes[newhomes$grund >= lower_g & newhomes$grund <= upper_g, ]

boxplot(newhomes$grund,
        main = "Boxplot af grund",
        ylab = "Grund (m2)")

summary(newhomes$grund)




# ---------------------------
# FJERN OUTLIERS I EJERUDGIFT
# ---------------------------
Q1_e <- quantile(newhomes$ejerudg, 0.25, na.rm = TRUE)
Q3_e <- quantile(newhomes$ejerudg, 0.75, na.rm = TRUE)
IQR_e <- Q3_e - Q1_e

lower_e <- Q1_e - 1.5 * IQR_e
upper_e <- Q3_e + 1.5 * IQR_e

newhomes <- newhomes[newhomes$ejerudg >= lower_e & newhomes$ejerudg <= upper_e, ]

boxplot(newhomes$ejerudg,
        main = "Boxplot af ejerudgift",
        ylab = "Ejerudgift (kr.)")

summary(newhomes$ejerudg)

# ---------------------------
# FJERN OUTLIERS I KVM
# ---------------------------
Q1_k <- quantile(newhomes$kvm, 0.25, na.rm = TRUE)
Q3_k <- quantile(newhomes$kvm, 0.75, na.rm = TRUE)
IQR_k <- Q3_k - Q1_k

lower_k <- Q1_k - 1.5 * IQR_k
upper_k <- Q3_k + 1.5 * IQR_k

newhomes <- newhomes[newhomes$kvm >= lower_k & newhomes$kvm <= upper_k, ]

summary(newhomes$kvm)

boxplot(newhomes$kvm,
        main = "Boxplot af kvm",
        ylab = "Kvadratmeter")

# Rens grund for outliers (igen)
Q1_g <- quantile(newhomes$grund, 0.25, na.rm = TRUE)
Q3_g <- quantile(newhomes$grund, 0.75, na.rm = TRUE)
IQR_g <- Q3_g - Q1_g

lower_g <- Q1_g - 1.5 * IQR_g
upper_g <- Q3_g + 1.5 * IQR_g

newhomes <- newhomes[newhomes$grund >= lower_g & newhomes$grund <= upper_g, ]

summary(newhomes$grund)

# Lav pris pr. kvadrat og afrund decimaler
newhomes$pris_per_kvm <- newhomes$pris / newhomes$kvm
newhomes$pris_per_kvm <- round(newhomes$pris_per_kvm, )

# Rens pris_per_kvm for outliers
boxplot(newhomes$pris_per_kvm,
        main = "Boxplot af pris_per_kvm",
        ylab = "Kroner (kr.)")

Q1_p <- quantile(newhomes$pris_per_kvm, 0.25, na.rm = TRUE)
Q3_p <- quantile(newhomes$pris_per_kvm, 0.75, na.rm = TRUE)
IQR_p <- Q3_p - Q1_p

lower_p <- Q1_p - 1.5 * IQR_p
upper_p <- Q3_p + 1.5 * IQR_p

newhomes <- newhomes[newhomes$pris_per_kvm >= lower_p & 
                       newhomes$pris_per_kvm <= upper_p, ]

summary(newhomes$pris_per_kvm)

# ---------------------------
# Beskrivende statistisk
# ---------------------------

# Hent summary for pris

# Summary af pris
s <- summary(newhomes$pris)

# Lav summary om til numerisk vektor
s_num <- as.numeric(s)

# Lav barplot og gem placeringerne
bp <- barplot(s_num,
              names.arg = names(s),
              main = "Summary af Pris",
              ylab = "Kr.",
              xlab = "Statistik",
              col = "lightblue",
              las = 2)

# Skriv tal over søjlerne
text(x = bp,
     y = s_num,
     labels = s_num,
     pos = 3,        # pos 3 = over søjlen
     cex = 0.8)

#--------------------------


#hent summary for kvm
kvm <- summary(newhomes$kvm)

# Lav summary om til numerisk vektor
kvm_num <- as.numeric(kvm)

# Lav barplot og gem placeringerne
bp <- barplot(kvm_num,
              names.arg = names(kvm),
              main = "Summary af kvm",
              ylab = "kvadratmeter",
              xlab = "Statistik",
              col = "lightblue",
              las = 2)

# Skriv tal over søjlerne
text(x = bp,
     y = kvm_num,
     labels = kvm_num,
     pos = 3,        # pos 3 = over søjlen
     cex = 0.8)
kvm
#--------------------------

#hent summary for grund
grund <- summary(newhomes$grund)

# Lav summary om til numerisk vektor
grund_num <- as.numeric(grund)

# Lav barplot og gem placeringerne
bp <- barplot(grund_num,
              names.arg = names(grund),
              main = "Summary af grund",
              ylab = "kvadratmeter",
              xlab = "Statistik",
              col = "lightblue",
              las = 2)
grund
# Skriv tal over søjlerne
text(x = bp,
     y = grund_num,
     labels = grund_num,
     pos = 3,        # pos 3 = over søjlen
     cex = 0.8)

#--------------------------

#hent summary for grund
pris_per_kvm_sum <- summary(newhomes$pris_per_kvm)

# Lav summary om til numerisk vektor
pris_per_kvm_sum <- as.numeric(pris_per_kvm_sum)

# Lav barplot og gem placeringerne
bp <- barplot(pris_per_kvm_sum,
              names.arg = names(pris_per_kvm_sum),
              main = "Summary af pris per kvm",
              ylab = "Kroner",
              xlab = "Statistik",
              col = "lightblue",
              las = 2)

# Skriv tal over søjlerne
text(x = bp,
     y = pris_per_kvm_sum,
     labels = pris_per_kvm_sum, 
     pos = 3,        # pos 3 = over søjlen
     cex = 0.8)

# Frekvenser af energimærker
freq <- table(newhomes$energi)

# Barplot
bp <- barplot(freq,
              main = "Fordeling af energimærker",
              ylab = "Antal boliger",
              xlab = "Energimærke",
              col = "lightgreen",
              las = 2)

# Skriv antal over hver søjle
text(x = bp,
     y = freq,
     labels = freq,
     pos = 3,
     cex = 0.8)

#--------------------------
# Frekvenser af værelser

# Fjern boliger med uønskede antal værelser
newhomes <- subset(newhomes, !(vaer %in% c(1, 8, 9, 10, 15)))
table(newhomes$vaer)

freq <- table(newhomes$vaer)
freq

# Barplot
bp <- barplot(freq,
              main = "Fordeling af værelser",
              ylab = "Antal boliger",
              xlab = "Antal værelser",
              col = "lightgreen",
              las = 1)

# Skriv antal over hver søjle
text(x = bp,
     y = freq,
     labels = freq,
     pos = 3,
     cex = 0.8)

#----------------------------
#Aggrationer
#_--------------------------
#✅ 1) Gennemsnitspris per boligtype
# Beregn gennemsnitspris
price_type <- aggregate(pris ~ type, data = newhomes, FUN = mean)

bp <- barplot(price_type$pris,
              names.arg = price_type$type,
              main = "Gennemsnitspris per boligtype",
              ylab = "Pris (kr.)",
              col = "lightblue",
              las = 2)

text(bp,
     price_type$pris,
     labels = round(price_type$pris),
     pos = 3,
     cex = 0.8)

#✅ 2) Median pris per kvm per boligtype
# Beregn median pris per kvm
ppk_type <- aggregate(pris_per_kvm ~ type, data = newhomes, FUN = median)

bp <- barplot(ppk_type$pris_per_kvm,
              names.arg = ppk_type$type,
              main = "Median pris per kvm per boligtype",
              ylab = "Kr. pr. kvm",
              col = "lightgreen",
              las = 2)

text(bp,
     ppk_type$pris_per_kvm,
     labels = round(ppk_type$pris_per_kvm),
     pos = 3,
     cex = 0.8)

#✅ 3) Gennemsnitligt boligareal (kvm) per boligtype
# Beregn gennemsnitlig kvm per type
kvm_type <- aggregate(kvm ~ type, data = newhomes, FUN = mean)

bp <- barplot(kvm_type$kvm,
              names.arg = kvm_type$type,
              main = "Gennemsnitligt boligareal per boligtype",
              ylab = "Kvadratmeter",
              col = "orange",
              las = 2)

text(bp,
     kvm_type$kvm,
     labels = round(kvm_type$kvm),
     pos = 3,
     cex = 0.8)
