# 1) Pris pr kvm ~ kvm
m1 <- lm(pris_per_kvm ~ kvm, data = newhomes)

summary(m1) #Multiple R-squared:  0.0003974, = 0,039%

plot(newhomes$kvm, newhomes$pris_per_kvm,
     main = "Der er ingen sammenhæng mellem kvm og pris per kvm",
     xlab = "kvm",
     ylab = "Pris per kvm (kr.)",
     pch = 19,
     col = rgb(0,0,1,0.3))

# Ekstra tekst under overskriften
mtext("Multiple R-squared:  0.0003974 = 0,039%", line = 0.5)

# Regressionslinje
abline(m1, col = "red", lwd = 2)



# 2) Pris pr kvm ~ grund
m2 <- lm(pris_per_kvm ~ grund, data = newhomes)

summary(m2) #Multiple R-squared:  0.03015,	= 3,0%

plot(newhomes$grund, newhomes$pris_per_kvm,
     main = "Der er Svag sammenhæng mellem pris_per_kvm og grund",
     xlab = "Grund (m2)",
     ylab = "Pris per kvm (kr.)",
     pch = 19, col = rgb(0,0,1,0.3))

# Ekstra tekst under overskriften
mtext("Multiple R-squared:  0.03015 = 3,0%", line = 0.5)

abline(m2, col = "red", lwd = 2)


# 3) Pris pr kvm ~ ejerudg
m3 <- lm(pris_per_kvm ~ ejerudg, data = newhomes)

summary(m3) #Multiple R-squared:  0.673, = 67,3%

plot(newhomes$ejerudg, newhomes$pris_per_kvm,
     main = "Der er Stærk sammenhæng mellem pris_per_kvm og ejerudgift",
     xlab = "Ejerudgift (kr.)",
     ylab = "Pris per kvm (kr.)",
     pch = 19, col = rgb(0,0,1,0.3))

# Ekstra tekst under overskriften
mtext("Multiple R-squared:  0.673 = 67,3%", line = 0.5)

abline(m3, col = "red", lwd = 2)


# 4) Pris per kvm ~ pris
m4 <- lm(pris_per_kvm ~ pris, data = newhomes)

summary(m4) #Multiple R-squared:  0.7971 = 79,7%

plot(newhomes$pris, newhomes$pris_per_kvm,
     main = "Der er Stærk sammenhæng mellem pris_per_kvm ~ pris",
     xlab = "Pris (kr.)",
     ylab = "Pris per kvm (kr.)",
     pch = 19, col = rgb(0,0,1,0.3))

# Ekstra tekst under overskriften
mtext("Multiple R-squared:  0.7971 = 79,7%", line = 0.5)

abline(m4, col = "red", lwd = 2)



# 5) Pris per kvm ~ alder
m5 <- lm(pris_per_kvm ~ alder, data = newhomes)

summary(m5) #Multiple R-squared:  0.05209 = 5,2%

plot(newhomes$alder, newhomes$pris_per_kvm,
     main = "Der er Svag sammenhæng mellem pris_per_kvm ~ alder",
     xlab = "Boligens alder (år)",
     ylab = "Pris per kvm (kr.)",
     pch = 19, col = rgb(0,0,1,0.3))

abline(m5, col = "red", lwd = 2)

# Ekstra tekst under overskriften
mtext("Multiple R-squared:  0.05209 = 5,2%", line = 0.5)






# Installér pakkes kun første gang
install.packages("corrplot")
library(corrplot)


# Udvælg numeriske variable
num_vars <- newhomes[, c("grund", "kvm", "pris_per_kvm", "ejerudg")]

# Beregn korrelationer
cor_matrix <- cor(num_vars)

# Find højeste korrelation (uden diagonal)
upper <- cor_matrix
upper[lower.tri(upper, diag = TRUE)] <- NA
max_cor <- max(abs(upper), na.rm = TRUE)

# Overskrift
title_text <- paste("Der er stærkest korrelation mellem ejerudgift og pris pr. kvm:", round(max_cor, 3))

# Plot fuld matrix (begge sider)
corrplot(cor_matrix,
         method = "color",
         type = "full",        # <-- FULD matrix
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         col = colorRampPalette(c("yellow", "orange", "red", "darkred"))(200),
         mar = c(0,0,3,0))

# Tilføj titel
title(title_text, line = 1)






