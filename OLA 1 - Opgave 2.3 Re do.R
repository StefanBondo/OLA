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

# ---------------------------
# FJERN OUTLIERS I aldet
# ---------------------------

Q1_a <- quantile(newhomes$alder, 0.25, na.rm = TRUE)
Q3_a <- quantile(newhomes$alder, 0.75, na.rm = TRUE)
IQR_a <- Q3_a - Q1_a

lower_a <- Q1_a - 1.5 * IQR_a
upper_a <- Q3_a + 1.5 * IQR_a

newhomes_no_out <- newhomes[newhomes$alder >= lower_a & newhomes$alder <= upper_a, ]

m_alder_no_out <- lm(pris_per_kvm ~ alder, data = newhomes_no_out)
summary(m_alder_no_out) #Multiple R-squared:  0.06596 = 6,5%
R2 <- summary(m_alder_no_out)$r.squared

plot(newhomes_no_out$alder, newhomes_no_out$pris_per_kvm,
     main = "Der er meget svag sammenhæng mellem pris per kvm og alder",
     xlab = "Boligens alder (år)",
     ylab = "Pris per kvm (kr.)",
     pch = 19,
     col = rgb(0,0,1,0.3))

abline(m_alder_no_out, col = "red", lwd = 2)

# Ekstra tekst under overskriften
mtext("Multiple R-squared:  0.06596 = 6,5%", line = 0.5)






# Installér pakkes kun første gang
install.packages("corrplot")
library(corrplot)


library(corrplot)

# Udvælg numeriske variable
num_vars <- newhomes[, c("grund", "kvm", "alder", "pris_per_kvm", "ejerudg")]

# Beregn korrelationer
cor_matrix <- cor(num_vars)

# Find højeste korrelation (uden diagonal)
upper <- cor_matrix
upper[lower.tri(upper, diag = TRUE)] <- NA
max_cor <- max(abs(upper), na.rm = TRUE)

# Lav overskrift-tekst
title_text <- "Der er stærkest korrelation mellem pris_per_kvm og ejerudgift:"

# Plot fuld korrelationsmatrix
corrplot(cor_matrix,
         method = "color",
         type = "full",               # viser begge sider af matrixen
         addCoef.col = "black",       # tilføj tal i felterne
         tl.col = "black",            # farve på labels
         tl.srt = 45,                 # rotation af labels
         col = colorRampPalette(c("yellow", "orange", "red", "darkred"))(200),
         mar = c(0, 0, 3, 0))

# Tilføj titel
title(title_text, line = 1)


