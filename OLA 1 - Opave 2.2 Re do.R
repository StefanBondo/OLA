cor.test(newhomes$pris, newhomes$kvm)
#      cor 
#0.4117872 

plot(newhomes$kvm, newhomes$pris,
     main = "Større boliger koster mere, men sammenhængen er ikke perfekt.",
     caption = "Kilde: Boligsiden",
     xlab = "Boligareal (kvm)",
     ylab = "Pris (kr.)",
     pch = 19,      # små prikker
     col = rgb(0, 0, 1, 0.4))  # gennemsigtig blå

# Tilføj regressionslinje
model <- lm(pris ~ kvm, data = newhomes)
abline(model, col = "red", lwd = 2)

options(scipen = 999)

cor.test()