library(dplyr)
library(ggplot2)
library(RColorBrewer)

# -------------------------------------------------
# 1) Klargør data til stabilitetstest
# -------------------------------------------------

vores_indikator <- plot_df %>%
  select(Kvartal, real_vækst_pct, VORES) %>%
  mutate(Kvartal = as.character(Kvartal)) %>%
  arrange(Kvartal)

# Afhængig og forklarende variabel
y <- vores_indikator$real_vækst_pct   # årlig realvækst
x <- vores_indikator$VORES     # VORES indikator (glattet)

N <- length(y)

# -------------------------------------------------
# 2) Stabilitetstest – fjern observationer FORFRA
# -------------------------------------------------

H <- 25   # antal kvartaler der fjernes

r2_forfra <- numeric(H)

for (h in 0:(H - 1)) {
  idx <- (1 + h):N
  mod <- lm(y[idx] ~ x[idx])
  r2_forfra[h + 1] <- summary(mod)$r.squared
}

# -------------------------------------------------
# 3) Stabilitetstest – fjern observationer BAGFRA
# -------------------------------------------------

r2_bagfra <- numeric(H)

for (h in 0:(H - 1)) {
  idx <- 1:(N - h)
  mod <- lm(y[idx] ~ x[idx])
  r2_bagfra[h + 1] <- summary(mod)$r.squared
}

# -------------------------------------------------
# 4) Saml resultater i ét datasæt
# -------------------------------------------------

stab_df <- bind_rows(
  data.frame(trin = 0:(H - 1), R2 = r2_forfra, retning = "Forfra"),
  data.frame(trin = 0:(H - 1), R2 = r2_bagfra, retning = "Bagfra")
)

# -------------------------------------------------
# 5) Plot stabiliteten
# -------------------------------------------------

ggplot(stab_df, aes(x = trin, y = R2, colour = retning)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title  = "Stabilitet af vores indikator",
    x      = "Antal kvartaler fjernet",
    y      = "R² for regressionen",
    colour = "Testretning"
  ) +
  theme_minimal(base_size = 13)

# -------------------------------------------------
# 6) Gennemsnitlig forklaringskraft
# -------------------------------------------------

mean_forfra <- mean(stab_df$R2[stab_df$retning == "Forfra"], na.rm = TRUE)
mean_bagfra <- mean(stab_df$R2[stab_df$retning == "Bagfra"], na.rm = TRUE)

mean_forfra
mean_bagfra

# -------------------------------------------------
# 7) Plot med gennemsnitslinjer
# -------------------------------------------------

ggplot(stab_df, aes(x = trin, y = R2, colour = retning)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  
  geom_hline(
    yintercept = mean_forfra,
    linetype = "dashed",
    colour = "#E69F00",
    linewidth = 1
  ) +
  geom_hline(
    yintercept = mean_bagfra,
    linetype = "dashed",
    colour = "#009E73",
    linewidth = 1
  ) +
  
  annotate(
    "text",
    x = max(stab_df$trin) - 2,
    y = mean_forfra + 0.003,
    label = paste0("Gns. Forfra: ", round(mean_forfra, 3)),
    colour = "#E69F00",
    hjust = 1
  ) +
  
  annotate(
    "text",
    x = max(stab_df$trin) - 2,
    y = mean_bagfra - 0.003,
    label = paste0("Gns. Bagfra: ", round(mean_bagfra, 3)),
    colour = "#009E73",
    hjust = 1
  ) +
  
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title = "Stabilitet af vores indikator over tid",
    x = "Antal kvartaler fjernet",
    y = "R² fra regressionen",
    colour = "Testretning",
    caption = "Kilde: Danmarks Statistik og egne beregninger"
  ) +
  theme_minimal(base_size = 13)

