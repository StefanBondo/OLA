library(caret)

#1. kvartal 1998 til 3. kvartal 2025

NKN1_98 <-NKN1 %>% slice(-c(1:8))

#Opgave 3.1
NKN1_98 <- NKN1_98 %>%
  mutate(
    stigning_dummy = as.integer(real_vækst_pct > 0)
  )

sum(NKN1_98$stigning_dummy == 1)          # antal stigninger
#[1] 83

mean(NKN1_98$stigning_dummy == 1) * 100   # procent stigninger
#[1] 74.77477%

sum(NKN1_98$stigning_dummy == 0)          # antal fald
#[1] 28

mean(NKN1_98$stigning_dummy == 0) * 100   # procent fald
#[1] 25.22523%


#Barplot
df_dummy <- data.frame(
  kategori = c("Stigning", "Fald"),
  antal = c(
    sum(NKN1_98$stigning_dummy == 1),
    sum(NKN1_98$stigning_dummy == 0)
  )
)

df_dummy$procent <- round(df_dummy$antal / sum(df_dummy$antal) * 100, 1)


library(ggplot2)

ggplot(df_dummy, aes(x = kategori, y = procent, fill = kategori)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(procent, "%")), 
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Stigning" = "#4da6ff", "Fald" = "#ff6b6b")) +
  labs(
    title = "Der er flest stigninger",
    caption = "Kilde: DST",
    x = "",
    y = "Procent"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

