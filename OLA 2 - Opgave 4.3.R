library(dplyr)
library(tidyr)
library(readxl)

Forbruggrupper <- Forbruggrupper %>%
  pivot_longer(
    cols = 2:16,               # dine 15 forbrugsgrupper
    names_to = "Gruppe",       # navnet på gruppen
    values_to = "Værdi"        # tallet for gruppen
  ) %>%
  pivot_wider(
    names_from = Gruppe,       # lav kolonner ud fra gruppekoderne
    values_from = Værdi
  ) %>%
  mutate(
    År_numeric = as.numeric(format(År, "%Y"))   # træk år ud af datoen
  )

# ----------------------------------------------------------
# 3. Hvad brugte danskerne flest penge på i 2024?
# ----------------------------------------------------------

størst_2024 <- Forbruggrupper %>%
  filter(År_numeric == 2024) %>%
  select(-År, -År_numeric) %>%              # behold kun forbrugsgrupper
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(),
               names_to = "Gruppe",
               values_to = "Værdi_2024") %>%
  arrange(desc(Værdi_2024)) %>%
  slice(1)

størst_2024

# ----------------------------------------------------------
# 4. Hvilken gruppe steg mest fra 2020 → 2024?
# ----------------------------------------------------------

ændringer <- Forbruggrupper %>%
  filter(År_numeric %in% c(2020, 2024)) %>%
  group_by(År_numeric) %>%
  summarise(across(2:16, mean, na.rm = TRUE)) %>%   # <-- Fixet!
  pivot_longer(cols = -År_numeric,
               names_to = "Gruppe",
               values_to = "Værdi") %>%
  pivot_wider(names_from = År_numeric,
              values_from = Værdi) %>%
  mutate(Stigning = `2024` - `2020`) %>%
  arrange(desc(Stigning))


ændringer

