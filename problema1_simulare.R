# ======================================================================
# PROBLEMA 1: Simularea Evenimentelor Rare - Detectia Cererilor Suspecte
# ======================================================================
# Autori: [numele echipei]
# Data:   Mai 2026
# ======================================================================

library(tidyverse)
library(patchwork)

# === Parametri ===
set.seed(42)
ZILE     <- 365
LAMBDA   <- 5000    # media cereri/zi pt distributia Poisson
SCENARII <- c(0.001, 0.005, 0.02)

# Costuri (cerinta 9)
C1 <- 2     # cost per verificare
C2 <- 100   # penalizare per suspect nedetectat

# === Functia de simulare ===
# Genereaza un an de date pt un scenariu p si aplica ambele strategii:
#   A = verificare aleatoare (procent fix 10%)
#   B = verificare adaptiva (creste procentul cand traficul e mare)
#
# Distributii folosite:
#   Poisson(lambda) -> nr total cereri/zi
#   Binomiala(n, p) -> nr cereri suspecte
#   Hipergeometrica -> detectie (extragere FARA inlocuire)
simuleaza_scenariu <- function(p_suspect) {
  total_cereri    <- rpois(ZILE, lambda = LAMBDA)
  cereri_suspecte <- rbinom(ZILE, size = total_cereri, prob = p_suspect)
  cereri_normale  <- total_cereri - cereri_suspecte

  # Strategia A: procent fix 10%
  verificate_A <- pmin(round(total_cereri * 0.10), total_cereri)

  # Strategia B: adaptiva graduala (praguri bazate pe sd = sqrt(lambda))
  pct_B <- case_when(
    total_cereri > LAMBDA + 2 * sqrt(LAMBDA) ~ 0.30,
    total_cereri > LAMBDA + sqrt(LAMBDA)     ~ 0.20,
    TRUE                                     ~ 0.10
  )
  verificate_B <- pmin(round(total_cereri * pct_B), total_cereri)

  # Detectie cu hipergeometrica (urna fara inlocuire)
  detectate_A <- rhyper(ZILE, cereri_suspecte, cereri_normale, verificate_A)
  detectate_B <- rhyper(ZILE, cereri_suspecte, cereri_normale, verificate_B)

  data.frame(
    Ziua = 1:ZILE, Total_Cereri = total_cereri,
    Suspecte = cereri_suspecte, Normale = cereri_normale,
    P_Scenariu = as.factor(p_suspect),
    Verificate_A = verificate_A, Detectate_A = detectate_A,
    Nedetectate_A = cereri_suspecte - detectate_A,
    Verificate_B = verificate_B, Detectate_B = detectate_B,
    Nedetectate_B = cereri_suspecte - detectate_B
  )
}

# === Rulam simularea pt cele 3 scenarii ===
# map_dfr aplica functia pe fiecare p si lipeste rezultatele automat
date_simulare <- map_dfr(SCENARII, simuleaza_scenariu)

# Transformam in format long (o coloana Strategie cu A/B)
date_long <- date_simulare %>%
  pivot_longer(
    cols = matches("_[AB]$"),
    names_to = c(".value", "Strategie"),
    names_pattern = "(.*)_(.)"
  )

# === Cerinta 5: Metrici de performanta ===
metrici <- date_long %>%
  group_by(P_Scenariu, Strategie) %>%
  summarise(
    Prob_Detectie      = mean(Detectate > 0),
    Prop_Detectate     = mean(ifelse(Suspecte == 0, NA, Detectate / Suspecte), na.rm = TRUE),
    Prop_Nedetectate   = mean(ifelse(Suspecte == 0, NA, Nedetectate / Suspecte), na.rm = TRUE),
    Medie_Verificari   = mean(Verificate),
    # Indicator de eficienta: detectii la 1000 verificari
    Eficienta          = sum(Detectate) / sum(Verificate) * 1000,
    .groups = "drop"
  )
print(metrici)

# === Cerinta 6: Grafice ===

# 6.1 Histograma cereri suspecte pe zi
g1 <- ggplot(date_long %>% filter(Strategie == "A"),
             aes(x = Suspecte, fill = P_Scenariu)) +
  geom_histogram(bins = 30, alpha = 0.7, color = "white") +
  facet_wrap(~P_Scenariu, scales = "free") +
  theme_minimal() +
  labs(title = "Distributia cererilor suspecte pe zi",
       x = "Nr. suspecte/zi", y = "Frecventa") +
  theme(legend.position = "none")
print(g1)

# 6.2 Histograma detectiilor (A vs B, fixam p = 0.005)
g2 <- ggplot(date_long %>% filter(P_Scenariu == "0.005"),
             aes(x = Detectate, fill = Strategie)) +
  geom_histogram(bins = 25, alpha = 0.6, position = "dodge", color = "black") +
  scale_fill_manual(values = c("A" = "steelblue", "B" = "seagreen"),
                    labels = c("A" = "Aleatoare", "B" = "Adaptiva")) +
  theme_minimal() +
  labs(title = "Detectii zilnice per strategie (p = 0.005)",
       x = "Nr. detectate/zi", y = "Frecventa")
print(g2)

# 6.3 Grafic comparativ intre strategii
g3 <- ggplot(metrici, aes(x = P_Scenariu, y = Prop_Detectate, fill = Strategie)) +
  geom_col(position = "dodge", color = "black") +
  scale_fill_manual(values = c("A" = "steelblue", "B" = "seagreen"),
                    labels = c("A" = "Aleatoare", "B" = "Adaptiva")) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(title = "Rata medie de detectie per strategie",
       x = "Probabilitate (p)", y = "Proportia detectata")
print(g3)

# 6.4 Evolutia zilnica suspecte vs detectate (p = 0.005)
g4 <- ggplot(date_long %>% filter(P_Scenariu == "0.005", Ziua <= 60),
             aes(x = Ziua)) +
  geom_line(aes(y = Suspecte, color = "Suspecte"), linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = Detectate, color = Strategie), linewidth = 1) +
  scale_color_manual(
    values = c("Suspecte" = "red", "A" = "steelblue", "B" = "seagreen"),
    labels = c("Suspecte" = "Total suspecte", "A" = "Det. Aleatoare", "B" = "Det. Adaptiva")
  ) +
  theme_minimal() +
  labs(title = "Evolutia zilnica (p = 0.005, primele 60 zile)",
       x = "Ziua", y = "Nr. cereri", color = "")
print(g4)

# Combinam graficele cu patchwork
print((g1 | g2) / (g3 | g4))

# === Cerinta 7: Efectul cresterii procentului de verificare ===
# Generam date pt p = 0.005 si testam 5 procente diferite
trafic_q   <- rpois(ZILE, LAMBDA)
suspecte_q <- rbinom(ZILE, trafic_q, 0.005)
normale_q  <- trafic_q - suspecte_q

rezultate_q <- map_dfr(c(0.01, 0.05, 0.10, 0.20, 0.30), function(pct) {
  ver <- pmin(round(trafic_q * pct), trafic_q)
  det <- rhyper(ZILE, suspecte_q, normale_q, ver)
  data.frame(
    Procent = pct * 100,
    Prob_Detectie = mean(det > 0),
    Prop_Detectata = mean(ifelse(suspecte_q == 0, NA, det / suspecte_q), na.rm = TRUE)
  )
})
print(rezultate_q)

ggplot(rezultate_q, aes(x = Procent)) +
  geom_line(aes(y = Prob_Detectie, color = "P(detectie >= 1/zi)"), linewidth = 1.2) +
  geom_point(aes(y = Prob_Detectie), size = 3) +
  geom_line(aes(y = Prop_Detectata, color = "Proportie detectata"), linewidth = 1.2, linetype = "dashed") +
  geom_point(aes(y = Prop_Detectata), size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  labs(title = "Efectul cresterii procentului de verificare (p = 0.005)",
       x = "Procent verificare (%)", y = "Rata", color = "Metrica")

# === Cerinta 9: Analiza de costuri ===
# C = c1 * nr_verificari + c2 * nr_nedetectate
costuri <- date_long %>%
  group_by(P_Scenariu, Strategie) %>%
  summarise(
    Cost_Verificari = sum(Verificate) * C1,
    Cost_Penalizari = sum(Nedetectate) * C2,
    Cost_Total      = Cost_Verificari + Cost_Penalizari,
    .groups = "drop"
  )
print(costuri)

# === Cerinta 8: Concluzie practica ===
cat("\n=== CONCLUZIE ===\n")
cat("Strategia adaptiva detecteaza mai mult, dar costa mai mult in verificari.\n")
cat("Alegerea depinde de raportul c2/c1 (cat de grav e sa ratezi un suspect).\n")
cat("Pentru a analiza variabilitatea statistica, vezi fisierul problema1_monte_carlo.R\n")
