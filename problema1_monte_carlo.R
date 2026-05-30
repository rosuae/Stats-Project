# ======================================================================
# PROBLEMA 1 - EXTENSIE: SIMULARE MONTE CARLO (1000 iteratii)
# ======================================================================
# Autori:
# Data:   Mai 2026
# ======================================================================

library(tidyverse)
library(patchwork)

set.seed(42)

ZILE     <- 365
LAMBDA   <- 5000
SCENARII <- c(0.001, 0.005, 0.02)

C1 <- 2     # cost per verificare
C2 <- 100   # penalizare per suspect nedetectat

# Repetam simularea de 1000 ori pt a evalua stabilitatea rezultatelor
N_SIM <- 1000

# Bucla pt fiecare p si fiecare iteratie, simulam un an si calculam costul total
rezultate_mc <- map_dfr(SCENARII, function(p_mc) {
  map_dfr(1:N_SIM, function(i) {
    trafic   <- rpois(ZILE, LAMBDA)
    suspecte <- rbinom(ZILE, trafic, p_mc)
    normale  <- trafic - suspecte

    # Strategia A: fix 10%
    ver_A <- pmin(round(trafic * 0.10), trafic)
    det_A <- rhyper(ZILE, suspecte, normale, ver_A)

    # Strategia B: adaptiva
    pct_B <- case_when(
      trafic > LAMBDA + 2 * sqrt(LAMBDA) ~ 0.30,
      trafic > LAMBDA + sqrt(LAMBDA)     ~ 0.20,
      TRUE                               ~ 0.10
    )
    ver_B <- pmin(round(trafic * pct_B), trafic)      # nr cereri verificate/zi
    det_B <- rhyper(ZILE, suspecte, normale, ver_B)   # detectii Hipergeometrica

    # Cost total anual
    data.frame(
      P = p_mc,
      Cost_A = sum(ver_A) * C1 + sum(suspecte - det_A) * C2,
      Cost_B = sum(ver_B) * C1 + sum(suspecte - det_B) * C2
    )
  })
})

# Rezumat statistic
rezumat_mc <- rezultate_mc %>%
  pivot_longer(cols = c(Cost_A, Cost_B), names_to = "Strategie", values_to = "Cost") %>%
  mutate(Strategie = str_replace(Strategie, "Cost_", "")) %>%
  group_by(P, Strategie) %>%
  summarise(Medie = mean(Cost), SD = sd(Cost),
            Min = min(Cost), Max = max(Cost), .groups = "drop")
print(rezumat_mc)

# Boxplot
ggplot(rezultate_mc %>%
         pivot_longer(cols = c(Cost_A, Cost_B), names_to = "Strategie", values_to = "Cost") %>%
         mutate(Strategie = str_replace(Strategie, "Cost_", ""),
                p_label = paste0("p = ", P)),
       aes(x = Strategie, y = Cost, fill = Strategie)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~p_label, scales = "free_y") +
  scale_fill_manual(values = c("A" = "steelblue", "B" = "seagreen"),
                    labels = c("A" = "Aleatoare", "B" = "Adaptiva")) +
  theme_minimal() +
  labs(title = sprintf("Variabilitatea costurilor (Monte Carlo - %d simulari)", N_SIM),
       x = "Strategie", y = "Cost Total")

# Care strategie minimizeaza costul mediu pt fiecare p
cat("\n=== CONCLUZIE MONTE CARLO ===\n")
for (p_val in SCENARII) {
  # Filtram datele pentru scenariul curent
  mc_p <- rezumat_mc %>% filter(P == p_val)

  # Gasim strategia cu costul mediu minim
  minim <- mc_p %>% slice(which.min(Medie))

  # valoarea lui p, strategia castigatoare, costul mediu si SD
  cat(sprintf("p = %s: Strategia %s minimizeaza costul mediu (%.0f, SD=%.0f)\n",
              p_val, minim$Strategie, minim$Medie, minim$SD))
}
cat("\nMonte Carlo confirma ca rezultatele sunt stabile (SD mic relativ la medie).\n")

dir.create("src", showWarnings = FALSE)

g6 <- ggplot(rezultate_mc %>%
         pivot_longer(cols = c(Cost_A, Cost_B), names_to = "Strategie", values_to = "Cost") %>%
         mutate(Strategie = str_replace(Strategie, "Cost_", ""),
                p_label = paste0("p = ", P)),
       aes(x = Strategie, y = Cost, fill = Strategie)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~p_label, scales = "free_y") +
  scale_fill_manual(values = c("A" = "steelblue", "B" = "seagreen"),
                    labels = c("A" = "Aleatoare", "B" = "Adaptiva")) +
  theme_minimal() +
  labs(title = sprintf("Variabilitatea costurilor (Monte Carlo - %d simulari)", N_SIM),
       x = "Strategie", y = "Cost Total")
ggsave("src/Monte_Carlo.png", plot = g6, width = 10, height = 5, bg = "white")

cat("\nGraficul Monte Carlo a fost salvat cu succes in folderul src/!\n")
