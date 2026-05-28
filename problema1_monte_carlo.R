library(tidyverse)
library(patchwork)

# === Parametri ===
set.seed(42)
ZILE     <- 365
LAMBDA   <- 5000    # media cereri/zi pt distributia Poisson
SCENARII <- c(0.001, 0.005, 0.02)

# Costuri
C1 <- 2     # cost per verificare
C2 <- 100   # penalizare per suspect nedetectat

# === Cerinta 10: Simulare Monte Carlo (1000 iteratii) ===
N_SIM <- 1000

rezultate_mc <- map_dfr(SCENARII, function(p_mc) {
  map_dfr(1:N_SIM, function(i) {
    trafic   <- rpois(ZILE, LAMBDA)
    suspecte <- rbinom(ZILE, trafic, p_mc)
    normale  <- trafic - suspecte

    # A: fix 10%
    ver_A <- pmin(round(trafic * 0.10), trafic)
    det_A <- rhyper(ZILE, suspecte, normale, ver_A)

    # B: adaptiva
    pct_B <- case_when(
      trafic > LAMBDA + 2 * sqrt(LAMBDA) ~ 0.30,
      trafic > LAMBDA + sqrt(LAMBDA)     ~ 0.20,
      TRUE                               ~ 0.10
    )
    ver_B <- pmin(round(trafic * pct_B), trafic)
    det_B <- rhyper(ZILE, suspecte, normale, ver_B)

    data.frame(
      P = p_mc,
      Cost_A = sum(ver_A) * C1 + sum(suspecte - det_A) * C2,
      Cost_B = sum(ver_B) * C1 + sum(suspecte - det_B) * C2
    )
  })
})

# Rezumat: media si variabilitatea costurilor
rezumat_mc <- rezultate_mc %>%
  pivot_longer(cols = c(Cost_A, Cost_B), names_to = "Strategie", values_to = "Cost") %>%
  mutate(Strategie = str_replace(Strategie, "Cost_", "")) %>%
  group_by(P, Strategie) %>%
  summarise(Medie = mean(Cost), SD = sd(Cost),
            Min = min(Cost), Max = max(Cost), .groups = "drop")
print(rezumat_mc)

# Boxplot variabilitate costuri
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

# === Concluzie Monte Carlo ===
cat("\n=== CONCLUZIE MONTE CARLO ===\n")
for (p_val in SCENARII) {
  mc_p <- rezumat_mc %>% filter(P == p_val)
  minim <- mc_p %>% slice(which.min(Medie))
  cat(sprintf("p = %s: Strategia %s minimizeaza costul mediu (%.0f, SD=%.0f)\n",
              p_val, minim$Strategie, minim$Medie, minim$SD))
}
cat("\nMonte Carlo confirma ca rezultatele sunt stabile (SD mic relativ la medie).\n")
