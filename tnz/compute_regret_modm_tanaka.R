# ------------------------------------------------------------
# Compute regret for MODM on Tanaka problem
# Author: Akanksha Mishra
# Date: 5 Feb 2026
# ------------------------------------------------------------

rm(list = ls())
# Load MODM results
load("tnz-bcl-results.RData")  
# gives:
# final_solutions_df  (x1, x2, Iteration)
# final_objectives_df (obj_1, obj_2, Iteration)

# True utility weights (same as used in preference simulation)
w_true <- c(0.85, 0.15)

utility <- function(x) {
  w_true[1] * x[1] + w_true[2] * x[2]
}

# Tanaka constraints (feasible region definition)
g1 <- function(x) {
  -(x[1]^2 + x[2]^2 - 1 - 0.1 * cos(16 * atan2(x[1], x[2])))
}

g2 <- function(x) {
  (x[1] - 0.5)^2 + (x[2] - 0.5)^2 - 0.25
}

constraint_violation <- function(x) {
  v <- max(0, g1(x)) + max(0, g2(x))
  
  # safety guard against NA/NaN
  if (is.na(v) || is.nan(v)) {
    return(Inf)
  }
  
  return(v)
}

# Compute reference optimal constrained utility u*
# (dense grid search over Tanaka domain)
cat("Computing reference optimal utility...\n")

grid <- seq(0, pi, length.out = 600)
u_star <- Inf

for (x1 in grid) {
  for (x2 in grid) {
    x <- c(x1, x2)
    if (constraint_violation(x) <= 0.1) {
      u_star <- min(u_star, utility(x))
    }
  }
}

cat("Reference constrained optimal utility u* =", u_star, "\n\n")

# Compute regret per iteration
n_iter <- nrow(final_solutions_df)

regret <- numeric(n_iter)
best_u <- Inf

for (t in 1:n_iter) {
  
  x_t <- c(final_solutions_df$x1[t], final_solutions_df$x2[t])
  u_t <- utility(x_t)
  
  best_u <- min(best_u, u_t)
  
  regret[t] <- best_u - u_star
}

# Print regret summary
cat("Final simple regret:", regret[n_iter], "\n")

regret_df <- data.frame(
  Iteration = 1:n_iter,
  Regret = regret
)

print(regret_df)

# Plot regret curve
library(ggplot2)

ggplot(regret_df, aes(x = Iteration, y = Regret)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  theme_light() +
  labs(
    title = "MODM Simple Regret — Tanaka Problem",
    x = "Iteration",
    y = "Simple Regret"
  )

write.csv(regret_df, "modm_tanaka_regret.csv", row.names = FALSE)

cat("\nSaved: modm_tanaka_regret.csv\n")