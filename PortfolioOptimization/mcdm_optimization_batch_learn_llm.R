# ----------------------------------------------
# Author: Akanksha Mishra
# Date: 15 Jul 2025
# Batch Bayesian Learning for Portfolio Optimization
# ----------------------------------------------
set.seed(2025)
# ----------------------------------------------
# 1. Libraries
# ----------------------------------------------
library(readr)
library(mco)
library(ggplot2)
library(reshape2)
library(viridis)
library(rstan)
library(GA)
library(DirichletReg)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# ----------------------------------------------
# 2. Start Timer
# ----------------------------------------------
start_time <- Sys.time()

# ----------------------------------------------
# 3. Load Stock Data
# ----------------------------------------------
setwd("~/research/MODM/portfolio-optimization")
stock_prices <- read_csv("stocks.csv")
tickers <- colnames(stock_prices)

# ----------------------------------------------
# 4. Compute Returns
# ----------------------------------------------
returns <- as.data.frame(
  stock_prices[-1, ] / stock_prices[-nrow(stock_prices), ] - 1
)
returns <- returns[!apply(is.na(returns), 1, all), ]

# ----------------------------------------------
# 5. Annualized Mean Return & Covariance
# ----------------------------------------------
mu <- sapply(returns, function(r) {
  r <- r[!is.na(r)]
  prod(1 + r)^(252 / length(r)) - 1
})

cov_matrix <- cov(returns, use = "pairwise.complete.obs") * 252

# ----------------------------------------------
# 6. Objective Functions
# ----------------------------------------------
portfolio_return <- function(x, mu) {
  -sum(x * mu)
}

portfolio_risk <- function(x, cov_matrix) {
  sqrt(t(x) %*% cov_matrix %*% x)
}

# ----------------------------------------------
# 7. Load Candidate Portfolios
# ----------------------------------------------
dv <- read_csv("decision_variables.csv")
n_solutions <- nrow(dv)

o1_values <- numeric(n_solutions)
o2_values <- numeric(n_solutions)

for (i in 1:n_solutions) {
  x <- as.numeric(dv[i, ])
  o1_values[i] <- portfolio_return(x, mu)
  o2_values[i] <- portfolio_risk(x, cov_matrix)
}

# ----------------------------------------------
# 8. Normalize Objectives
# ----------------------------------------------
min_max_normalize <- function(val, min_val, max_val) {
  if (max_val == min_val) return(rep(0, length(val)))
  (val - min_val) / (max_val - min_val)
}

o1_min <- min(o1_values)
o1_max <- max(o1_values)
o2_min <- min(o2_values)
o2_max <- max(o2_values)

n_obj_1 <- min_max_normalize(o1_values, o1_min, o1_max)
n_obj_2 <- min_max_normalize(o2_values, o2_min, o2_max)

# ----------------------------------------------
# 9. Scalarization Setup
# ----------------------------------------------
epsilon <- 0.15
z_star <- c(max(n_obj_1), max(n_obj_2)) + epsilon
a <- 10
p <- 2

# ----------------------------------------------
# 10. Load ALL Preferences (Batch)
# ----------------------------------------------
df_pref <- read_csv("llm_preferences_all.csv")
df_pref <- df_pref[!is.na(df_pref$preference), ]

preferences <- df_pref$preference
index_i <- df_pref$portfolio_i + 1
index_j <- df_pref$portfolio_j + 1

# ----------------------------------------------
# 11. Load Stan Model
# ----------------------------------------------
compiled_model <- stan_model(
  "C:/Users/Akanksha Mishra/Documents/research/MODM/mcdm-bayes-main/R/tnz.stan")

# ----------------------------------------------
# 12. Batch Bayesian Inference (ONCE)
# ----------------------------------------------
fit_batch <- sampling(
  compiled_model,
  data = list(
    n_pairs = length(preferences),
    y = preferences,
    index_i = index_i,
    index_j = index_j,
    n_points = length(n_obj_1),
    obj_1 = n_obj_1,
    obj_2 = n_obj_2,
    eta = c(1, 1),
    a = a,
    p = p,
    z_star = z_star
  ),
  chains = 1,
  iter = 21000,
  warmup = 1000,
  seed = 2025,
  control = list(adapt_delta = 0.99)
)

print(fit_batch)

# ----------------------------------------------
# 13. Extract Final Weights
# ----------------------------------------------
samples <- rstan::extract(fit_batch)
w_batch_llm <- colMeans(samples$weights)

cat("Final inferred weights:\n")
print(w_batch_llm)

write.csv(
  data.frame(w1 = w_batch_llm[1], w2 = w_batch_llm[2]),
  "batch_llm_final_weights.csv",
  row.names = FALSE
)

# ----------------------------------------------
# 14. Batch GA Optimization (ONCE)
# ----------------------------------------------
min_vals <- apply(dv, 2, min)
max_vals <- apply(dv, 2, max)

fitness_batch <- function(x) {
  val <-
    w_batch_llm[1] *
    abs(z_star[1] - min_max_normalize(portfolio_return(x, mu), o1_min, o1_max))^p +
    w_batch_llm[2] *
    abs(z_star[2] - min_max_normalize(portfolio_risk(x, cov_matrix), o2_min, o2_max))^p
  -val
}

ga_batch <- ga(
  type = "real-valued",
  fitness = fitness_batch,
  lower = min_vals,
  upper = max_vals,
  popSize = 100,
  maxiter = 200,
  run = 100
)

# ----------------------------------------------
# 15. Store Final Solutions
# ----------------------------------------------
batch_llm_final_solutions <- ga_batch@solution
write.csv(
  batch_llm_final_solutions,
  "batch_llm_final_solutions.csv",
  row.names = FALSE
)

# ----------------------------------------------
# 16. Compute Final Objectives
# ----------------------------------------------
n_final <- nrow(batch_llm_final_solutions)

batch_llm_final_objectives <- data.frame(
  NegReturn = numeric(n_final),
  Risk = numeric(n_final)
)

for (i in 1:n_final) {
  x <- as.numeric(batch_llm_final_solutions[i, ])
  batch_llm_final_objectives$NegReturn[i] <- portfolio_return(x, mu)
  batch_llm_final_objectives$Risk[i] <- portfolio_risk(x, cov_matrix)
}

write.csv(
  batch_llm_final_objectives,
  "batch_llm_final_objectives.csv",
  row.names = FALSE
)

# ----------------------------------------------
# 17. End Timer
# ----------------------------------------------
end_time <- Sys.time()
execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

write.csv(
  data.frame(Execution_Time_Seconds = execution_time),
  "batch_llm_execution_time.csv",
  row.names = FALSE
)

cat("✅ Batch learning completed\n")
cat("⏱ Execution time (seconds):", execution_time, "\n")

# ----------------------------------------------
# 18. Save Workspace
# ----------------------------------------------
save.image("po-batch-llm-results-w55.RData")