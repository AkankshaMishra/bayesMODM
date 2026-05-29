# ----------------------------------------------
# Author: Akanksha Mishra
# Date: 9 Feb 2026
# MLE for PO using Random generate preferences
# ----------------------------------------------
############################################################
## Frequentist BTL MLE for Portfolio Scalarization Weights
############################################################

library(readr)
library(mco)
library(ggplot2)
library(reshape2)
library(viridis)
# 2. Load Stock Price Data
setwd("~/research/MODM/portfolio-optimization")
stock_prices <- read_csv("stocks.csv")
tickers <- colnames(stock_prices)

# 3. Compute Daily Returns
returns <- as.data.frame(stock_prices[-1, ] / stock_prices[-nrow(stock_prices), ] - 1)
returns <- returns[!apply(is.na(returns), 1, all), ]

# 4. Annualized geometric mean return (CAGR) and Covariance (cov)
mu <- sapply(returns, function(r) {
  r <- r[!is.na(r)]  # mimic returns.count() in Python
  prod(1 + r)^(252 / length(r)) - 1
})
cov_matrix <- cov(returns, use = "pairwise.complete.obs") * 252

# 6. Plot Expected Annual Return per Stock
mu_df <- data.frame(Stock = tickers, Return = mu)
corr <- cor(returns)
corr_melted <- melt(corr)
colnames(corr_melted) <- c("Var1", "Var2", "Correlation")
# 8. Define Objective Function
portfolio_return <- function(x, mu) {
  expected_return <- sum(x * mu)
  return(-expected_return)
}
portfolio_risk <- function(x, cov_matrix) {
  # x <- x / sum(x)  # ensure weights sum to 1
  expected_risk <- sqrt(t(x) %*% cov_matrix %*% x)
  return(expected_risk)
}

# 9. Evaluate Objectives
dv <- read_csv("decision_variables.csv")
n_solutions <- nrow(dv)
o1_values <- numeric(n_solutions)
o2_values <- numeric(n_solutions)

for (i in 1:n_solutions) {
  x <- as.numeric(dv[i, ])
  o1_values[i] <- portfolio_return(x, mu)
  o2_values[i] <- portfolio_risk(x, cov_matrix)
}

plot(o1_values, o2_values,
     xlab = "Negative Return", ylab = "Risk",
     main = "", pch = 19, col = "blue")

# 10. Normalize Objectives
min_max_normalize <- function(val, min_val, max_val) {
  if (max_val == min_val) return(rep(0, length(val)))  # avoid division by zero
  (val - min_val) / (max_val - min_val)
}

o1_min <- min(o1_values, na.rm = TRUE)
o1_max <- max(o1_values, na.rm = TRUE)
o2_min <- min(o2_values, na.rm = TRUE)
o2_max <- max(o2_values, na.rm = TRUE)

n_obj_1 <- min_max_normalize(o1_values, o1_min, o1_max)
n_obj_2 <- min_max_normalize(o2_values, o2_min, o2_max)

epsilon <- 0.15
z_star <- c(max(n_obj_1), max(n_obj_2)) + epsilon

library(numDeriv)

K <- 2
n_pais <- combn(n_solutions, 2)
# n_pairs <- length(preferences)
## ---------------------------------------------------------
## 1. Construct tilde f_r(x) = |f_r(x) - z_r^*|^p
## ---------------------------------------------------------

tilde_f <- cbind(
  abs(n_obj_1 - z_star[1])^p,
  abs(n_obj_2 - z_star[2])^p
)

## Pairwise differences
F_diff <- matrix(0, nrow = n_pairs, ncol = K)

for (k in seq_len(n_pairs)) {
  i <- index_i[k]
  j <- index_j[k]
  F_diff[k, ] <- tilde_f[i, ] - tilde_f[j, ]
}

## ---------------------------------------------------------
## 2. Softmax parameterization (simplex constraint)
## ---------------------------------------------------------

softmax <- function(theta) {
  exp_theta <- exp(theta - max(theta))
  exp_theta / sum(exp_theta)
}

## ---------------------------------------------------------
## 3. Log-likelihood (BTL logistic form)
## ---------------------------------------------------------

loglik_btl <- function(theta, y, F_diff, a) {
  w <- softmax(theta)
  delta <- -as.vector(F_diff %*% w)
  eta <- a * delta
  
  sum(y * eta - log1p(exp(eta)))
}

## ---------------------------------------------------------
## 4. Gradient
## ---------------------------------------------------------

grad_btl <- function(theta, y, F_diff, a) {
  w <- softmax(theta)
  delta <- -as.vector(F_diff %*% w)
  p_ij <- plogis(a * delta)
  
  residual <- y - p_ij
  
  grad_w <- a * colSums(F_diff * residual)
  
  J <- diag(w) - tcrossprod(w)
  
  as.vector(J %*% grad_w)
}

## ---------------------------------------------------------
## 5. Optimize (MLE)
## ---------------------------------------------------------

theta_init <- c(0.5, 0.5)

opt <- optim(
  par = theta_init,
  fn  = function(th) -loglik_btl(th, preferences, F_diff, a),
  gr  = function(th) -grad_btl(th, preferences, F_diff, a),
  method = "L-BFGS-B",
  control = list(maxit = 100000, factr = 1e9)
)
print(opt$convergence)

if (opt$convergence != 0) {
  warning("Optimization may not have converged.")
}

w_hat <- softmax(opt$par)

cat("\n=== Portfolio BTL MLE Results ===\n")
print(w_hat)

## ---------------------------------------------------------
## 6. Fisher information → standard errors
## ---------------------------------------------------------

H_theta <- hessian(
  func = function(th) -loglik_btl(th, preferences, F_diff, a),
  x = opt$par
)

# cov_theta <- solve(H_theta)
cov_theta <- solve(H_theta + 1e-6 * diag(2))

J <- diag(w_hat) - tcrossprod(w_hat)
cov_w <- J %*% cov_theta %*% t(J)

se_w <- sqrt(diag(cov_w))

cat("\nStandard errors:\n")
print(se_w)
