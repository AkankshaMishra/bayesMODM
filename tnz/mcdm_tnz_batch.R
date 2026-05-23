# Author: Akanksha Mishra
# Date: 21 Feb 2026
# MODM Tanaka Batch learning
# these are the definition of two objectives i.e obj_1 and obj_2
obj_1 <- function(x) {
  x[1]
}
obj_2 <- function(x) {
  x[2]
}

# Sets the working directory and loads feasible points 
load("C:/Users/Akanksha Mishra/Documents/research/MODM/mcdm-bayes-main/R/tnz-feasible-points.rda")
n_solutions <- 50
x_values <- x_tnz_100[1:n_solutions]
y_values <- y_tnz_100[1:n_solutions]

# Evaluate objectives for each point
o1_values <- x_values
o2_values <- y_values

obj_1_min <- min(o1_values); obj_1_max <- max(o1_values)
obj_2_min <- min(o2_values); obj_2_max <- max(o2_values)

min_max_normalize <- function(val, min_val, max_val) {
  (val - min_val) / (max_val - min_val)
}

# Ideal point
n_obj_1 <- min_max_normalize(o1_values, obj_1_min, obj_1_max)
n_obj_2 <- min_max_normalize(o2_values, obj_2_min, obj_2_max)
z_star <- c(min(n_obj_1), min(n_obj_2))

# ------------------------------------------------------------
# Scalarization & preference model
# ------------------------------------------------------------
scalarization <- function(w, x, p = 2) {
  w[1] * abs(z_star[1] - min_max_normalize(obj_1(x), obj_1_min, obj_1_max))^p +
    w[2] * abs(z_star[2] - min_max_normalize(obj_2(x), obj_2_min, obj_2_max))^p
}

exp_scalarization <- function(w, x, a = 40, p = 2) {
  exp(-a * scalarization(w, x, p))
}

eval_prob_exp <- function(w, x_i, x_j, a = 40, p = 2) {
  s1 <- exp_scalarization(w, x_i, a, p)
  s2 <- exp_scalarization(w, x_j, a, p)
  s1 / (s1 + s2)
}

# ------------------------------------------------------------
# True parameters
# ------------------------------------------------------------
true_weights <- c(0.6, 0.4)
a <- 10
p <- 2

# ------------------------------------------------------------
# Collect ALL preference data (batch)
# ------------------------------------------------------------
library(GA)
set.seed(2021)

all_preferences <- c()
all_index_i <- c()
all_index_j <- c()
all_obj1 <- c()
all_obj2 <- c()

global_index_offset <- 0

# ------------------------------------------------------------
# Storage (batch)
# ------------------------------------------------------------
batch_final_solutions <- vector("list", length = 8)
batch_final_objectives <- vector("list", length = 8)

for (t in 1:8) {
  
  cat("📌 Batch iteration:", t, "\n")
  
  # ---- GA objective (fixed unknown weights) ----
  # x1 <- x2 <- seq(0, 3.14, by = 0.08)
  #adding constraints
  c1 <- function(x){
    -x[1]^2-x[2]^2+1+0.1*cos(16*atan(x[1]/x[2]))
  }
  
  c2 <- function(x){
    (x[1]-0.5)^2+(x[2]-0.5)^2-0.5
  }
  
  sff <- function(x) {
    ss <- scalarization(true_weights, x, p)
    ss<- -ss
    pen<-sqrt(.Machine$double.xmax)
    penalty1 <- max(c1(x),0)*pen
    penalty2 <- max(c2(x),0)*pen
    ss-penalty1-penalty2
  }
  
  g <- ga(
    type = "real-valued",
    fitness = sff,
    lower = c(0, 0),
    upper = c(3.14, 3.14),
    popSize = 100,
    maxiter = 100,
    run = 100
  )
  
  # -----------------------------
  # Collect population for prefs
  # -----------------------------
  pop <- g@population
  x_vals <- pop[,1]
  y_vals <- pop[,2]
  n_pts <- nrow(pop)
  
  o1 <- min_max_normalize(x_vals, obj_1_min, obj_1_max)
  o2 <- min_max_normalize(y_vals, obj_2_min, obj_2_max)
  
  # Pairwise comparisons
  cc <- combn(n_pts, 2)
  prefs <- numeric(ncol(cc))

  for (r in seq_len(ncol(cc))) {
    xi <- pop[cc[1, r], ]
    xj <- pop[cc[2, r], ]
    pij <- eval_prob_exp(true_weights, xi, xj, a, p)
    prefs[r] <- rbinom(1, 1, pij)
  }

  # Store globally
  all_preferences <- c(all_preferences, prefs)
  all_index_i <- c(all_index_i, cc[1,] + global_index_offset)
  all_index_j <- c(all_index_j, cc[2,] + global_index_offset)
  all_obj1 <- c(all_obj1, o1)
  all_obj2 <- c(all_obj2, o2)

  global_index_offset <- global_index_offset + n_pts
}
# ------------------------------------------------------------
# Batch Bayesian inference (ONE Stan call)
# ------------------------------------------------------------
library(rstan)
compiled_model <- stan_model(
  "C:/Users/Akanksha Mishra/Documents/research/MODM/mcdm-bayes-main/R/tnz.stan"
)

alpha_0 <- c(1, 1)  # fixed prior

fit_batch <- sampling(
  compiled_model,
  data = list(
    n_pairs  = length(all_preferences),
    y        = all_preferences,
    index_i = all_index_i,
    index_j = all_index_j,
    n_points = length(all_obj1),
    obj_1   = all_obj1,
    obj_2   = all_obj2,
    eta     = alpha_0,
    a       = a,
    p       = p,
    z_star  = z_star
  ),
  chains = 1,
  iter = 21000,
  warmup = 2000,
  seed = 2020
)

print(fit_batch)

estimate_dirichlet_mm <- function(X) {
  # X: matrix of samples (rows are samples, columns are components)
  N <- nrow(X)
  K <- ncol(X)
  
  log_p <- log(X)
  mean_log_p <- colMeans(log_p)
  mean_p <- colMeans(X)
  
  # Estimate alpha0 using method-of-moments
  psi_inv <- function(x) {
    # Inverse of digamma using Newton-Raphson
    if (x >= -2.22) {
      return(exp(x) + 0.5)
    } else {
      return(-1 / (x - digamma(1)))
    }
  }
  
  s <- log(mean_p) - mean_log_p
  alpha0_init <- (K - 1) / sum(s)
  alpha <- alpha0_init * mean_p
  return(alpha)
}

# ------------------------------------------------------------
# FINAL BATCH DECISION (ONE SOLUTION)
# ------------------------------------------------------------
w_batch <- summary(fit_batch, pars = "weights")$summary[, "mean"] #posterior means

posterior_samples <- rstan::extract(fit_batch, pars = "weights")$weights

alpha_0 <- estimate_dirichlet_mm(posterior_samples)
print(alpha_0)

batch_decision_start <- Sys.time()

sff_batch <- function(x) {
  ss <- scalarization(w_batch, x, p)
  ss <- -ss
  pen <- sqrt(.Machine$double.xmax)
  penalty1 <- max(c1(x), 0) * pen
  penalty2 <- max(c2(x), 0) * pen
  ss - penalty1 - penalty2
}

g_batch_final <- ga(
  type = "real-valued",
  fitness = sff_batch,
  lower = c(0, 0),
  upper = c(3.14, 3.14),
  popSize = 100,
  maxiter = 200,
  run = 100
)

batch_final_solution <- as.numeric(g_batch_final@solution[1, 1:2])

batch_final_objectives <- c(
  obj_1 = obj_1(batch_final_solution),
  obj_2 = obj_2(batch_final_solution)
)

batch_decision_end <- Sys.time()

batch_decision_time <-
  as.numeric(difftime(batch_decision_end, batch_decision_start, units = "secs"))

# save.image(file = "tnz-batch-results.RData")