# ----------------------------------------------
# Author: Akanksha Mishra
# Date: 15 Jul 2025
# Random generate preferences
# ----------------------------------------------
# install.packages(c("readr", "mco", "ggplot2", "reshape2", "viridis"))
library(readr)
library(mco)
library(ggplot2)
library(reshape2)
library(viridis)

# 2. Load Stock Price Data
setwd("~/research/MODM/portfolio-optimization")
stock_prices <- read_csv("stocks.csv")
# Reattach column names if needed (since index=False in CSV)
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

# # 5. Eigenvalue Clipping (covariance cleaning)
# eig <- eigen(cov_matrix)
# eig$values <- pmax(eig$values, 0)
# cov_matrix <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)

# 6. Plot Expected Annual Return per Stock
mu_df <- data.frame(Stock = tickers, Return = mu)

ggplot(mu_df, aes(x = Stock, y = Return)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "", y = "Expected Annual Return", x = "")

# 7. Plot Correlation Matrix
corr <- cor(returns)
corr_melted <- melt(corr)
colnames(corr_melted) <- c("Var1", "Var2", "Correlation")

p <- ggplot(corr_melted, aes(Var1, Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "C", limits = c(-1, 1)) +
  coord_fixed() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  labs(title = "", x = "", y = "")
p
# ggsave("corr-mat.pdf", plot = p, width = 6, height = 6)

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

plot(n_obj_1, n_obj_2,
     xlab = "Negative Return", ylab = "Risk",
     main = "Normalized objective space", pch = 19, col = "blue")

# 11. Scalarization
epsilon <- 0.15
z_star <- c(max(n_obj_1), max(n_obj_2)) + epsilon

scalarization <- function(w, x, p = 2) {
  sum((w * abs(z_star - c(
    min_max_normalize(portfolio_return(x, mu), o1_min, o1_max),
    min_max_normalize(portfolio_risk(x, cov_matrix), o2_min, o2_max)
  )))^p)
}

exp_scalarization <- function(w, x, a = 40, p = 2) {
  exp(-a * scalarization(w, x, p))
}

eval_prob_exp <- function(w, x_i, x_j, a = 40, p = 2) {
  s1 <- exp_scalarization(w, x_i, a, p)
  s2 <- exp_scalarization(w, x_j, a, p)
  s1 / (s1 + s2)
}

# 12. Generate Pairwise Preferences
true_weights <- c(0.5, 0.5)
a <- 40 #10 for w2
p <- 2

pairs <- combn(n_solutions, 2)
# Sample random pairs (columns)
selected_cols <- sample(ncol(pairs), 4000)
selected_pairs <- pairs[, selected_cols]

probs <- preferences <- index_i <- index_j <- c()

for (k in 1:ncol(selected_pairs)) {
  i <- selected_pairs[1, k]
  j <- selected_pairs[2, k]
  
  x_i <- as.numeric(dv[i, ])
  x_j <- as.numeric(dv[j, ])
  
  p_ij <- eval_prob_exp(true_weights, x_i, x_j, a, p)
  probs <- c(probs, p_ij)
  preferences <- c(preferences, rbinom(1, 1, p_ij))
  index_i <- c(index_i, i)
  index_j <- c(index_j, j)
}


# 13. Load and Fit Stan Model

library(rstan)
options(mc.cores = parallel::detectCores())

compiled_model <- stan_model("C:/Users/Akanksha Mishra/Documents/research/MODM/mcdm-bayes-main/R/tnz.stan")

fit <- sampling(
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
    z_star = z_star,
    control = list(adapt_delta=0.99)
  ),
  chains = 1,
  seed = 2020,
  iter = 21000,
  warmup = 1000
)
print(fit)

# 14. Parameter Extraction and Updates
set.seed(2025)

# Store weights and alpha estimates per iteration
weights_updates <- vector()
alpha_updates <- vector()
ga_solutions <- vector("list", 1)
posterior_samples_list <- list()
# Store final solutions and timing
final_solutions <- vector("list", 8)
final_objectives <- matrix(NA, nrow = 8, ncol = 2)
colnames(final_objectives) <- c("NegReturn", "Risk")
iteration_time <- numeric(8)


alpha_t = c(1, 1)
library(DirichletReg)
weights_t = rdirichlet(1, alpha = alpha_t)
# Moment-matching Dirichlet estimator
estimate_dirichlet_mm <- function(X) {
  N <- nrow(X)
  K <- ncol(X)
  log_p <- log(X+ 1e-10)
  mean_log_p <- colMeans(log_p)
  mean_p <- colMeans(X)
  s <- log(mean_p) - mean_log_p
  alpha0_init <- (K - 1) / sum(s)
  alpha <- alpha0_init * mean_p
  return(alpha)
}
min_vals <- apply(dv, 2, min, na.rm = TRUE)
max_vals <- apply(dv, 2, max, na.rm = TRUE)

library(GA)
for (t in 1:8) {
  cat("📌 Iteration:", t, "\n")
  iter_start_time <- Sys.time()

  weights_updates <- rbind(weights_updates, weights_t)
  alpha_updates <- rbind(alpha_updates, alpha_t)

  sff <- function(x) {
    ss <- (weights_t[1] * abs(z_star[1] - min_max_normalize(portfolio_return(x, mu), o1_min, o1_max)) ^ p +
             weights_t[2] * abs(z_star[2] - min_max_normalize(portfolio_risk(x, cov_matrix), o2_min, o2_max)) ^ p)
    -ss
  }

  # ---- GA-Based Preference Sampling ----
  population_history <- list()
  fitness_history <- list()
  monitor_func <- function(obj) {
    population_history <<- append(population_history, list(obj@population))
    fitness_history <<- append(fitness_history, list(obj@fitness))
  }

  ga_result <- ga(
    type = "real-valued",
    fitness = sff,
    lower = min_vals,
    upper = max_vals,
    popSize = 100,
    maxiter = 100,
    run = 100,
    monitor = monitor_func
  )
  
  ga_solutions[[t]] <- ga_result@solution[,1:14]

  # ---- Select Diverse Solutions ----
  selected_iters <- c(2, 3, 10, 50, 99)
  populations <- do.call(rbind, lapply(selected_iters, function(iter) {
    fsort <- sort.int(fitness_history[[iter]], index.return = TRUE)
    selected <- c(fsort$ix[1:2], fsort$ix[length(fsort$x)])
    population_history[[iter]][selected, ]
  }))

  # ---- Generate Pairwise Preferences ----
  x_vals <- populations[1, ]
  y_vals <- populations[2, ]
  n_solutions <- nrow(populations)
  pair_indices <- combn(n_solutions, 2)
  index_i <- pair_indices[1, ]
  index_j <- pair_indices[2, ]

  preferences <- mapply(function(i, j) {
    x_i <- populations[i, ]
    x_j <- populations[j, ]
    prob <- eval_prob_exp(true_weights, x_i, x_j, a, p)
    rbinom(1, 1, prob)
  }, index_i, index_j)

  # ---- Re-evaluate Objectives for Selected Solutions ----
  o1_vals <- o2_vals <- numeric(n_solutions)
  for (i in 1:n_solutions) {
    x <- populations[i, ]
    o1_vals[i] <- portfolio_return(x, mu)
    o2_vals[i] <- portfolio_risk(x, cov_matrix)
  }

  n_obj_1 <- min_max_normalize(o1_vals, o1_min, o1_max)
  n_obj_2 <- min_max_normalize(o2_vals, o2_min, o2_max)

  plot(n_obj_1, n_obj_2,
       main = paste("OS Populations - Generation", t),
       xlab = "Negative Return", ylab = "Risk",
       pch = 19, col = "darkred")

  # ---- Bayesian Inference with Stan ----
  fit <- sampling(
    compiled_model,
    data = list(
      n_pairs = length(preferences),
      y = preferences,
      index_i = index_i,
      index_j = index_j,
      n_points = n_solutions,
      obj_1 = n_obj_1,
      obj_2 = n_obj_2,
      eta = alpha_t,
      a = a,
      p = p,
      z_star = z_star
    ),
    # init = 0,
    chains = 1,
    iter = 10000,
    warmup = 1000,
    seed = 2020,
    control = list(adapt_delta=0.99, max_treedepth = 15)
  )
  print(fit)
  divergent <- get_sampler_params(fit, inc_warmup = FALSE)[[1]][,"divergent__"]
  cat("Number of divergent transitions:", sum(divergent), "\n")

  posterior_samples_list[[t]] <- rstan::extract(fit)  # if you prefer a list of arrays

  # ---- Update Estimates ----
  samples_df <- as.data.frame(rstan::extract(fit, permuted = FALSE)[, 1, ])
  est_weights <- data.frame(
    Iteration = 1:nrow(samples_df),
    w1 = samples_df$`weights[1]`,
    w2 = samples_df$`weights[2]`
  )

  weights_mat <- as.matrix(est_weights[, c("w1", "w2")])
  weights_t <- colMeans(weights_mat)
  alpha_t <- estimate_dirichlet_mm(weights_mat)

  print(weights_t)
  print(alpha_t)
  
  posterior_sff <- function(x) {
    ss <- (weights_t[1] * abs(z_star[1] - 
                                min_max_normalize(portfolio_return(x, mu), o1_min, o1_max))^p +
             weights_t[2] * abs(z_star[2] - 
                                  min_max_normalize(portfolio_risk(x, cov_matrix), o2_min, o2_max))^p)
    -ss
  }
  
  ga_final <- ga(
    type = "real-valued",
    fitness = posterior_sff,
    lower = min_vals,
    upper = max_vals,
    popSize = 200,
    maxiter = 200,
    run = 100
  )
  
  x_star <- as.numeric(ga_final@solution[1, ])
  final_solutions[[t]] <- x_star
  final_objectives[t, 1] <- portfolio_return(x_star, mu)
  final_objectives[t, 2] <- portfolio_risk(x_star, cov_matrix)
  iteration_time[t] <- as.numeric(
    difftime(Sys.time(), iter_start_time, units = "secs")
  )
}

# 15. Plot Pareto front
df_base <- data.frame(NegReturn = o1_values, Risk = o2_values, Source = "Pareto-optimal front")

# Initialize overlay dataframe
df_overlay <- data.frame()

# Evaluate objectives for each iteration
for (t in seq_along(ga_solutions)) {
  solutions <- ga_solutions[[t]]
  
  # Ensure matrix format
  if (is.null(nrow(solutions))) {
    solutions <- matrix(solutions, nrow = 1)
  }
  
  for (i in 1:nrow(solutions)) {
    x <- as.numeric(solutions[i, ])
    o1 <- portfolio_return(x, mu)
    o2 <- portfolio_risk(x, cov_matrix)
    
    df_overlay <- rbind(df_overlay, data.frame(
      NegReturn = o1,
      Risk = o2,
      Iteration = t
    ))
  }
}

label_data <- subset(df_overlay, Iteration < 8)
star_data <- subset(df_overlay, Iteration == 8)

p <- ggplot() +
  geom_point(data = df_base, 
             aes(x = NegReturn, y = Risk, shape = Source), 
             color = "grey40", size = 1.5, stroke = 0.6) +   # grey cross with legend
  geom_text(data = label_data, 
            aes(x = NegReturn, y = Risk, label = Iteration), 
            color = "red", size = 4, vjust = -1) +           # iteration numbers
  geom_point(data = star_data, 
             aes(x = NegReturn, y = Risk), 
             shape = 8, color = "red", size = 4) +           # red star
  scale_shape_manual(values = c("Pareto-optimal front" = 4)) +
  guides(shape = guide_legend(override.aes = list(color = "grey40"))) +
  labs(x = "Negative Return", y = "Risk", shape = "", title = "") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", legend.justification = "right")
p
# ggsave("pareto_front_bayesian_iter.pdf", plot = p, width = 8, height = 6)

# 16. Post processing

# Combine and print weight + alpha summary
wa <- cbind(weights_updates, alpha_updates)
rownames(wa) <- 0:(nrow(wa) - 1) #Modified: 7th May 2025
colnames(wa) <- c("w1", "w2", "alpha1", "alpha2")

require(xtable)
xt <- xtable(wa)
digits(xt) <- xdigits(xt, zap = 3)
print(xt)

# 17. Plot weight updates
require(tidyverse)
tt <-
  tibble(
    Iterations = 0:(nrow(weights_updates)-1),
    w1 = weights_updates[, 1],
    w2 = weights_updates[, 2]

  )

weights_updates_long <-
  gather(tt,
         key = "Index",
         value = "Weights",
         w1,
         w2
  )

legend_labels <-
  c(
    expression("w"[1]),
    expression("w"[2])
  )
gp <-
  ggplot(data = weights_updates_long,
         aes(
           x = Iterations,
           y = Weights,
           color = Index,
           linetype = Index
         )) +
  geom_line(size = 1.1) +
  geom_point(size = 4, shape = 4) +
  scale_y_continuous(breaks = seq(0, 1, .1)) +
  scale_x_continuous(breaks = seq(0, 7, 1)) +
  geom_hline(
    data = data.frame(type = factor(c("w1", "w2")), true_weights = true_weights),
    aes(
      yintercept = true_weights,
      color = type,
      linetype = type
    )
  ) +
  theme_light() + # scale_color_grey() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 16, colour = "black"),
    legend.title = element_blank(),
    axis.text.x = element_text(size = 16, colour = "black"),
    axis.text.y = element_text(size = 16, colour = "black"),
    axis.title.y = element_text(size = 20, margin = margin(
      l = 0,
      r = 5,
      t = 0,
      b = 0
    )),
    axis.title.x = element_text(size = 20, margin = margin(
      l = 0,
      r = 0,
      t = 15,
      b = 0
    ))
  ) +
  scale_color_discrete(labels = legend_labels) +
  scale_linetype_discrete(labels = legend_labels)
gp
# ggsave(
#   filename = paste("po-Bayesian-weights-updates-w2-new.pdf", sep = ""),
#   plot = gp,
#   width = 4,
#   height = 3,
#   units = "in"
# )

# 18. Credible Interval and density plots

samples <- rstan::extract(fit)
estimated_weights <- colMeans(samples$weights)
cat("✅ Estimated weights:", estimated_weights, "\n")

# Summary
print(summary(fit, pars = "weights")$summary)

# 0.95 credible interval
weights_samples <- samples$weights  # matrix with shape [num_samples, 2]

# Compute posterior means
mean_w1 <- mean(weights_samples[, 1])
mean_w2 <- mean(weights_samples[, 2])

# Compute 95% credible intervals
ci_w1 <- quantile(weights_samples[, 1], c(0.025, 0.975))
ci_w2 <- quantile(weights_samples[, 2], c(0.025, 0.975))

# Combine into a table
results_table <- data.frame(
  w1 = sprintf("%.2f (%.2f--%.2f)", mean_w1, ci_w1[1], ci_w1[2]),
  w2 = sprintf("%.2f (%.2f--%.2f)", mean_w2, ci_w2[1], ci_w2[2]),
  n = ncol(selected_pairs)
)

# Print table
print(results_table)


# Density plots
library(ggplot2)
library(tidyr)
library(dplyr)

# Convert to data frame
df <- as.data.frame(weights_samples)
colnames(df) <- c("w1", "w2")

# Pivot to long format for ggplot
df_long <- df %>%
  pivot_longer(cols = everything(), names_to = "Weight", values_to = "Value")

# True weights
true_weights <- c(w1 = 0.7, w2 = 0.3)

# Line type mapping
line_types <- c(w1 = "solid", w2 = "dashed")

# Colors
fill_colors <- c(w1 = "#298c8c", w2 = "#1a80bb")

# Custom labels for legend
custom_labels <- c(w1 = expression(w[1]), w2 = expression(w[2]))

# Plot
gp <- ggplot(df_long, aes(x = Value, color = Weight, linetype = Weight)) +
  geom_density(size = 1.2) +
  geom_vline(data = data.frame(Weight = names(true_weights), Value = true_weights),
             aes(xintercept = Value, color = Weight),
             linetype = "dashed", size = 1) +
  scale_color_manual(values = fill_colors, labels = custom_labels) +
  scale_linetype_manual(values = line_types, labels = custom_labels) +
  scale_x_continuous(breaks = seq(0.0, 0.9, 0.1), limits = c(0, 1)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14)
  ) +
  labs(
    x = "Weights",
    y = "density"
  )

# Show plot
print(gp)

# Save to PDF for publication
# ggsave("density_weights_w2_a10_n4000.pdf", plot = gp, width = 8, height = 5)
save.image(file = "po-bcl-sp-results-w55.RData")