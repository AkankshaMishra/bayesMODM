# Author: Akanksha Mishra
# Date: 21 Feb 2026
# MODM Tanaka BCL
# these are the definition of two objectives i.e obj_1 and obj_2
obj_1 <- function(x) {
  x[1]
}
obj_2 <- function(x) {
  x[2]
  
}

# Sets the working directory and loads feasible points 
load("C:/Users/Akanksha Mishra/Documents/research/MODM/mcdm-bayes-main/R/tnz-feasible-points.rda")
n_solutons <- 20
x_values <- x_tnz_100[1:n_solutons]
y_values <- y_tnz_100[1:n_solutons]

# Evaluate objectives for each point
o1_values <- c()
o2_values <- c()
for (i in 1:n_solutons){
  o1_values <- c(o1_values, obj_1(c(x_values[i], y_values[i])))
  o2_values <- c(o2_values, obj_2(c(x_values[i], y_values[i])))
  
}

# Get min and max of each objective
obj_1_max <- max(o1_values)
obj_1_min <- min(o1_values)
obj_2_max <- max(o2_values)
obj_2_min <- min(o2_values)

# Normalization Function
min_max_normalize <- function(val, min_val, max_val) {
  if(abs(max_val - min_val) < 1e-12) return(0.5)
  (val - min_val) / (max_val - min_val)
}

# Normalize objective values
n_obj_1 <- min_max_normalize(o1_values, obj_1_min, obj_1_max)
n_obj_2 <- min_max_normalize(o2_values, obj_2_min, obj_2_max)


epsilon <- 0.15
z_star <- c(min(n_obj_1), min(n_obj_2))  # ideal point which is taken as max(values) + epsilon

# data generative hierarchy # continued from 1st march from there

# Scalarization functions
scalarization <- function(w, x, p = 2) {
  (w[1] * abs(z_star[1] - min_max_normalize(obj_1(x), obj_1_min, obj_1_max)) ^ p + 
     w[2] * abs(z_star[2] - min_max_normalize(obj_2(x), obj_2_min, obj_2_max)) ^ p ) 
  
}

exp_scalarization <- function(weights, x, a = 40, p = 2) {exp(-a*scalarization(weights, x, p))}

# Compute probabilities 
eval_prob_exp <- function(weights, x_i, x_j, a = 40, p = 2) {
  s1 <- exp_scalarization(weights, x_i, a, p) 
  s2 <- exp_scalarization(weights, x_j, a, p) 
  # s1 / (s1 + s2)
  denom <- s1 + s2
  
  if(!is.finite(denom) || denom == 0) return(0.5)
  
  s1 / denom
}

## A example ----- 
true_weights <- c(.85, .15)
a <- 8
p <- 10

library(rstan)
library(rstantools)
compiled_model <- stan_model("C:/Users/Akanksha Mishra/Documents/research/MODM/mcdm-bayes-main/R/tnz-inf.stan")

## Bayesian continual learning -------

require(DirichletReg)
require(GA)
require(lattice)

set.seed(2021)

weights_updates <- vector()
alpha_updates <- vector()
ga_solutions <- vector("list", 1)

alpha_t = c(1, 1)
weights_t = rdirichlet(1, alpha = alpha_t)

#Modified: 7th May 2025 -- Define moment matching Dirichlet estimator
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

# Storage containers
final_solutions <- vector("list", length = 8)   # final GA solution per iteration
final_objectives <- vector("list", length = 8)  # obj1, obj2 of final solution
iteration_time <- numeric(8)                    # execution time per iteration

for (t in 1:8) {
  
  cat("📌 Iteration:", t, "\n")
  iter_start <- Sys.time()
  
  weights_updates <- rbind(weights_updates, weights_t)
  alpha_updates <- rbind(alpha_updates, alpha_t)

  ## GA ------
  
  x1 <- x2 <- seq(0, 3.14, by = 0.08)
  #adding constraints
  c1 <- function(x){
    -x[1]^2-x[2]^2+1+0.1*cos(16*atan(x[1]/x[2]))
  }
  
  c2 <- function(x){
    (x[1]-0.5)^2+(x[2]-0.5)^2-0.5
  }
  
  sff <- function(x) {
    ss <- (weights_t[1] * abs(z_star[1] - min_max_normalize(obj_1(x), obj_1_min, obj_1_max)) ^ p + 
             weights_t[2] * abs(z_star[2] - min_max_normalize(obj_2(x), obj_2_min, obj_2_max)) ^ p)  
    
    ss<- -ss
    pen<-sqrt(.Machine$double.xmax)
    penalty1 <- max(c1(x),0)*pen
    penalty2 <- max(c2(x),0)*pen
    ss-penalty1-penalty2
  }
  x <- c(x1[1], x2[1])
  sff(x)
  
  sf <- function(x1, x2) {
    val <- rep(0, length(x1))
    for (i in 1:length(x1)) {
      val[i] <- sff(c(x1[i], x2[i]))
    }
    val 
  }
  f <- outer(x1, x2, sf)
  
  margin = c(5 - 4, 4 - 3, 4 - 4, 2 - 2) # c(bottom, left, top, right)
  cex.axis = 1.2
  cex.lab = 1.4
  cex = 1
  plot.file <- paste("tnz-scalarization-function-iter", t, sep = "")
  op <- par(bg = "white")
  trellis.device(
    pdf, file = paste(plot.file, ".pdf", sep = ""), height = 5.5,
    width = 7, title = "", onefile = T
  )
  par(mar = margin + .1) # c(bottom, left, top, right)
  persp3D(
    x1,
    x2,
    f,
    cex.axis = cex.axis,
    cex.lab = cex.lab,
    cex = cex,
    theta = 60,
    phi = 35,
    col.palette = bl2gr.colors,
    xlab = "\n (x_1)",
    ylab = "\n (x_2)",
    zlab = "\n\n-Scalarization\n (Eq. 3)"
  )
  par(op)
  dev.off()
  
  ## Method 1 -------
  popualtion_history <- list()
  fitness_history <- list()
  mf <- function(obj){
    # assign value to global variable so use <<-
    popualtion_history <<- append(popualtion_history, list(obj@population))
    fitness_history <<- append(fitness_history, list(obj@fitness))
  }
  g <-
    ga(
      type = "real-valued",
      fitness = sff,
      lower = c(0, 0),
      upper = c(3.14, 3.14),
      popSize = 100,
      maxiter = 100,
      run = 100,
      monitor = mf
    )
  ga_solutions[[t]] <- g@solution[,1:2]
  
  populations <- vector()
  for (iter in c(2, 3, 10, 50, 99)) { # sample.int(100, 5)
    fsort <- sort.int(fitness_history[[iter]], index.return = T)
    sel_index <- c(fsort$ix[1:2], fsort$ix[length(fitness_history[[iter]])])
    populations <- rbind(populations, popualtion_history[[iter]][sel_index,])
  }
  
  x_values <- populations[,1]
  y_values <- populations[,2]
  n_solutons <- length(x_values)
  cc2 <- combn(n_solutons, 2) # sample.int(n_solutons, 10)
  index_i <- cc2[1,]
  index_j <- cc2[2,]
  preferences <- rep(0, length(index_i))
  for (r in 1:ncol(cc2)) {
    x_i <- c(x_values[index_i[r]], y_values[index_i[r]])
    x_j <- c(x_values[index_j[r]], y_values[index_j[r]])
    p_i_j <- eval_prob_exp(true_weights, x_i, x_j, a, p) # we should use true weights
    preferences[r] <- rbinom(1, 1, p_i_j)
  }


  o1_values <- c()
  o2_values <- c()
  for (i in 1:n_solutons){
    o1_values <- c(o1_values, obj_1(c(x_values[i], y_values[i])))
    o2_values <- c(o2_values, obj_2(c(x_values[i], y_values[i])))
  }
  n_obj_1 <- min_max_normalize(o1_values, obj_1_min, obj_1_max)
  n_obj_2 <- min_max_normalize(o2_values, obj_2_min, obj_2_max)


  fit <-
    sampling(
      compiled_model,
      data = list(
        n_pairs = length(preferences),
        y = preferences,
        index_i = index_i,
        index_j = index_j,
        n_points = length(n_obj_1),
        obj_1 = n_obj_1,
        obj_2 = n_obj_2,
        eta = alpha_t,
        a = a,
        p = p,
        z_star = z_star),
      chains = 1,
      seed = 2020,
      iter = 21000,
      warmup = 1000
    )
  print(fit)


  params1 <- as.data.frame(rstan::extract(fit, permuted=FALSE)[,1,])
  est_weights <-
    data.frame(
      Iterations = 1:length(params1$`weights[1]`),
      w1 = params1$`weights[1]`,
      w2 = params1$`weights[2]`

    )
  Weights <- as.matrix(est_weights[,c(2,3)])
  weights_t <- colMeans(Weights)
  alpha_t <- estimate_dirichlet_mm(Weights) #Modified: 7th May 2025

  # ------------------------------------------------
  # Bayesian decision at iteration t (GA with learned weights)
  # ------------------------------------------------
  sff_final <- function(x) {
    ss <- (weights_t[1] * abs(z_star[1] - min_max_normalize(obj_1(x), obj_1_min, obj_1_max)) ^ p +
             weights_t[2] * abs(z_star[2] - min_max_normalize(obj_2(x), obj_2_min, obj_2_max)) ^ p)

    ss <- -ss
    pen <- sqrt(.Machine$double.xmax)
    penalty1 <- max(c1(x), 0) * pen
    penalty2 <- max(c2(x), 0) * pen
    ss - penalty1 - penalty2
  }

  ga_final <- ga(
    type = "real-valued",
    fitness = sff_final,
    lower = c(0, 0),
    upper = c(3.14, 3.14),
    popSize = 100,
    maxiter = 200,
    run = 100
  )

  decision_x <- as.numeric(ga_final@solution[1, 1:2])

  final_solutions[[t]] <- decision_x
  final_objectives[[t]] <- c(
    obj_1 = obj_1(decision_x),
    obj_2 = obj_2(decision_x)
  )

  iter_end <- Sys.time()
  iteration_time[t] <- as.numeric(difftime(iter_end, iter_start, units = "secs"))
}

# save.image(file = "mcdm-tnz-bcl.RData")
final_solutions_df <- do.call(rbind, final_solutions)
colnames(final_solutions_df) <- c("x1", "x2")
final_solutions_df <- as.data.frame(final_solutions_df)
final_solutions_df$Iteration <- 1:8

final_objectives_df <- do.call(rbind, final_objectives)
final_objectives_df <- as.data.frame(final_objectives_df)
final_objectives_df$Iteration <- 1:8

execution_time_df <- data.frame(
  Iteration = 1:8,
  Time_sec = iteration_time
)

# Plot weight updates
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
#   filename = paste("tnz-Bayesian-weights-updates.pdf", sep = ""),
#   plot = gp,
#   width = 8,
#   height = 4.5,
#   units = "in"
# )

# ----------------------------------------------
# Post processing
# ----------------------------------------------

# Combine and print weight + alpha summary
wa <- cbind(weights_updates, alpha_updates) 
rownames(wa) <- 0:(nrow(wa) - 1) #Modified: 7th May 2025
colnames(wa) <- c("w1", "w2", "alpha1", "alpha2")

require(xtable)
xt <- xtable(wa)
digits(xt) <- xdigits(xt, zap = 3)
print(xt)

# save(
#   final_solutions_df,
#   final_objectives_df,
#   execution_time_df,
#   weights_updates,
#   alpha_updates,
#   file = "tnz-bcl-results.RData"
# )

# save.image(file = "tnz-bcl-results.RData")

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
  n = n_solutons
)

# Print table
print(results_table)