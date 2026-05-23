#modified: 19 feb 2026
require(GA)
require(lattice)
require(tidyverse)

obj_1 <- function(x) {
  x[1]
}
obj_2 <- function(x) {
  x[2]
  
}

c1 <- function(x) {
  -x[1] ^ 2 - x[2] ^ 2 + 1 + 0.1 * cos(16 * atan(x[1] / x[2]))
}

c2 <- function(x) {
  (x[1] - 0.5) ^ 2 + (x[2] - 0.5) ^ 2 - 0.5
}

# home PC
setwd("~/research/MODM/mcdm-original")

tnz_pareto_front <- read_csv("tnz-pareto-front.csv", col_names = F)
tnz_function_space <- read_csv("tnz-function-space.csv", col_names = F)

## generating the feasible space 

ngrid <- 500
x1 <- seq(0, 1.4, length = ngrid)
x2 <- seq(0, 1.4, length = ngrid)
x12 <- expand.grid(x1, x2)
color_codes <- adjustcolor(bl2gr.colors(6)[2:3], alpha = 0.1)

margin = c(5 - 1, 4 - 0, 4 - 3, 2 - 1) # c(bottom, left, top, right)

op <- par(bg = "white")
trellis.device(
  pdf, file = "tnz-feasible-region-v1-nn.pdf", height = 4,
  width = 4, title = "", onefile = T
)
par(mar = margin + .2) # c(bottom, left, top, right)
plot(x1,
     x2,
     type = "n",
     xaxs = "i",
     yaxs = "i", 
     xlab = expression(x[1]), 
     ylab = expression(x[2]),
     cex = 1,
     cex.lab = 1.2,
     cex.axis = 1)
lines(tnz_pareto_front$X1, tnz_pareto_front$X2, lty = 2)
points(tnz_function_space$X1, tnz_function_space$X2, pch = 4, cex = .4, c = "gray")
legend(
  "topright",
  legend = c("Pareto-optimal front"),
  col = c("gray"),
  pch = 4,
  cex = 1.1, 
  box.lwd = NA, 
  inset=.01
)
image(x1,
      x2,
      matrix(ifelse(apply(x12, 1, c1) <= 0, 0, NA), ngrid, ngrid),
      col = color_codes[1],
      add = TRUE)
image(x1,
      x2,
      matrix(ifelse(apply(x12, 1, c2) <= 0, 0, NA), ngrid, ngrid),
      col = color_codes[2],
      add = TRUE)

# *tnz_sampling_smc.R
load("C:/Users/Akanksha Mishra/Documents/research/MODM/mcdm-bayes-main/R/tnz-feasible-points.rda")
points(x_tnz_100[1:20], y_tnz_100[1:20], col="blue", cex=.8, pch = 3)

ga_df <- read_csv("C:/Users/Akanksha Mishra/Documents/research/MODM/mcdm-bayes-main/R/final_solutions_df.csv")

last_iter <- max(ga_df$Iteration)
ga_prev <- ga_df[ga_df$Iteration != last_iter, ]
ga_last <- ga_df[ga_df$Iteration == last_iter, ]

# Label all but last with iteration numbers
text(ga_prev$x1, ga_prev$x2,
     labels = ga_prev$Iteration,
     col = "red", cex = 0.7)

# Plot last iteration as a star
points(ga_last$x1, ga_last$x2,
       col = "red", pch = 8, cex = 1)

# text(ga_df$x1, ga_df$x2,
#      labels = ga_df$Iteration,
#      col = "red", cex = 0.7)

batch_sol <- read_csv("C:/Users/Akanksha Mishra/Documents/research/MODM/mcdm-bayes-main/R/batch_final_solution.csv")

points(batch_sol$x1, batch_sol$x2,
       col = "red", pch = 17, cex = 1)

plmbo_df <- read.csv("C:/Users/Akanksha Mishra/Documents/research/MODM/PLMBO/tol_0.1/plmbo_tanaka_feasible_x.csv")

points(plmbo_df$x1, plmbo_df$x2,
       col = "darkgreen", pch = 19, cex = 0.7)

par(op)
dev.off()



a <- sample(1:110, 1000, replace = TRUE)
cut(a, breaks = c(0, 18, 30, 60, max(a))) 

library(MASS)
x = matrix(c(19, 11, 58, 8), nrow = 2, byrow = T)
D = factor(c("S1", "SH"), levels = c("S1", "SH"))
m = glm(x ~ D, family = binomial)
summary(m)



library(MASS)
x = matrix(c(19,11,58,8), nrow=2, byrow=T)
D = factor(c("S1","SH"), levels=c("S1","SH"))
m = glm(x~D, family=binomial)
summary(m)





ngrid <- 500
x1 <- seq(0.05, .25, length = ngrid)
x2 <- seq(.9, 1.10, length = ngrid)
x12 <- expand.grid(x1, x2)
color_codes <- adjustcolor(bl2gr.colors(6)[2:3], alpha = 0.1)

margin = c(5 - 1, 4 - 0, 4 - 3, 2 - 1) # c(bottom, left, top, right)

op <- par(bg = "white")
trellis.device(
  pdf, file = "tnz-feasible-region-v1b-nn.pdf", height = 4,
  width = 4, title = "", onefile = T
)
par(mar = margin + .2) # c(bottom, left, top, right)

plot(x1, x2,
     type = "n",
     xaxs = "i", yaxs = "i",
     xlim = c(0.14, 0.22),   # tighter zoom
     ylim = c(0.92, 0.97),   # tighter zoom
     xlab = expression(x[1]),
     ylab = expression(x[2]),
     cex.lab = 1.2,
     cex.axis = 1)

lines(tnz_pareto_front$X1, tnz_pareto_front$X2, lty = 2)
points(tnz_function_space$X1, tnz_function_space$X2, pch = 4, cex = .6, c = "gray")

legend(
  "topright",
  legend = c("Pareto-optimal front"),
  col = c("gray"),
  pch = 4,
  cex = 1.1, 
  box.lwd = NA, 
  inset=.01
)
image(x1,
      x2,
      matrix(ifelse(apply(x12, 1, c1) <= 0, 0, NA), ngrid, ngrid),
      col = color_codes[1],
      add = TRUE)
image(x1,
      x2,
      matrix(ifelse(apply(x12, 1, c2) <= 0, 0, NA), ngrid, ngrid),
      col = color_codes[2],
      add = TRUE)

# *tnz_sampling_smc.R
points(x_tnz_100[1:20], y_tnz_100[1:20], col="blue", cex=.8, pch = 3)

ga_df <- read_csv("C:/Users/Akanksha Mishra/Documents/research/MODM/mcdm-bayes-main/R/final_solutions_df.csv")

last_iter <- max(ga_df$Iteration)
ga_prev <- ga_df[ga_df$Iteration != last_iter, ]
ga_last <- ga_df[ga_df$Iteration == last_iter, ]

# Label all but last with iteration numbers
text(ga_prev$x1, ga_prev$x2,
     labels = ga_prev$Iteration,
     col = "red", cex = 0.7)

# Plot last iteration as a star
points(ga_last$x1, ga_last$x2,
       col = "red", pch = 8, cex = 1)

# text(ga_df$x1, ga_df$x2,
#      labels = ga_df$Iteration,
#      col = "red", cex = 0.7)

batch_sol <- read_csv("C:/Users/Akanksha Mishra/Documents/research/MODM/mcdm-bayes-main/R/batch_final_solution.csv")

points(batch_sol$x1, batch_sol$x2,
       col = "red", pch = 17, cex = 1.2)

plmbo_df <- read.csv("C:/Users/Akanksha Mishra/Documents/research/MODM/PLMBO/tol_0.1/plmbo_tanaka_feasible_x.csv")

points(plmbo_df$x1, plmbo_df$x2,
       col = "darkgreen", pch = 19, cex = 0.7)

par(op)
dev.off()