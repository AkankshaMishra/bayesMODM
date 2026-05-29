# Author: Akanksha Mishra
# Zoomed version of solutions for bcl vs batch

# Feasible TNZ set
feasible_df <- data.frame(
  x1 = x_tnz_100,
  x2 = y_tnz_100
)

# BCL solutions
bcl_df <- final_solutions_df
bcl_df$Method <- "Bayesian Continual"

# Batch solution (single point)
batch_df <- data.frame(
  x1 = batch_final_solution[1],
  x2 = batch_final_solution[2],
  Method = "Batch Bayesian"
)

library(ggplot2)

# Combine points that define the zoom region
zoom_df <- rbind(
  bcl_df[, c("x1", "x2")],
  batch_df[, c("x1", "x2")]
)

# Define zoom limits with a small margin
x_margin <- 0.01
y_margin <- 0.01

x_limits <- range(zoom_df$x1) + c(-x_margin, x_margin)
y_limits <- range(zoom_df$x2) + c(-y_margin, y_margin)

ggplot() +
  # Feasible TNZ set (background)
  geom_point(
    data = feasible_df,
    aes(x = x1, y = x2),
    color = "grey80",
    alpha = 0.6,
    size = 1,
    shape = 3
  ) +
  # BCL solutions
  geom_point(
    data = bcl_df,
    aes(x = x1, y = x2),
    color = "#0072B2",
    size = 1.5
  ) +
  # Batch solution
  geom_point(
    data = batch_df,
    aes(x = x1, y = x2),
    color = "red",
    size = 2,
    shape = 17
  ) +
  coord_cartesian(
    xlim = x_limits,
    ylim = y_limits
  ) +
  theme_light() +
  labs(
    x = expression(x[1]),
    y = expression(x[2]),
    # title = "Zoomed View: Batch vs Bayesian Continual (TNZ)",
    # subtitle = "Focused on region containing continual (blue) and batch (red) decisions"
  )