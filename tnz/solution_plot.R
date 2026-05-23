# Author: Akanksha Mishra
# Solutions for bcl vs batch vs PLMBO

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

# Read additional feasible points from CSV
feasible2_df <- read.csv("C:/Users/Akanksha Mishra/Documents/research/MODM/PLMBO/tol_0.1/plmbo_tanaka_feasible_x.csv")

library(ggplot2)

ggplot() +
  geom_point(
    data = feasible_df,
    aes(x = x1, y = x2),
    color = "grey70",
    alpha = 0.6,
    size = 1,
    shape = 3
  ) +
  geom_point(
    data = feasible2_df,
    aes(x = x1, y = x2),
    color = "darkgreen",
    alpha = 0.7,
    size = 1
  ) +
  geom_point(
    data = bcl_df,
    aes(x = x1, y = x2),
    color = "#0072B2",
    size = 1,
    
  ) +
  geom_point(
    data = batch_df,
    aes(x = x1, y = x2),
    color = "red",
    size = 1,
    shape = 17
  ) +
  theme_light() +
  labs(
    x = expression(x[1]),
    y = expression(x[2]),
    # title = "TNZ-Batch-vs-BCL-PLMBO",
    # subtitle = "Grey: feasible set (TNZ)",
    # "| Green: feasible set (CSV)",
    # "| Blue: continual decisions",
    # "| Red triangle: batch decision"
  )

# # Objective values
# bcl_obj_df <- final_objectives_df
# bcl_obj_df$Method <- "Bayesian Continual"
# 
# batch_obj_df <- data.frame(
#   obj_1 = batch_final_objectives["obj_1"],
#   obj_2 = batch_final_objectives["obj_2"],
#   Method = "Batch Bayesian"
# )

# ggplot() +
#   geom_point(
#     data = bcl_obj_df,
#     aes(x = obj_1, y = obj_2),
#     color = "#0072B2",
#     size = 3
#   ) +
#   geom_path(
#     data = bcl_obj_df,
#     aes(x = obj_1, y = obj_2),
#     color = "#0072B2",
#     linewidth = 1
#   ) +
#   geom_point(
#     data = batch_obj_df,
#     aes(x = obj_1, y = obj_2),
#     color = "#D55E00",
#     size = 5,
#     shape = 17
#   ) +
#   theme_light() +
#   labs(
#     x = expression(f[1](x)),
#     y = expression(f[2](x)),
#     title = "Objective-Space Comparison",
#     subtitle = "Blue: BCL trajectory | Red: batch decision"
#   )