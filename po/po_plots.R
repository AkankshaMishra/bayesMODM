#Modified: 18 Feb 2026
#PO on objective space

rm(list = ls())
setwd("~/research/MODM/portfolio-optimization/w73")

library(ggplot2)
library(readr)

# === Load Data ===
df_base <- read_csv("df_base.csv")  # Grey cross points
df_batch_sp <- read.csv("df_batch_sp.csv")
df_batch_llm <- read.csv("df_batch_llm.csv")
df_red <- read_csv("df_overlay_sp.csv")    # Red labeled points (with iteration number)
df_blue <- read_csv("df_overlay_llm.csv")  # Blue labeled points (with iteration number)

# === Separate 8th iteration ===
red_star <- subset(df_red, Iteration == 7)
red_others <- subset(df_red, Iteration != 7)

blue_star <- subset(df_blue, Iteration == 7)
blue_others <- subset(df_blue, Iteration != 7)

legend_df <- data.frame(
  Preference = c("BTL-simulated preferences", "LLM-generated preferences"),
  x = NA,
  y = NA
)

# === Prepare Plot ===
p <- ggplot() +
  # Base Pareto-optimal front as grey crosses
  geom_point(data = df_base, aes(x = NegReturn, y = Risk),
             shape = 4,
             color = "black", size = 1, stroke = 0.6) +
  
  # 🔺 Batch solution(s) for random preferences as red triangles
  geom_point(
    data = df_batch_sp,
    aes(x = NegReturn, y = Risk),
    shape = 17, color = "red", size = 2
  ) +
  
  # 🔺 Batch solution(s) for LLM preferences as blue triangles
  geom_point(
    data = df_batch_llm,
    aes(x = NegReturn, y = Risk),
    shape = 17, color = "blue", size = 2
  ) +
  
  # Red-labeled iteration points (1–7)
  geom_point(data = red_others, aes(x = NegReturn, y = Risk),
             color = "red", size = 2) +
  geom_text(data = red_others, aes(x = NegReturn, y = Risk, label = Iteration),
            color = "red", size = 2, vjust = -1) +
  
  # Red star for iteration 8
  geom_point(data = red_star, aes(x = NegReturn, y = Risk),
             shape = 8, color = "red", size = 2) +
  
  # Blue-labeled iteration points (1–7)
  geom_point(data = blue_others, aes(x = NegReturn, y = Risk),
             color = "blue", size = 2) +
  geom_text(data = blue_others, aes(x = NegReturn, y = Risk, label = Iteration),
            color = "blue", size = 2, vjust = -1) +
  
  # Blue star for iteration 8
  geom_point(data = blue_star, aes(x = NegReturn, y = Risk),
             shape = 8, color = "blue", size = 2) +
  
  geom_line(
    data = legend_df,
    aes(x = x, y = y, color = Preference),
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "BTL-simulated preferences" = "red",
      "LLM-generated preferences" = "blue"
    )
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = "solid",
        shape = NA
      )
    )
  )+
  
  # Customize legend and axes
  # scale_shape_manual(values = c("Pareto-optimal front" = 4)) +
  # guides(shape = guide_legend(override.aes = list(color = "black"))) +
  labs(x = "Expected return", y = "Expected risk", shape = "", title = "") +
  theme_minimal(base_size = 10) +
  theme(
    axis.text = element_text(color = "black"),  # Tick labels
    axis.title = element_text(color = "black"), # Axis titles
    legend.text = element_text(color = "black"),
    legend.position = c(0.7, 0.7),
    legend.title = element_blank(),
    legend.background = element_rect(
      fill = "white",
      color = "gray",
      linewidth = 0.2
    )
    )

# === Display Plot ===
print(p)

ggsave("po_sp_llm_batch_vs_bcl_w73.pdf", plot = p, width = 5, height = 3, units = "in", dpi = 300)

