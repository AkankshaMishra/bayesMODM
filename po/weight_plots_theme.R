# Modified: 20 Feb 2026
# Plot weight updates
rm(list = ls())
setwd("~/research/MODM/portfolio-optimization")
load("~/research/MODM/portfolio-optimization/po-bcl-sp-results-w73.RData")
load("~/research/MODM/portfolio-optimization/po-batch-sp-results-w73.RData")
setwd("~/research/MODM/portfolio-optimization/w73")

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

batch_df <- tibble(
  Index = c("w2", "w1"),
  w_batch = w_batch
)

gp <-
  ggplot(data = weights_updates_long,
         aes(
           x = Iterations,
           y = Weights,
           color = Index,
           linetype = Index
         )) +
  geom_line(size = 1.0) +
  geom_point(size = 3, shape = 4) +
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
  geom_hline(
    data = batch_df,
    aes(yintercept = w_batch, color = Index, linetype = Index),
    linetype = "dotdash",
    linewidth = 1.0,
    alpha = 0.9
  )+
  labs(
    x = "Iterations",
    y = "Weights",
    title = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    
    # legend.text = element_text(color = "black", size = 9),
    legend.title = element_blank(),
    legend.position = c(0.75, 0.55), #0.75, 0.75
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white", color = "gray", linewidth = 0.2),
    legend.key.size = unit(0.4, "cm")
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      override.aes = list(linetype = "solid", shape = NA)),
    linetype = guide_legend(nrow = 1)
  ) +
  scale_color_discrete(labels = legend_labels) +
  scale_linetype_discrete(labels = legend_labels)
gp

ggsave(
  filename = paste("po_sp_wt_batch_vs_bcl_w73.pdf", sep = ""),
  plot = gp,
  width = 5,
  height = 3,
  units = "in"
)