# Modified: 18 Feb 2026
# Plot weight updates
setwd("~/research/MODM/portfolio-optimization")
load("~/research/MODM/portfolio-optimization/po-bcl-sp-results-w55.RData")
load("~/research/MODM/portfolio-optimization/po-batch-sp-results-w55.RData")
setwd("~/research/MODM/portfolio-optimization/w55")

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
  Index = c("w1", "w2"),
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
  geom_hline(
    data = batch_df,
    aes(yintercept = w_batch, color = Index, linetype = Index),
    linetype = "dotdash",
    linewidth = 1.3,
    alpha = 0.9
  )+
  theme_light() + # scale_color_grey() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12, colour = "black"),
    legend.title = element_blank(),
    axis.text.x = element_text(size = 10, colour = "black"),
    axis.text.y = element_text(size = 10, colour = "black"),
    axis.title.y = element_text(size = 12, margin = margin(
      l = 0,
      r = 5,
      t = 0,
      b = 0
    )),
    axis.title.x = element_text(size = 12, margin = margin(
      l = 0,
      r = 0,
      t = 15,
      b = 0
    ))
  ) +
  scale_color_discrete(labels = legend_labels) +
  scale_linetype_discrete(labels = legend_labels)
gp

ggsave(
  filename = paste("po_sp_wt_batch_vs_bcl_w55.pdf", sep = ""),
  plot = gp,
  width = 5,
  height = 3,
  units = "in"
)