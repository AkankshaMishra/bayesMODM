rm(list = ls())

library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)

# ------------------------------------------------------------
# Load regret data
# ------------------------------------------------------------
plmbo <- read_csv("plmbo_tanaka_regret.csv")
modm  <- read_csv("modm_tanaka_regret.csv")

plmbo$Method <- "PLMBO"
modm$Method  <- "MODM"

df <- bind_rows(plmbo, modm)

# ------------------------------------------------------------
# Plot regret comparison
# ------------------------------------------------------------
p_full <- ggplot(df, aes(x = Iteration, y = Regret, color = Method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal(base_size = 10) +
  labs(
    # title = "Simple Regret Comparison on Tanaka Problem",
    x = "Iteration",
    y = "Simple Regret"
  ) +
  # scale_y_log10() +
  scale_color_manual(values = c("PLMBO" = "#E64B35", "MODM" = "#4DBBD5")) +
  theme(
    legend.position = c(0.65, 0.25),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white", color = "gray", linewidth = 0.2),
    legend.title = element_blank()
  )

p_zoom <- p_full +
  coord_cartesian(ylim = c(0.015, 0.021)) +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = "gray", size = 0.8),
    plot.background  = element_rect(fill = "white", color = "gray", size = 0.8)
  )

p_full + inset_element(p_zoom, left = 0.4, bottom = 0.4, right = 0.95, top = 0.95)

# ------------------------------------------------------------
# Save publication-ready figure
# ------------------------------------------------------------
ggsave(
  "tanaka_regret_comparison.pdf",
  width = 5,
  height = 3
)

cat("Saved: tanaka_regret_comparison.pdf\n")