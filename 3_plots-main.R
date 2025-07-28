# MSC Review Rates Simulation - Visualization
# This script creates visualizations of the simulation results

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)
theme_set(theme_classic())

# Read simulation results
results_summary <- read.csv("outputs/simulation_results_summary_tidy-20perc.csv")


scnrs_to_plot <- data.frame(
  description = c(
    "Baseline - YFT", "Baseline - YFT", "Baseline - YFT",
    "High trip bias",
    "High trip variance + trip bias",
    "High vessel bias",
    "High vessel variance + vessel bias",
    "Rare species + trip bias",
    "Rare species + vessel bias",
    "Very rare species+ trip bias",
    "Very rare species+ vessel bias"
  ),
  strategy = c(
    "sets", "trips", "vessels",
    "trips",
    "trips",
    "vessels",
    "vessels",
    "trips",
    "vessels",
    "trips",
    "vessels"
  )
)

#UP TO HERE add strategies to scnrs to plot

results_plot <- results_summary %>%
  inner_join(scnrs_to_plot) %>%
  # change description for "Baseline - YFT" to "Baseline {monitoring strategy}
  mutate(description = ifelse(description == "Baseline - YFT",
    paste("Baseline - ", strategy), description
  ))
# Order x-axis by monitoring strategy
results_plot <- results_plot %>%
  mutate(description = factor(description, levels = results_plot %>%
    arrange(strategy, description) %>%
    distinct(description) %>%
    pull(description)))

plot_bias_percent <- ggplot(results_plot, aes(x = description, y = mean_bias_percent, color = strategy)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_ci_mean_bias_percent, ymax = upper_ci_mean_bias_percent),
    position = position_dodge(width = 0.5), width = 0.25
  ) +
  labs(
    title = "Percent Bias in Catch Rate Estimation",
    subtitle = "By Monitoring Strategy",
    x = "", y = "Percent Bias (%)", color = "Monitoring Strategy"
  ) +
  scale_color_canva() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 13, face = "bold"),
    title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "top",
    plot.margin = margin(10, 10, 10, 15),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines")
  )

plot_bias_percent

# Save the plot
ggsave("plots/plot_bias_percent.png", plot = plot_bias_percent,
       width = 10, height = 6, dpi = 300)
