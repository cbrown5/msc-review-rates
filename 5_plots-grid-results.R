# MSC Review Rates Simulation - Visualization
# This script creates visualizations of the simulation results

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(patchwork)
theme_set(theme_classic())

# Read simulation results for grid analysis
results_summary <- read.csv("outputs/simulation_results_summary_tidy-grid.csv") 

# Extract scenario type from description column
results_plot <- results_summary %>%
  mutate(
    scenario = case_when(
      grepl("Baseline", description) ~ "Baseline",
      grepl("Vessel bias", description) ~ "Vessel bias",
      grepl("Trip bias", description) ~ "Trip bias"
    ),
    # Convert p_sets_select to percentage for better readability
    review_rate_percent = p_sets_select * 100,
    # Clean up strategy names for legend
    strategy_clean = str_to_title(strategy)
  ) %>%
  filter(!is.na(scenario))  # Remove any rows without clear scenario

yaxis_scale <- c(-70, 50)  # Adjust scale based on expected bias range
# Create the main plot: Review rate vs % bias by scenario
plot_grid_results <- ggplot(results_plot, aes(x = review_rate_percent, y = mean_bias_percent, color = strategy_clean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.7) +
  geom_line(size = 1, alpha = 0.8, position = position_dodge(width = 5)) +
  geom_point(size = 2.5, alpha = 0.9, position = position_dodge(width = 5)) +
  geom_errorbar(
    aes(ymin = lower_ci_mean_bias_percent, ymax = upper_ci_mean_bias_percent),
    width = 2, alpha = 0.7, size = 0.5,
    position = position_dodge(width = 5)
  ) +
  facet_grid(p_monitor ~ scenario, scales = "fixed") +
  labs(
    title = "Impact of Review Rate on Catch Rate Estimation Bias",
    subtitle = "Market Species: Comparison across Monitoring Strategies and Bias Scenarios",
    x = "Review Rate (%)", 
    y = "Percent Bias (%)", 
    color = "Monitoring Strategy"
  ) +
  scale_color_canva() +
  scale_x_continuous(breaks = seq(10, 100, 20), limits = c(10, 100)) +
  scale_y_continuous(limits = yaxis_scale, breaks = seq(-60, 40, 20)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    plot.margin = margin(15, 15, 15, 15),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.3),
    panel.grid.major.y = element_line(color = "grey90", size = 0.3)
  )

plot_grid_results

ggsave("plots/plot_bias_grid_results.png", plot = plot_grid_results,
       width = 12, height = 6, dpi = 300)

# Create simplified plot for baseline scenario only
baseline_data <- results_plot %>%
  filter(scenario == "Baseline") %>%
  filter(p_monitor == 1) %>%
  mutate(
    strategy_panel = case_when(
      strategy_clean == "Sets" ~ "Set-level Monitoring",
      strategy_clean == "Trips" ~ "Trip-level Monitoring", 
      strategy_clean == "Vessels" ~ "Vessel-level Monitoring"
    ),
    monitor_rate_label = paste0(p_monitor * 100, "% Coverage")
  ) %>%
  mutate(
    strategy_panel = factor(
      strategy_panel,
      levels = c("Set-level Monitoring", "Vessel-level Monitoring", "Trip-level Monitoring"),
      labels = c("Scenario 1: Sets", "Scenario 2: Vessels", "Scenario 3: Trips")
    )
  )

plot_baseline_simple <- ggplot(baseline_data, aes(x = review_rate_percent, y = mean_bias_percent)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.7) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 2.5, alpha = 0.9) +
  geom_errorbar(
    aes(ymin = lower_ci_mean_bias_percent, ymax = upper_ci_mean_bias_percent),
    width = 2, alpha = 0.7, size = 0.5
  ) +
  facet_wrap(~ strategy_panel, nrow = 1) +
  labs(
    title = "Review Rate Impact on Catch Estimation Bias",
    subtitle = "Market Species across Different Monitoring Scenarios",
    x = "Review Rate (%)", 
    y = "Percent Bias (%)", 
    color = "Monitor Coverage"
  ) +
  scale_color_canva() +
  scale_x_continuous(breaks = seq(20, 100, 20), limits = c(10, 100)) +
  scale_y_continuous(limits = c(-20, 20), breaks = seq(-20, 20, 5)) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    plot.margin = margin(15, 15, 15, 15),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", size = 0.3)
  )

plot_baseline_simple

ggsave("plots/plot_baseline_simple.png", plot = plot_baseline_simple,
       width = 12, height = 4, dpi = 300)
ggsave("plots/plot_baseline_simple.eps", plot = plot_baseline_simple,
       width = 12, height = 4)


