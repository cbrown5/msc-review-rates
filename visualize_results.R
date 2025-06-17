# MSC Review Rates Simulation - Visualization
# This script creates visualizations of the simulation results

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)

# Read simulation results
results_full <- read.csv("simulation_results_full.csv")
results_summary <- read.csv("simulation_results_summary.csv")

# Plot 1: Bias comparison across monitoring strategies
plot_bias <- ggplot(results_summary, aes(x = description, y = mean_bias_percent, fill = strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                position = position_dodge(0.9), width = 0.25) +
  labs(title = "Bias in Catch Rate Estimation by Monitoring Strategy",
       x = "Parameter Set", y = "Percent Bias (%)", fill = "Monitoring Strategy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Estimated vs. True Catch Rates
plot_rates <- ggplot(results_summary, aes(x = mean_true_rate, y = mean_estimated_rate, color = strategy, shape = description)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Estimated vs. True Catch Rates",
       x = "True Catch Rate", y = "Estimated Catch Rate", 
       color = "Monitoring Strategy", shape = "Parameter Set") +
  theme_minimal()

# Plot 3: Distribution of bias for each parameter set
plot_distribution <- function(param_id) {
  subset_data <- results_full %>% filter(parameter_set_id == param_id)
  param_desc <- unique(subset_data$description)
  
  ggplot(subset_data, aes(x = bias_percent, fill = strategy)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Distribution of Bias -", param_desc),
         x = "Percent Bias (%)", y = "Density", fill = "Monitoring Strategy") +
    theme_minimal()
}

# Create distribution plots for each parameter set
distribution_plots <- lapply(unique(results_full$parameter_set_id), plot_distribution)

# Plot 4: Monitoring coverage comparison
plot_coverage <- ggplot(results_summary, aes(x = description, y = mean_estimated_rate, fill = strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Estimated Catch Rate by Monitoring Strategy",
       x = "Parameter Set", y = "Mean Estimated Catch Rate", fill = "Monitoring Strategy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 5: Bias magnitude comparison
plot_bias_magnitude <- ggplot(results_summary, aes(x = strategy, y = abs(mean_bias_percent), fill = description)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Absolute Bias by Monitoring Strategy and Parameter Set",
       x = "Monitoring Strategy", y = "Absolute Percent Bias (%)", fill = "Parameter Set") +
  theme_minimal()

# Save plots
ggsave("bias_comparison.png", plot_bias, width = 12, height = 8)
ggsave("estimated_vs_true.png", plot_rates, width = 10, height = 8)
ggsave("coverage_comparison.png", plot_coverage, width = 12, height = 8)
ggsave("bias_magnitude.png", plot_bias_magnitude, width = 12, height = 8)

# Save distribution plots
for (i in 1:length(distribution_plots)) {
  ggsave(paste0("bias_distribution_", i, ".png"), distribution_plots[[i]], width = 10, height = 6)
}

# Create a combined plot for a report
combined_plot <- grid.arrange(plot_bias, plot_rates, ncol = 1)
ggsave("combined_results.png", combined_plot, width = 12, height = 16)

# Print summary statistics
cat("Summary of Results:\n")
cat("==================\n\n")

# Print mean bias by strategy across all parameter sets
bias_summary <- results_summary %>%
  group_by(strategy) %>%
  summarize(
    mean_bias = mean(mean_bias_percent, na.rm = TRUE),
    sd_bias = sd(mean_bias_percent, na.rm = TRUE),
    .groups = "drop"
  )

print(bias_summary)

cat("\nVisualization complete. Plots saved as PNG files.\n")