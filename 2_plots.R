# MSC Review Rates Simulation - Visualization
# This script creates visualizations of the simulation results

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)
theme_set(theme_classic())

# Read simulation results
results_full <- read.csv("outputs/simulation_results_full_tidy.csv")
results_summary <- read.csv("outputs/simulation_results_summary_tidy.csv")

# Plot 1: Bias comparison across monitoring strategies
plot_bias <- ggplot(results_summary, aes(x = description, y = mean_bias, color = strategy)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_ci_mean_bias, ymax = upper_ci_mean_bias),
                position = position_dodge(width = 0.5), width = 0.25) +
  labs(title = "Bias in Catch Rate Estimation by Monitoring Strategy",
       x = "Parameter Set", y = "Bias", color = "Monitoring Strategy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_bias_percent <- ggplot(results_summary, aes(x = description, y = mean_bias_percent, color = strategy)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_ci_mean_bias_percent, ymax = upper_ci_mean_bias_percent),
                position = position_dodge(width = 0.5), width = 0.25) +
  labs(title = "Percent Bias in Catch Rate Estimation",
       subtitle = "By Monitoring Strategy",
       x = "", y = "Percent Bias (%)", color = "Monitoring Strategy") +
  # scale_color_viridis_d() +  # Modern color palette
  # theme_test() + 
  # theme_fivethirtyeight()+
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

# Plot 3: Distribution of bias for each parameter set
plot_distribution <- function(param_id) {
  subset_data <- results_full %>% filter(parameter_set_id == param_id)
  param_desc <- unique(subset_data$description)
  
  ggplot(subset_data, aes(x = bias_percent, fill = strategy)) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Distribution of Bias -", param_desc),
         x = "Percent Bias (%)", y = "Density", fill = "Monitoring Strategy") 
}

# Create distribution plots for each parameter set
distribution_plots <- lapply(unique(results_full$parameter_set_id), plot_distribution)

# Plot 4: Monitoring coverage comparison
plot_coverage <- ggplot(results_summary, aes(x = description, y = mean_estimated_rate, color = strategy)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(title = "Mean Estimated Catch Rate by Monitoring Strategy",
       x = "Parameter Set", y = "Mean Estimated Catch Rate", color = "Monitoring Strategy") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 5: Bias magnitude comparison
plot_bias_magnitude <- ggplot(results_summary, aes(x = strategy, y = abs(mean_bias_percent), color = description)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(title = "Absolute Bias by Monitoring Strategy and Parameter Set",
       x = "Monitoring Strategy", y = "Absolute Percent Bias (%)", color = "Parameter Set") 

# Function to save both high-resolution and low-resolution versions of plots
save_plot_versions <- function(plot, filename, width_hi, height_hi) {
  # Save high-resolution version
  ggsave(paste0("plots/", filename, ".png"), 
         plot, width = width_hi, height = height_hi)
  
  # Calculate dimensions for low-resolution version (maintaining aspect ratio)
  aspect_ratio <- width_hi / height_hi
  if (aspect_ratio >= 1) {
    # Wider than tall
    width_lo <- 640
    height_lo <- 640 / aspect_ratio
  } else {
    # Taller than wide
    height_lo <- 640
    width_lo <- 640 * aspect_ratio
  }
  
  # Save low-resolution version
  ggsave(paste0("plots/lowres/", filename, "_lowres.png"), 
         plot, width = width_lo / 72, height = height_lo / 72, dpi = 72)
}

# Plot 2: Estimated vs. True Rates (creating this plot which seems to be missing but referenced later)
plot_rates <- ggplot(results_summary, aes(x = mean_true_rate, y = mean_estimated_rate, color = strategy)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Estimated vs. True Catch Rates",
       x = "True Catch Rate", y = "Estimated Catch Rate", color = "Monitoring Strategy") +
  facet_wrap(~description)

# Save plots with both high-resolution and low-resolution versions
save_plot_versions(plot_bias, "bias_comparison", 12, 8)
save_plot_versions(plot_bias_percent, "percent_bias_comparison", 12, 8)
save_plot_versions(plot_rates, "estimated_vs_true", 10, 8)
save_plot_versions(plot_coverage, "coverage_comparison", 12, 8)
save_plot_versions(plot_bias_magnitude, "bias_magnitude", 12, 8)

# Save distribution plots - both versions
for (i in 1:length(distribution_plots)) {
  save_plot_versions(distribution_plots[[i]], paste0("bias_distribution_", i), 10, 6)
}

# Create a combined plot for a report
combined_plot <- grid.arrange(plot_bias, plot_rates, ncol = 1)
save_plot_versions(combined_plot, "combined_results", 12, 16)

# Print summary statistics
cat("Summary of Results:\n")
cat("==================\n\n")
