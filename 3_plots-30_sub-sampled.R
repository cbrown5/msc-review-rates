# MSC Review Rates Simulation - Visualization
# This script creates visualizations of the simulation results
# Compare scenario with all sets on 30% vessels/trips with 50% sets of 60% vessels/trips

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(patchwork)
theme_set(theme_classic())

monitoring_params_df <- read.csv("parameters-monitoring.csv")
spp_params_df <- read.csv("parameters-species.csv")

params_df <- merge(monitoring_params_df, spp_params_df, by = NULL) %>%
mutate(parameter_set_id = row_number()) 

# Read simulation results and filter to compare specific monitoring scenarios
# monitoring_param_set 1-3: 30% vessels/trips with 100% sets
# monitoring_param_set 7-9: 60% vessels/trips with 50% sets  
results_summary <- read.csv("outputs/simulation_results_full_tidy.csv")  %>%
  mutate(catch_missed = (true_catch_rate - estimated_catch_rate)*total_sets) %>%
    group_by(parameter_set_id, description, strategy) %>%
    summarize(
      mean_estimated_rate = mean(estimated_catch_rate, na.rm = TRUE),
      mean_monitored_sets = mean(monitored_sets, na.rm = TRUE),
      mean_true_rate = mean(true_catch_rate, na.rm = TRUE),
      mean_true_catch = mean(true_catch_rate * total_sets, na.rm = TRUE),
      mean_estimated_catch = mean(estimated_catch_rate * total_sets, na.rm = TRUE),
      mean_catch_missed = mean(catch_missed, na.rm = TRUE),
      mean_bias = mean(bias, na.rm = TRUE),
      mean_bias_percent = mean(bias_percent, na.rm = TRUE),
      lower_ci_mean_bias = quantile(bias, 0.025, na.rm = TRUE),
      upper_ci_mean_bias = quantile(bias, 0.975, na.rm = TRUE),
      lower_ci_mean_bias_percent = quantile(bias_percent, 0.025, na.rm = TRUE),
      upper_ci_mean_bias_percent = quantile(bias_percent, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
  left_join(params_df, by = c("description", "parameter_set_id"))

results_summary <- results_summary %>%
  filter(monitoring_param_set %in% c(1, 2, 3, 7, 8, 9)) %>%
  mutate(
    # Create scenario labels for comparison
    monitoring_scenario = case_when(
      monitoring_param_set %in% c(1, 2, 3) ~ "30% vessels/trips (100% sets)",
      monitoring_param_set %in% c(7, 8, 9) ~ "30% vessels/trips (20% sets)",
      TRUE ~ "Other"
    ),
    # Clean up description labels
    description = str_replace(description, " - sub-sample sets", "")
  )

results_summary %>% filter(
  description == "Baseline",
  strategy %in% c("sets"),
  species == "Market species"
)


scnrs_to_plot <- data.frame(
  strategy = c("sets", "trips", "vessels", "trips", "vessels"), 
  description = c(
      rep("Baseline", 3),
      "Trip bias", 
      "Vessel bias"
  )
)

spp_scnrs_to_plot <- merge(scnrs_to_plot, data.frame(species = unique(results_summary$species)),
      by = NULL) %>%
      arrange(strategy)

results_plot <- results_summary %>%
  inner_join(spp_scnrs_to_plot, by = c("species", "strategy", "description")) 

yaxis_scale <- c(-99, 99)

# Market species plot - faceted by monitoring scenario
results_market <- results_plot %>%
  filter(grepl("Market species", species)) %>%
  mutate(
    colour_scale = str_to_title(paste(strategy, description, sep = " - "))
  )

plot_market <- ggplot(results_market, aes(x = colour_scale, y = mean_bias_percent, color = monitoring_scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_ci_mean_bias_percent, ymax = upper_ci_mean_bias_percent),
    position = position_dodge(width = 0.5), width = 0.25
  ) +
  facet_wrap(~ species, ncol = 2) +
  labs(
    title = "Market Species: Percent Bias in Catch Rate Estimation",
    subtitle = "Comparison of 30% vessels/trips (100% sets) vs 30% vessels/trips (20% sets)",
    x = "", y = "Percent Bias (%)", color = "Monitoring Scenario"
  ) +
  scale_color_canva() +
  scale_y_continuous(limits = yaxis_scale, breaks = seq(-80, 80, 20)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 10, 10, 15),
    panel.grid.minor = element_blank()
  )
plot_market

# Market species plot - mean monitored sets
plot_market_sets <- ggplot(results_market, aes(x = colour_scale, y = mean_monitored_sets, color = monitoring_scenario)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
  facet_wrap(~ species, ncol = 2) +
  labs(
    title = "Market Species: Mean Number of Monitored Sets",
    subtitle = "Comparison of 30% vessels/trips (100% sets) vs 30% vessels/trips (20% sets)",
    x = "", y = "Mean Monitored Sets", color = "Monitoring Scenario"
  ) +
  scale_color_canva() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 10, 10, 15),
    panel.grid.minor = element_blank()
  )
plot_market_sets

# Market species plot - mean catch missed
plot_market_missed <- ggplot(results_market, aes(x = colour_scale, y = mean_catch_missed, color = monitoring_scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
  facet_wrap(~ species, ncol = 2) +
  labs(
    title = "Market Species: Mean Catch Missed",
    subtitle = "Comparison of 30% vessels/trips (100% sets) vs 30% vessels/trips (20% sets)",
    x = "", y = "Mean Catch Missed", color = "Monitoring Scenario"
  ) +
  scale_color_canva() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top",
    strip.text = element_text(size = 12, face = "bold"),
    plot.margin = margin(10, 10, 10, 15),
    panel.grid.minor = element_blank()
  )
plot_market_missed

#
# Bycatch species plot - monitoring scenario as color
#
results_bycatch <- results_plot %>%
  filter(grepl("Bycatch species", species)) %>%
  mutate(
    colour_scale = str_to_title(paste(strategy, description, sep = " - "))
  )

plot_bycatch <- ggplot(results_bycatch, aes(x = colour_scale, y = mean_bias_percent,
 color = monitoring_scenario)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_ci_mean_bias_percent, ymax = upper_ci_mean_bias_percent),
    position = position_dodge(width = 0.5), width = 0.25
  ) +
  labs(
    title = "Bycatch Species: Percent Bias in Catch Rate Estimation",
    subtitle = "Comparison of 30% vessels/trips (100% sets) vs 30% vessels/trips (20% sets)",
    x = "", y = "Percent Bias (%)", color = "Monitoring Scenario"
  ) +
  scale_color_canva() +
  scale_y_continuous(limits = yaxis_scale, breaks = seq(-80, 80, 20)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top",
    plot.margin = margin(10, 10, 10, 15),
    panel.grid.minor = element_blank()
  )
plot_bycatch
# Save the plots with updated filenames to reflect the new comparison
ggsave("plots/plot_bias_percent_bycatch_30vs60_comparison.png", plot = plot_bycatch,
       width = 8, height = 9, dpi = 300)
# ggsave("plots/plot_bias_percent_bycatch_30vs60_comparison.pdf", plot = plot_bycatch,
#        width = 8, height = 9)

ggsave("plots/plot_bias_percent_market_30vs60_comparison.png", plot = plot_market,
       width = 8, height = 9, dpi = 300)
# ggsave("plots/plot_bias_percent_market_30vs60_comparison.pdf", plot = plot_market,
#        width = 8, height = 9)

ggsave("plots/plot_monitored_sets_market_30vs60_comparison.png", plot = plot_market_sets,
       width = 8, height = 9, dpi = 300)

ggsave("plots/plot_catch_missed_market_30vs60_comparison.png", plot = plot_market_missed,
       width = 8, height = 9, dpi = 300)
