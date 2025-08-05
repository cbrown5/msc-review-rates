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

# Read simulation results and filter to compare specific monitoring scenarios
# monitoring_param_set 1-3: 30% vessels/trips with 100% sets
# monitoring_param_set 7-9: 60% vessels/trips with 50% sets  
results_summary <- read.csv("outputs/simulation_results_summary_tidy.csv") %>%
  filter(monitoring_param_set %in% c(1, 2, 3, 7, 8, 9)) %>%
  mutate(
    # Create scenario labels for comparison
    monitoring_scenario = case_when(
      monitoring_param_set %in% c(1, 2, 3) ~ "30% vessels/trips (100% sets)",
      monitoring_param_set %in% c(7, 8, 9) ~ "60% vessels/trips (50% sets)",
      TRUE ~ "Other"
    ),
    # Clean up description labels
    description = str_replace(description, " - sub-sample sets", "")
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

yaxis_scale <- c(-90, 90)

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
    subtitle = "Comparison of 30% vessels/trips (100% sets) vs 60% vessels/trips (50% sets)",
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
    subtitle = "Comparison of 30% vessels/trips (100% sets) vs 60% vessels/trips (50% sets)",
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
