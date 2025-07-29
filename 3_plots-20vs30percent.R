# MSC Review Rates Simulation - Visualization
# This script creates visualizations of the simulation results

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(patchwork)
theme_set(theme_classic())

# Read simulation results - don't filter p_monitor to get both 0.2 and 0.3
results_summary <- read.csv("outputs/simulation_results_summary_tidy.csv") %>%
  #remove the string '20% for all description values
  mutate(description = str_replace(description, " 20%", ""))


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

# Market species plot - faceted by p_monitor
results_market <- results_plot %>%
  filter(grepl("Market species", species)) %>%
  mutate(
    p_monitor_label = paste("Monitoring Rate:", p_monitor),
    colour_scale = str_to_title(paste(strategy, description, sep = " - ")),
        p_monitor = factor(p_monitor, levels = c(0.2, 0.3), labels = c("20%", "30%"))

  )

plot_market <- ggplot(results_market, aes(x = colour_scale, y = mean_bias_percent, color = p_monitor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_ci_mean_bias_percent, ymax = upper_ci_mean_bias_percent),
    position = position_dodge(width = 0.5), width = 0.25
  ) +
  facet_wrap(~ species, ncol = 2) +
  labs(
    title = "Market Species: Percent Bias in Catch Rate Estimation",
    subtitle = "Comparison of 20% vs 30% Monitoring Rates",
    x = "", y = "Percent Bias (%)", color = "Monitoring Scenario"
  ) +
  # scale_color_canva() +
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
# Bycatch species plot - p_monitor as x-axis levels
#
results_bycatch <- results_plot %>%
  filter(grepl("Bycatch species", species)) %>%
  # filter(species == "Market species + high trip variance") %>%
  mutate(
    x_axis = paste(species, "- Rate:", p_monitor),
    colour_scale = str_to_title(paste(strategy, description, sep = " - ")),
    p_monitor = factor(p_monitor, levels = c(0.2, 0.3), labels = c("20%", "30%"))
  ) %>%
  arrange(species, p_monitor)

plot_bycatch <- ggplot(results_bycatch, aes(x = colour_scale, y = mean_bias_percent,
 color = factor(p_monitor))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_ci_mean_bias_percent, ymax = upper_ci_mean_bias_percent),
    position = position_dodge(width = 0.5), width = 0.25
  ) +
  labs(
    title = "Bycatch Species: Percent Bias in Catch Rate Estimation",
    subtitle = "Comparison of 20% vs 30% Monitoring Rates",
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
# Save the plots
# ggsave("plots/plot_bias_percent_market_comparison.png", plot = plot_market,
#        width = 12, height = 8, dpi = 300)
ggsave("plots/plot_bias_percent_bycatch_comparison.png", plot = plot_bycatch,
       width = 8, height = 9, dpi = 300)
ggsave("plots/plot_bias_percent_bycatch_comparison.pdf", plot = plot_bycatch,
       width = 8, height = 9)


ggsave("plots/plot_bias_percent_market_comparison.png", plot = plot_market,
       width = 8, height = 9, dpi = 300)
ggsave("plots/plot_bias_percent_market_comparison.pdf", plot = plot_market,
       width = 8, height = 9)
