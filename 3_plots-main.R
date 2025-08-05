# MSC Review Rates Simulation - Visualization
# This script creates visualizations of the simulation results

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(patchwork)
theme_set(theme_classic())

# Read simulation results
results_summary <- read.csv("outputs/simulation_results_summary_tidy.csv") %>%
  filter(p_monitor == 0.3)


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
  inner_join(spp_scnrs_to_plot)  %>%
  # change description labels
  mutate(description = case_when(
    description == "Baseline" ~ "Random sets",
    description == "Trip bias" ~ "Trip bias",
    description == "Vessel bias" ~ "Vessel bias",
    TRUE ~ description
  ))

yaxis_scale <- c(-90, 70)

# Create data for each plot

#
# Sets plot - only sets strategy
#
results_sets <- results_plot %>%
  filter(strategy == "sets") %>%
  mutate(x_axis = paste(species, description, sep = " - ")) %>%
    arrange(species, description) 

plot_sets <- ggplot(results_sets, aes(x = species, y = mean_bias_percent, color = description)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_ci_mean_bias_percent, ymax = upper_ci_mean_bias_percent),
    position = position_dodge(width = 0.5), width = 0.25
  ) +
  labs(
    title = "Sets Scenario: Percent Bias in Catch Rate Estimation",
    x = "", y = "Percent Bias (%)", color = "Monitoring Scenario"
  ) +
  scale_color_canva() +
  scale_y_continuous(limits = yaxis_scale, breaks = seq(-80, 60, 20)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 13, face = "bold"),
    title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 14),
    legend.position = "top",
    plot.margin = margin(10, 10, 10, 15),
    panel.grid.minor = element_blank()
  )
plot_sets

#
# Trips plot - trips and sets strategies
#

results_trips <- results_plot %>%
  filter(strategy %in% c("sets", "trips"))%>%
  mutate(
    colour_scale = str_to_title(paste(strategy, description, sep = " - ")),
    colour_scale = case_when(colour_scale == "Sets - Random Sets" ~ "Random Sets",
       colour_scale == "Trips - Random Sets" ~ "Randomly selected \n trips",
       colour_scale == "Trips - Trip Bias" ~ "Bias to low \n catch rate trips",
       TRUE ~ colour_scale)
  ) %>%
  arrange(species, description) 
    
pd2 <- position_dodge(width = 0.5)

plot_trips <- ggplot(results_trips) + 
  aes(x = species, y = mean_bias_percent, color = colour_scale,
  group = colour_scale) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = pd2) +
  geom_errorbar(aes(ymin = lower_ci_mean_bias_percent, ymax = upper_ci_mean_bias_percent),
    position = pd2, width = 0.25) +
  labs(
    title = "Trips Scenario: Percent Bias in Catch Rate Estimation",
    subtitle = "With Sets Scenario as Reference",
    x = "", y = "Percent Bias (%)", color = "Monitoring Scenario"
  ) +
  scale_color_canva() +
  scale_y_continuous(limits = yaxis_scale, breaks = seq(-80, 60, 20)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 13, face = "bold"),
    title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top",
    plot.margin = margin(10, 10, 10, 15),
    panel.grid.minor = element_blank()
  )
plot_trips

#
# Vessels plot - vessels and sets strategies
#

results_vessels <- results_plot %>%
  filter(strategy %in% c("sets", "vessels")) %>%
  mutate(
    colour_scale = str_to_title(paste(strategy, description, sep = " - "))
  ) %>%
  arrange(species, description)

pd3 <- position_dodge(width = 0.5)
plot_vessels <- ggplot(results_vessels) + 
  aes(x = species, y = mean_bias_percent, color = colour_scale,
  group = colour_scale) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = pd3) +
  geom_errorbar(aes(ymin = lower_ci_mean_bias_percent, ymax = upper_ci_mean_bias_percent),
    position = pd3, width = 0.25) +
  labs(
    title = "Vessels Scenario: Percent Bias in Catch Rate Estimation",
    subtitle = "With Sets Scenario as Reference",
    x = "", y = "Percent Bias (%)", color = "Monitoring Scenario"
  ) +
  scale_color_canva() +
  scale_y_continuous(limits = yaxis_scale, breaks = seq(-80, 60, 20)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 13, face = "bold"),
    title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top",
    plot.margin = margin(10, 10, 10, 15),
    panel.grid.minor = element_blank()
  )
plot_vessels

#
# Make a patchwork of the three plots
#

combined_plot <- plot_sets + plot_trips + plot_vessels +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Percent Bias in Catch Rate Estimation by Monitoring Scenario",
    subtitle = "Comparison of Sets, Trips, and Vessels Strategies",
    theme = theme(plot.title = element_text(size = 16, face = "bold"),
                  plot.subtitle = element_text(size = 14))
  )
combined_plot
# Save the plots
ggsave("plots/plot_bias_percent_sets.png", plot = plot_sets,
       width = 8, height = 9, dpi = 300)
ggsave("plots/plot_bias_percent_trips.png", plot = plot_trips,
       width = 8, height = 9, dpi = 300)
ggsave("plots/plot_bias_percent_vessels.png", plot = plot_vessels,
       width = 8, height = 9, dpi = 300)

      ggsave("plots/plot_bias_percent_sets.pdf", plot = plot_sets,
        width = 8, height = 9)
      ggsave("plots/plot_bias_percent_trips.pdf", plot = plot_trips,
        width = 8, height = 9)
      ggsave("plots/plot_bias_percent_vessels.pdf", plot = plot_vessels,
        width = 8, height = 9)
