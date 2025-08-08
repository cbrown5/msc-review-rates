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
monitoring_params_df <- read.csv("parameters-monitoring.csv")
spp_params_df <- read.csv("parameters-species.csv")

params_df <- merge(monitoring_params_df, spp_params_df, by = NULL) %>%
mutate(parameter_set_id = row_number()) 

# Read simulation results and filter to compare specific monitoring scenarios
# monitoring_param_set 1-3: 30% vessels/trips with 100% sets
# monitoring_param_set 7-9: 60% vessels/trips with 50% sets  
results_summary <- read.csv("outputs/simulation_results_full_tidy.csv")  %>%
  mutate(catch_real = true_catch_rate * total_sets,
         catch_estimated = estimated_catch_rate * total_sets,
         # Calculate catch missed
         catch_missed = catch_real - catch_estimated) %>%
    group_by(parameter_set_id, description, strategy) %>%
    summarize(
      mean_estimated_rate = mean(estimated_catch_rate, na.rm = TRUE),
      mean_monitored_sets = mean(monitored_sets, na.rm = TRUE),
      mean_true_rate = mean(true_catch_rate, na.rm = TRUE),
      mean_true_catch = mean(true_catch_rate * total_sets, na.rm = TRUE),
      mean_estimated_catch = mean(estimated_catch_rate * total_sets, na.rm = TRUE),
      mean_catch_real = mean(catch_real, na.rm = TRUE),
      lower_ci_catch_real = quantile(catch_real, 0.025, na.rm = TRUE),
      upper_ci_catch_real = quantile(catch_real, 0.975, na.rm = TRUE),
      mean_catch_estimated = mean(catch_estimated, na.rm = TRUE),
      lower_ci_catch_estimated = quantile(catch_estimated, 0.025, na.rm = TRUE),
      upper_ci_catch_estimated = quantile(catch_estimated, 0.975, na.rm = TRUE),
      mean_catch_missed = mean(catch_missed, na.rm = TRUE),
      lower_ci_catch_missed = quantile(catch_missed, 0.025, na.rm = TRUE),
      upper_ci_catch_missed = quantile(catch_missed, 0.975, na.rm = TRUE),
      mean_bias = mean(bias, na.rm = TRUE),
      mean_bias_percent = mean(bias_percent, na.rm = TRUE),
      lower_ci_mean_bias = quantile(bias, 0.025, na.rm = TRUE),
      upper_ci_mean_bias = quantile(bias, 0.975, na.rm = TRUE),
      lower_ci_mean_bias_percent = quantile(bias_percent, 0.025, na.rm = TRUE),
      upper_ci_mean_bias_percent = quantile(bias_percent, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
  left_join(params_df, by = c("description", "parameter_set_id")) %>%
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

yaxis_scale <- c(-100, 170)

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
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 15),
    panel.grid.minor = element_blank()
  )
plot_sets



# Market species plot - mean monitored sets
plot_sets_monitored <- ggplot(results_sets, aes(x = species, y = mean_monitored_sets, color = description)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = position_dodge(width = 0.5))


# Market species plot - mean catch missed
plot_market_missed <- ggplot(results_sets, aes(x = species, y = mean_catch_missed, color = description)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
   geom_errorbar(aes(ymin = lower_ci_catch_missed, ymax = upper_ci_catch_missed),
    position = position_dodge(width = 0.5), width = 0.25) +
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
# Trips plot - trips and sets strategies
#

results_trips <- results_plot %>%
  filter(strategy %in% c("sets", "trips"))%>%
  mutate(
    colour_scale = str_to_title(paste(strategy, description, sep = " - ")),
    colour_scale = case_when(colour_scale == "Sets - Random Sets" ~ "Scnr 1: Random Sets",
       colour_scale == "Trips - Random Sets" ~ "Scnr 2: Randomly selected \n trips",
       colour_scale == "Trips - Trip Bias" ~ "Scnr 2: Bias to low \n catch rate trips",
       TRUE ~ colour_scale)
  ) %>%
  arrange(species, description) 

results_trips$mean_true_catch

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
# Trips plot - mean monitored sets
#
plot_trips_monitored <- ggplot(results_trips) + 
  aes(x = species, y = mean_monitored_sets, color = colour_scale,
  group = colour_scale) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = pd2) +
  labs(
    title = "Trips Scenario: Mean Monitored Sets",
    subtitle = "With Sets Scenario as Reference",
    x = "", y = "Mean Monitored Sets", color = "Monitoring Scenario"
  ) +
  scale_color_canva() +
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


#
# Vessels plot - vessels and sets strategies
#

results_vessels <- results_plot %>%
  filter(strategy %in% c("sets", "vessels")) %>%
  mutate(
    colour_scale = str_to_title(paste(strategy, description, sep = " - ")),
     colour_scale = case_when(colour_scale == "Sets - Random Sets" ~ "Scnr 1: Random Sets",
       colour_scale == "Vessels - Random Sets" ~ "Scnr 3: Randomly selected \n vessels",
       colour_scale == "Vessels - Vessel Bias" ~ "Scnr 3: Bias to low \n catch rate vessels",
       TRUE ~ colour_scale)
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
# Vessels plot - mean monitored sets
#
plot_vessels_monitored <- ggplot(results_vessels) + 
  aes(x = species, y = mean_monitored_sets, color = colour_scale,
  group = colour_scale) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = pd3) +
  labs(
    title = "Vessels Scenario: Mean Monitored Sets",
    subtitle = "With Sets Scenario as Reference",
    x = "", y = "Mean Monitored Sets", color = "Monitoring Scenario"
  ) +
  scale_color_canva() +
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

#
# Vessels plot - missed catch
#
plot_vessels_missed <- ggplot(results_vessels) + 
  aes(x = species, y = mean_catch_missed, color = colour_scale,
  group = colour_scale) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = pd3) +
  geom_errorbar(aes(ymin = lower_ci_catch_missed, ymax = upper_ci_catch_missed),
    position = pd3, width = 0.25) +
  labs(
    title = "Vessels Scenario: Mean Catch Missed",
    subtitle = "With Sets Scenario as Reference",
    x = "", y = "Mean Catch Missed", color = "Monitoring Scenario"
  ) +
  scale_color_canva() +
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
plot_vessels_missed

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

# Create comprehensive table of catch missed numbers for all monitoring scenarios
catch_missed_table <- results_summary %>%
  inner_join(spp_scnrs_to_plot) %>%
  mutate(description = case_when(
    description == "Baseline" ~ "Random sets",
    description == "Trip bias" ~ "Trip bias",
    description == "Vessel bias" ~ "Vessel bias",
    TRUE ~ description
  )) %>%
  select(
    species,
    strategy,
    description,
    mean_catch_missed,
    lower_ci_catch_missed,
    upper_ci_catch_missed,
    mean_catch_real,
    lower_ci_catch_real,
    upper_ci_catch_real,
    mean_catch_estimated,
    lower_ci_catch_estimated,
    upper_ci_catch_estimated,
    mean_monitored_sets,
    mean_true_catch,
    mean_estimated_catch,
    mean_bias_percent,
    p_monitor
  ) %>%
  arrange(species, strategy, description)

# Save the table as CSV
write.csv(catch_missed_table, "outputs/catch_missed_summary_table.csv", row.names = FALSE)

# Also create a wide format table for easier comparison
catch_missed_wide <- catch_missed_table %>%
  select(species, strategy, description, mean_catch_missed) %>%
  pivot_wider(
    names_from = c(strategy, description),
    values_from = mean_catch_missed,
    names_sep = "_"
  )

write.csv(catch_missed_wide, "outputs/catch_missed_wide_table.csv", row.names = FALSE)

#Wide table of total catch true

catch_wide <- results_summary %>%
  select(species, strategy, description, mean_true_catch) %>%
  pivot_wider(
    names_from = c(strategy, description),
    values_from = mean_true_catch,
    names_sep = "_"
  )
catch_wide
