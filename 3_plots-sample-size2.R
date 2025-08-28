# Plot of impact of sample sizes on catch rate estimates

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(patchwork)
theme_set(theme_classic())

add_CIs <- FALSE

#main scenario results 
results_summary_mean <- read.csv("outputs/simulation_results_summary.csv")

#
# Load and wrangle the extra runs with 30% coverage
#

# Read simulation results
monitoring_params_df <- read.csv("parameters-monitoring-scenarios-extra.csv")
spp_params_df <- read.csv("parameters-species.csv")

params_df <- merge(monitoring_params_df, spp_params_df, by = NULL) %>%
mutate(parameter_set_id = row_number()) 

# Read simulation results and filter to compare specific monitoring scenarios
# monitoring_param_set 1-3: 30% vessels/trips with 100% sets
# monitoring_param_set 7-9: 60% vessels/trips with 50% sets  
# write.csv(all_results_df, "outputs/simulation_results_full_tidy-extra.csv", row.names = FALSE)
# write.csv(summary_results_df, "outputs/simulation_results_summary_tidy-extra.csv", row.names = FALSE)

results_summary <- read.csv("outputs/simulation_results_full_tidy-extra.csv")  %>%
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
  left_join(params_df, by = c("description", "parameter_set_id")) 

#Bind ot main scnario results 
results_summary <- bind_rows(results_summary, results_summary_mean)

#strategy is the units that monitoring is allocated by (scenario)
#description is the parameter set from parameters-monitoring-scenarios.csv
# We want to have: 
# "Baseline" (100% coverage, 20% monitoring) with "sets"
# "Baseline 20%" and with vessels 
# "Vessel bias 20%" with "vessels"
# "Baseline 75%" with "trips"
# "Trip bias 75%" with "trips"

scnrs_to_plot <- data.frame(
  strategy = c("sets", "vessels", "vessels", "trips", "trips", "sets", "vessels", "vessels", "trips", 
    "trips", "sets", "vessels", "vessels", "trips", "trips"), 
  description = c("Baseline 30%", "Baseline 30%", "Vessel bias 30%", "Baseline 30%", "Trip bias 30%", #scenarios from the extra runs 
                  "Baseline 20%", "Baseline 20%", "Vessel bias 20%", "Baseline 20%", "Trip bias 20%",
                  "Baseline 100%", "Baseline 100%", "Vessel bias 100%", "Baseline 100%", "Trip bias 100%"
                  ) #main scenarios
)

#up to here, 'trip bias 20%' is going missing

spp_scnrs_to_plot <- merge(scnrs_to_plot, data.frame(species = "Market species"),
      by = NULL) %>%
      arrange(strategy)

table(results_summary$description, results_summary$strategy)

results_plot <- results_summary %>%
   inner_join(spp_scnrs_to_plot)#  %>%
  # change description labels
  # mutate(description = case_when(
  #   description == "Baseline" ~ "Random sets",
  #   description == "Trip bias" ~ "Trip bias",
  #   description == "Vessel bias" ~ "Vessel bias",
  #   TRUE ~ description
  # ))
  table(results_plot$description, results_plot$strategy)
  table(results_plot$description, results_plot$p_monitor)

max(results_plot$upper_ci_mean_bias_percent, na.rm = TRUE)
min(results_plot$lower_ci_mean_bias_percent, na.rm = TRUE)

yaxis_scale <- c(-60, 50)
results_plot$p_monitor
# Market species plot - faceted by p_monitor
results_market <- results_plot %>%
  filter(grepl("Market species", species)) %>%
  mutate(total_sample_size = paste0(as.character(100*p_monitor*p_sets_select),"%"))%>%
  mutate(
    p_monitor_label = paste("Monitoring Rate:", p_monitor),
    colour_scale = str_to_title(paste(strategy, description, sep = " - ")),
        p_monitor = factor(p_monitor, levels = c(0.2, 0.3, 1), labels = c("20%", "30%", "100%"))
  ) %>%
  mutate(Scenario = 
# #Label pattern I want for results_market
 case_when(colour_scale == "Sets - Baseline 20%" ~ "Scnr 1: Random Sets",
          colour_scale == "Sets - Baseline 30%" ~ "Scnr 1: Random Sets",
          colour_scale == "Sets - Baseline 100%" ~ "Scnr 1: Random Sets",
      colour_scale == "Trips - Baseline 20%" ~ "Scnr 3: Randomly \n selected trips",
      colour_scale == "Trips - Trip Bias 20%" ~ "Scnr 3: Bias to low \n catch rate trips",
      colour_scale == "Trips - Trip Bias 30%" ~ "Scnr 3: Bias to low \n catch rate trips",
      colour_scale == "Trips - Baseline 30%" ~ "Scnr 3: Randomly \n selected trips",
      colour_scale == "Trips - Baseline 100%" ~ "Scnr 3: Randomly \n selected trips",
      colour_scale == "Trips - Trip Bias 100%" ~ "Scnr 3: Bias to low \n catch rate trips",
      colour_scale == "Vessels - Baseline 20%" ~ "Scnr 2: Randomly \n selected vessels",
      colour_scale == "Vessels - Baseline 30%" ~ "Scnr 2: Randomly \n selected vessels",
      colour_scale == "Vessels - Baseline 100%" ~ "Scnr 2: Randomly \n selected vessels",
      colour_scale == "Vessels - Vessel Bias 20%" ~ "Scnr 2: Bias to low \n catch rate vessels",
      colour_scale == "Vessels - Vessel Bias 30%" ~ "Scnr 2: Bias to low \n catch rate vessels",
      colour_scale == "Vessels - Vessel Bias 100%" ~ "Scnr 2: Bias to low \n catch rate vessels",
      TRUE ~ as.character(colour_scale)
 )
)

results_market$p_monitor
table(results_market$p_monitor)
table(results_market$Scenario)
table(results_market$colour_scale)
# Labels for results_main_market 
# Scnr 1: Random Sets main study
# Scnr 2: Randomly \n selected vessels main study
# Scnr 2: Bias to low \n catch rate vessels main study
# Scnr 3: Randomly \n selected trips main study
# Scnr 3: Bias to low \n catch rate trips main study

  

plot_market <- ggplot(results_market, aes(x = Scenario, 
  y = mean_bias_percent, color = p_monitor)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = lower_ci_mean_bias_percent, ymax = upper_ci_mean_bias_percent),
      position = position_dodge(width = 0.5), width = 0.25
  ) +
  labs(
    title = "Market Species comparison of scenarios",
    x = "", y = "Percent Bias (%)", color = "Coverage (%)"
  ) +
  # scale_color_canva() +
  scale_y_continuous(limits = yaxis_scale, breaks = seq(-80, 80, 20)) +
  theme_classic() +
  scale_color_brewer(palette = "Set2") +
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

ggsave("plots/plot_bias_percent_sample-size.png", plot = plot_market,
       width = 8, height = 9, dpi = 300)
