# Looks at comparison of effects for different total sample sizes 

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

results_main <- read.csv("outputs/results_all_summary.csv")

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
  mutate(total_sample_size = paste0(as.character(100*p_monitor*p_sets_select),"%"))%>%
  mutate(
    p_monitor_label = paste("Monitoring Rate:", p_monitor),
    colour_scale = str_to_title(paste(strategy, description, sep = " - ")),
        p_monitor = factor(p_monitor, levels = c(0.2, 0.3), labels = c("20%", "30%"))

  ) %>%
  mutate(Scenario = 
#Label pattern I want for results_market
 case_when(colour_scale == "Sets - Baseline" & p_monitor == "20%" ~ "Scnr 1: Random Sets",
          colour_scale == "Sets - Baseline" & p_monitor == "30%" ~ "Scnr 1: Random Sets",
       colour_scale == "Trips - Baseline" & p_monitor == "20%" ~ "Scnr 3: Randomly \n selected trips",
      colour_scale == "Trips - Trip Bias" & p_monitor == "20%" ~ "Scnr 3: Bias to low \n catch rate trips",
      colour_scale == "Trips - Trip Bias" & p_monitor == "30%" ~ "Scnr 3: Bias to low \n catch rate trips",
      colour_scale == "Trips - Baseline" & p_monitor == "30%" ~ "Scnr 3: Randomly \n selected trips",
      colour_scale == "Vessels - Baseline" & p_monitor == "20%" ~ "Scnr 2: Randomly \n selected vessels",
      colour_scale == "Vessels - Baseline" & p_monitor == "30%" ~ "Scnr 2: Randomly \n selected vessels",
      colour_scale == "Vessels - Vessel Bias" & p_monitor == "20%" ~ "Scnr 2: Bias to low \n catch rate vessels",
      colour_scale == "Vessels - Vessel Bias" & p_monitor == "30%" ~ "Scnr 2: Bias to low \n catch rate vessels",
      TRUE ~ as.character(colour_scale)
 )
  )
results_market$Scenario
# Labels for results_main_market 
# Scnr 1: Random Sets main study
# Scnr 2: Randomly \n selected vessels main study
# Scnr 2: Bias to low \n catch rate vessels main study
# Scnr 3: Randomly \n selected trips main study
# Scnr 3: Bias to low \n catch rate trips main study

results_main_market <- results_main %>%
  filter(species == "Market species") %>%
  filter(!is.na(colour_scale)) %>%
  mutate(Scenario = paste(colour_scale)) %>%
  mutate(total_sample_size = paste0(as.character(100*p_monitor*p_sets_select),"%")) %>%
  select(-p_monitor) %>%
  filter(total_sample_size != "4%") #remove teh duplicate scenario

# Combine the datasets
results_market_combined <- bind_rows(results_main_market, results_market) %>%
  # Filter to only market species (no variance additions)
  filter(species == "Market species") %>%
  # Ensure total_sample_size is a factor with the correct order
  mutate(total_sample_size = factor(total_sample_size, 
    levels = c("4%", "6%", "15%", "20%"))) %>%
  arrange(Scenario, total_sample_size)
  

plot_market <- ggplot(results_market_combined, aes(x = Scenario, 
y = mean_bias_percent, color = total_sample_size)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_ci_mean_bias_percent, ymax = upper_ci_mean_bias_percent),
    position = position_dodge(width = 0.5), width = 0.25
  ) +
  labs(
    title = "Market Species comparison of scenarios",
    x = "", y = "Percent Bias (%)", color = "Total % of sets reviewed"
  ) +
  # scale_color_canva() +
  scale_y_continuous(limits = yaxis_scale, breaks = seq(-80, 80, 20)) +
  theme_classic() +
  scale_colour_brewer(palette = "RdBu") +
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
