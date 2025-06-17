# MSC Monitoring Simulation Model - Demonstration Script
# Author: Bilby (R Workshop Expert)
# Description: Demonstrates how to use the MSC monitoring simulation model

# Load all required scripts
source("simulation_engine.R")
source("parameter_management.R")
source("analysis_visualization.R")

# Load additional libraries for the demo
library(ggplot2)
library(dplyr)

cat("=== MSC Monitoring Simulation Model Demo ===\n")

# 1. Load and examine parameters
cat("1. Loading baseline parameters...\n")
baseline_params <- load_parameters("parameters/baseline_parameters.json")
print_parameter_summary(baseline_params)

# 2. Run a single simulation
cat("\n2. Running a single simulation...\n")
single_result <- run_single_simulation(baseline_params)

# Examine the results
cat("Fleet structure:\n")
cat("  - Total vessels:", attr(single_result$fleet, "n_vessels"), "\n")
cat("  - Total trips:", attr(single_result$fleet, "total_trips"), "\n")
cat("  - Total sets:", attr(single_result$fleet, "total_sets"), "\n")

cat("\nCatch data summary:\n")
catch_summary <- single_result$catch_data %>%
  group_by(species) %>%
  summarise(
    total_sets = n(),
    monitored_sets = sum(monitored),
    coverage = round(100 * sum(monitored) / n(), 1),
    mean_catch = round(mean(count), 2),
    .groups = "drop"
  )
print(catch_summary)

# 3. Calculate metrics for single simulation
cat("\n3. Calculating metrics for single simulation...\n")
single_metrics <- calculate_metrics(single_result)
print(single_metrics[, c("species", "relative_bias", "coverage_percentage", "detection_bias")])

# 4. Compare monitoring strategies
cat("\n4. Comparing monitoring strategies (this may take a moment)...\n")
strategy_comparison <- compare_monitoring_strategies(
  baseline_params, 
  strategies = c("random_sets", "random_vessels", "random_trips"),
  n_simulations = 50,  # Reduced for demo speed
  verbose = TRUE
)

# Print summary statistics
print_summary_statistics(strategy_comparison)

# 5. Create visualizations
cat("\n5. Creating visualizations...\n")

# Bias comparison plot
bias_plot <- plot_bias_comparison(strategy_comparison)
ggsave("bias_comparison.png", bias_plot, width = 10, height = 6, dpi = 300)
cat("  - Bias comparison plot saved as 'bias_comparison.png'\n")

# Coverage analysis plot
coverage_plot <- plot_coverage_analysis(strategy_comparison)
ggsave("coverage_analysis.png", coverage_plot, width = 10, height = 6, dpi = 300)
cat("  - Coverage analysis plot saved as 'coverage_analysis.png'\n")

# Detection rates plot
detection_plot <- plot_detection_rates(strategy_comparison)
ggsave("detection_rates.png", detection_plot, width = 10, height = 6, dpi = 300)
cat("  - Detection rates plot saved as 'detection_rates.png'\n")

# 6. Test different scenarios
cat("\n6. Testing different parameter scenarios...\n")

scenarios <- c("baseline", "high_vessel_var", "biased_monitoring")
scenario_results <- list()

for (scenario in scenarios) {
  cat("  Running scenario:", scenario, "\n")
  
  # Load parameters for this scenario
  params <- load_parameters(paste0("parameters/", scenario, "_parameters.json"))
  
  # Run simulations (fewer for demo)
  results <- run_multiple_simulations(params, n_simulations = 20, verbose = FALSE)
  scenario_results[[scenario]] <- results
  
  # Print key results
  key_metrics <- results$summary_statistics %>%
    filter(species != "Overall") %>%
    select(species, mean_relative_bias, mean_coverage)
  
  cat("    Results:\n")
  for (i in 1:nrow(key_metrics)) {
    row <- key_metrics[i, ]
    cat("      ", row$species, ": bias =", round(row$mean_relative_bias, 1), 
        "%, coverage =", round(row$mean_coverage, 1), "%\n")
  }
}

# 7. Save comprehensive results
cat("\n7. Saving results...\n")
save_results(strategy_comparison, "strategy_comparison_results", formats = c("rds", "csv"))

# Create and save summary report
summary_report <- create_summary_report(strategy_comparison, "summary_report.rds")
cat("  - Summary report saved as 'summary_report.rds'\n")

# 8. Demonstrate parameter comparison
cat("\n8. Comparing parameter sets...\n")
baseline_params <- load_parameters("parameters/baseline_parameters.json")
biased_params <- load_parameters("parameters/biased_monitoring_parameters.json")
compare_parameters(baseline_params, biased_params, "Baseline", "Biased Monitoring")

cat("\n=== Demo Complete! ===\n")
cat("Files created:\n")
cat("  - bias_comparison.png\n")
cat("  - coverage_analysis.png\n")
cat("  - detection_rates.png\n")
cat("  - strategy_comparison_results.rds\n")
cat("  - strategy_comparison_results.csv\n")
cat("  - summary_report.rds\n")

cat("\nNext steps:\n")
cat("  1. Examine the generated plots\n")
cat("  2. Modify parameters in the JSON files\n")
cat("  3. Run your own simulations with different scenarios\n")
cat("  4. Use the analysis functions to explore results\n")

cat("\nRemember: Coffee is always better in Australia! ☕\n")