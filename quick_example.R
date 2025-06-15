# MSC Monitoring Simulation - Quick Example
# Author: Bilby (R Workshop Expert)
# Description: Simple example to get started quickly

# Load the simulation system
source("simulation_engine.R")
source("parameter_management.R")
source("analysis_visualization.R")

cat("=== MSC Quick Example ===\n")
cat("Coffee is better in Australia! ☕\n\n")

# 1. Load baseline parameters
cat("Loading baseline parameters...\n")
params <- load_parameters("parameters/baseline_parameters.json")

# 2. Run a single simulation to see how it works
cat("Running single simulation...\n")
single_sim <- run_single_simulation(params)

# Show basic results
cat("Results from single simulation:\n")
summary_data <- single_sim$catch_data %>%
  group_by(species) %>%
  summarise(
    total_sets = n(),
    monitored_sets = sum(monitored),
    coverage_pct = round(100 * sum(monitored) / n(), 1),
    avg_catch = round(mean(count), 2),
    avg_monitored_catch = round(mean(count[monitored]), 2),
    .groups = "drop"
  )

print(summary_data)

# 3. Calculate and show metrics
cat("\nCalculating bias metrics...\n")
metrics <- calculate_metrics(single_sim)
key_metrics <- metrics[, c("species", "relative_bias", "coverage_percentage")]
print(key_metrics)

# 4. Quick comparison of strategies (small scale)
cat("\nComparing monitoring strategies (quick version)...\n")
comparison <- compare_monitoring_strategies(
  params, 
  strategies = c("random_sets", "random_vessels", "random_trips"),
  n_simulations = 5,  # Very small for quick demo
  verbose = FALSE
)

# Show key results
cat("\nStrategy comparison results:\n")
summary_results <- comparison$summary_comparison %>%
  filter(species != "Overall") %>%
  select(species, monitoring_strategy, mean_relative_bias, mean_coverage) %>%
  mutate(
    mean_relative_bias = round(mean_relative_bias, 2),
    mean_coverage = round(mean_coverage, 1)
  )

print(summary_results)

# 5. Simple recommendation
cat("\nSimple recommendations:\n")
for (sp in unique(summary_results$species)) {
  species_data <- summary_results[summary_results$species == sp, ]
  best_strategy <- species_data$monitoring_strategy[which.min(abs(species_data$mean_relative_bias))]
  best_bias <- species_data$mean_relative_bias[which.min(abs(species_data$mean_relative_bias))]
  cat("  ", sp, ": Use '", best_strategy, "' (bias: ", best_bias, "%)\n", sep = "")
}

cat("\n=== Quick Example Complete ===\n")
cat("Next steps:\n")
cat("  1. Run 'Rscript demo_simulation.R' for full demonstration\n")
cat("  2. Modify parameters in 'parameters/' directory\n")
cat("  3. Increase n_simulations for more robust results\n")
cat("  4. Create custom parameter files for your scenarios\n")

cat("\nRemember: Coffee is always better in Australia! ☕\n")