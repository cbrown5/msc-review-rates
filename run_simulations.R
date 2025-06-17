# MSC Review Rates Simulation - Simulation Runner
# This script runs the simulations for each parameter set and saves the results

# Load required libraries
library(tidyverse)

# Source the functions
source("simulation_functions.R")

# Read parameter sets
params_df <- read.csv("parameters.csv")

# Number of replications
n_replications <- 1000

# Initialize results storage
all_results <- list()
summary_results <- list()

# Run simulations for each parameter set
for (i in 1:nrow(params_df)) {
  cat("Running parameter set", i, "of", nrow(params_df), "\n")
  
  # Extract parameters for this set
  params <- as.list(params_df[i, ])
  
  # Initialize storage for this parameter set
  param_results <- list()
  
  # Run replications
  for (rep in 1:n_replications) {
    if (rep %% 100 == 0) {
      cat("  Replication", rep, "of", n_replications, "\n")
    }
    
    # Run single simulation
    param_results[[rep]] <- run_single_simulation(params, rep)
  }
  
  # Combine results for all replications
  all_param_results <- do.call(rbind, param_results)
  
  # Calculate summary statistics
  summary_param_results <- all_param_results %>%
    group_by(parameter_set_id, description, strategy) %>%
    summarize(
      mean_estimated_rate = mean(estimated_catch_rate, na.rm = TRUE),
      mean_true_rate = mean(true_catch_rate, na.rm = TRUE),
      mean_bias = mean(bias, na.rm = TRUE),
      mean_bias_percent = mean(bias_percent, na.rm = TRUE),
      lower_ci = quantile(estimated_catch_rate, 0.025, na.rm = TRUE),
      upper_ci = quantile(estimated_catch_rate, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Store results
  all_results[[i]] <- all_param_results
  summary_results[[i]] <- summary_param_results
}

# Combine all results
all_results_df <- do.call(rbind, all_results)
summary_results_df <- do.call(rbind, summary_results)

# Save results
write.csv(all_results_df, "simulation_results_full.csv", row.names = FALSE)
write.csv(summary_results_df, "simulation_results_summary.csv", row.names = FALSE)

cat("Simulations complete. Results saved to CSV files.\n")