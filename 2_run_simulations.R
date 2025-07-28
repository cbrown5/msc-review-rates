# MSC Review Rates Simulation - Tidy Simulation Runner
# This script runs the simulations for each parameter set using the new tidy functions
# It does all combinations of species and monitoring scenarios. 
# Note that the application of vessel or trip bias only applies to monitoring scenarios for vessels or trips. 
# therefore the outcomes with these bias parameters != 0 will be the same for all 'set based' monitoring scenarios. 

# Load required libraries
library(tidyverse)

# Source the functions
source("simulation_functions.R")

# Read parameter sets
params_df <- read.csv("parameters.csv")

# Number of replications
n_replications <- 250

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
    
    # Run single simulation with tidy implementation
    param_results[[rep]] <- run_single_simulation_tidy(params, rep)
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
      lower_ci_mean_bias = quantile(bias, 0.025, na.rm = TRUE),
      upper_ci_mean_bias = quantile(bias, 0.975, na.rm = TRUE),
      lower_ci_mean_bias_percent = quantile(bias_percent, 0.025, na.rm = TRUE),
      upper_ci_mean_bias_percent = quantile(bias_percent, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Store results
  all_results[[i]] <- all_param_results
  summary_results[[i]] <- summary_param_results
}

# Combine all results
all_results_df <- do.call(rbind, all_results)
summary_results_df <- do.call(rbind, summary_results)

# Save results (with tidy suffix to distinguish from original implementation)
write.csv(all_results_df, "outputs/simulation_results_full_tidy.csv", row.names = FALSE)
write.csv(summary_results_df, "outputs/simulation_results_summary_tidy.csv", row.names = FALSE)

cat("Simulations complete. Tidy results saved to CSV files.\n")
