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
monitoring_params_df <- read.csv("parameters-monitoring.csv")
spp_params_df <- read.csv("parameters-species.csv")

params_df <- merge(monitoring_params_df, spp_params_df, by = NULL) %>%
mutate(parameter_set_id = row_number()) 

# params_df <- params_df[1:3,]
#Number of replications
n_replications <- 500

# Initialize results storage
all_results <- list()
summary_results <- list()
# Run simulations for each parameter set using purrr::map

run_simulation_for_param <- function(params) {
  # Run replications for a single parameter set
  
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
  
  all_param_results <- bind_rows(param_results)
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
  
  list(all_param_results = all_param_results, summary_param_results = summary_param_results)
}


system.time(
simulation_results <- parallel::mclapply(1:nrow(params_df), function(x) {
  params <- params_df[x, ]
  cat("Running parameter set", params$parameter_set_id, "of", nrow(params_df), "\n")
  run_simulation_for_param(params)
}, 
mc.cores = 8, mc.set.seed = FALSE)
)

# # Version if parallel::mclapply is not available
# simulation_results <- lapply(1:nrow(params_df), function(x) {
#   params <- params_df[x, ]
#   cat("Running parameter set", params$parameter_set_id, "of", nrow(params_df), "\n")
#   run_simulation_for_param(params)
# })


all_results <- purrr::map(simulation_results, "all_param_results")
summary_results <- purrr::map(simulation_results, "summary_param_results")

# Combine all results
all_results_df <- do.call(rbind, all_results)
summary_results_df <- do.call(rbind, summary_results) %>%
  left_join(params_df, by = c("description", "parameter_set_id"))

# Save results (with tidy suffix to distinguish from original implementation)
write.csv(all_results_df, "outputs/simulation_results_full_tidy.csv", row.names = FALSE)
write.csv(summary_results_df, "outputs/simulation_results_summary_tidy.csv", row.names = FALSE)

cat("Simulations complete. Tidy results saved to CSV files.\n")
