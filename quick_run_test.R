# Quick test to run a small simulation with the tidy functions

# Save output to a file
sink("quick_test_output.txt")

# Load required libraries
library(tidyverse)

# Source the functions
source("simulation_functions.R")

# Read parameter sets
params_df <- read.csv("parameters.csv")

# Set a small number of replications for testing
n_replications <- 5

# Run a simulation for the first parameter set only
params <- as.list(params_df[1, ])
cat("Testing with parameter set:", params$description, "\n\n")

# Initialize storage
param_results <- list()

# Run a few replications
for (rep in 1:n_replications) {
  cat("Running replication", rep, "of", n_replications, "\n")
  
  # Run tidy simulation
  results <- run_single_simulation_tidy(params, rep)
  
  # Store results
  param_results[[rep]] <- results
  
  # Print summary for this replication
  cat("  Strategy: sets, Bias %:", round(results$bias_percent[1], 2), "\n")
  cat("  Strategy: vessels, Bias %:", round(results$bias_percent[2], 2), "\n")
  cat("  Strategy: trips, Bias %:", round(results$bias_percent[3], 2), "\n\n")
}

# Combine results for all replications
all_results <- do.call(rbind, param_results)

# Calculate summary statistics
summary_results <- all_results %>%
  group_by(strategy) %>%
  summarize(
    mean_bias_percent = mean(bias_percent),
    sd_bias_percent = sd(bias_percent),
    lower_ci = quantile(bias_percent, 0.025),
    upper_ci = quantile(bias_percent, 0.975)
  )

cat("Overall results:\n")
print(summary_results)

cat("\nSimulation test complete!\n")

# Close the output file
sink()
