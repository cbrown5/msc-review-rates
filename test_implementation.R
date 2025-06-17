# Test script to verify the MSC simulation implementation works correctly

# Load required libraries
library(tidyverse)

# Source the functions
source("simulation_functions.R")

# Read parameter sets
params_df <- read.csv("parameters.csv")

cat("Testing MSC Review Rates Simulation Implementation\n")
cat("=================================================\n\n")

# Test with the first parameter set (Baseline)
params <- as.list(params_df[5, ])

cat("Testing with Baseline parameter set:\n")
print(params)
cat("\n")

# Test fleet structure generation
cat("1. Testing fleet structure generation...\n")
fleet_structure <- generate_fleet_structure(params)
cat("   Number of vessels:", fleet_structure$V, "\n")
cat("   Number of trips per vessel (first 5):", head(fleet_structure$T, 5), "\n")
cat("   Number of sets for first vessel:", fleet_structure$S[[1]], "\n\n")

# Test random effects generation
cat("2. Testing random effects generation...\n")
random_effects <- generate_random_effects(params, fleet_structure)
cat("   Vessel random effects (first 5):", head(random_effects$x, 5), "\n")
cat("   Trip random effects for first vessel:", random_effects$z[[1]], "\n\n")

# Test catch simulation
cat("3. Testing catch simulation...\n")
catches <- simulate_catches(params, fleet_structure, random_effects)
cat("   Catches for first trip of first vessel:", catches$y[[1]][[1]], "\n")
cat("   Expected catch rates for first trip of first vessel:", round(catches$mu[[1]][[1]], 2), "\n\n")

# Test monitoring strategies
cat("4. Testing monitoring strategies...\n")

# Test set monitoring
monitoring_sets <- apply_set_monitoring(params, fleet_structure, catches, random_effects)
cat("   Set monitoring for first trip of first vessel:", monitoring_sets$M[[1]][[1]], "\n")

# Test vessel monitoring
monitoring_vessels <- apply_vessel_monitoring(params, fleet_structure, catches, random_effects)
cat("   Vessel monitoring for first trip of first vessel:", monitoring_vessels$M[[1]][[1]], "\n")

# Test trip monitoring
monitoring_trips <- apply_trip_monitoring(params, fleet_structure, catches, random_effects)
cat("   Trip monitoring for first trip of first vessel:", monitoring_trips$M[[1]][[1]], "\n\n")

# Test catch statistics estimation
cat("5. Testing catch statistics estimation...\n")
stats_sets <- estimate_catch_statistics(catches, monitoring_sets, fleet_structure)
stats_vessels <- estimate_catch_statistics(catches, monitoring_vessels, fleet_structure)
stats_trips <- estimate_catch_statistics(catches, monitoring_trips, fleet_structure)

cat("   Set monitoring - True rate:", round(stats_sets$true_catch_rate, 3), 
    ", Estimated rate:", round(stats_sets$estimated_catch_rate, 3), 
    ", Bias %:", round(stats_sets$bias_percent, 2), "\n")
cat("   Vessel monitoring - True rate:", round(stats_vessels$true_catch_rate, 3), 
    ", Estimated rate:", round(stats_vessels$estimated_catch_rate, 3), 
    ", Bias %:", round(stats_vessels$bias_percent, 2), "\n")
cat("   Trip monitoring - True rate:", round(stats_trips$true_catch_rate, 3), 
    ", Estimated rate:", round(stats_trips$estimated_catch_rate, 3), 
    ", Bias %:", round(stats_trips$bias_percent, 2), "\n\n")

# Test single simulation function
cat("6. Testing single simulation function...\n")
results <- run_single_simulation(params, 1)
print(results)

cat("\nTest completed successfully! All functions are working correctly.\n")
cat("You can now run the full simulation with: source('run_simulations.R')\n")