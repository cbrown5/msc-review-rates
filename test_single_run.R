# Test script to verify the tidy monitoring functions work correctly

# Load required libraries
library(tidyverse)

# Source the functions
source("simulation_functions.R")

# Read parameter sets
monitoring_params_df <- read.csv("parameters-monitoring.csv")
spp_params_df <- read.csv("parameters-species.csv")

params_df <- merge(monitoring_params_df, spp_params_df, by = NULL) %>%
  mutate(parameter_set_id = row_number()) 

# Test with the first parameter set (Baseline + Market species)
params <- as.list(params_df[1, ])

cat("Testing parameter set:", params$parameter_set_id, "\n")
cat("Description:", params$description, "\n") 
cat("Species:", params$species, "\n")
cat("Monitoring rate:", params$p_monitor, "\n")
cat("Vessel bias:", params$bias_v, "\n")
cat("Trip bias:", params$bias_factor, "\n\n")


# Generate simulation data
sim_data <- simulate_fleet_catches(params, rep = 1)
catches_df <- sim_data$catches_df

print(head(catches_df))


# Apply set monitoring
catches_set <- apply_set_monitoring_tidy(catches_df, params$p_monitor)
cat("   Number of monitored sets:", sum(catches_set$monitored), "\n")
cat("   Proportion monitored:", round(sum(catches_set$monitored) / nrow(catches_set), 2), "\n\n")

# Apply vessel monitoring
cat("3. Testing vessel-based monitoring...\n")
catches_vessel <- apply_vessel_monitoring_tidy(catches_df, params$p_monitor, params$bias_v)
cat("   Number of monitored sets:", sum(catches_vessel$monitored), "\n")
monitored_vessels <- length(unique(catches_vessel$vessel_id[catches_vessel$monitored == 1]))
cat("   Number of monitored vessels:", monitored_vessels, "\n")
cat("   Proportion of vessels monitored:", round(monitored_vessels / length(unique(catches_vessel$vessel_id)), 2), "\n\n")

# Apply trip monitoring
cat("4. Testing trip-based monitoring...\n")
catches_trip <- apply_trip_monitoring_tidy(catches_df, params$p_monitor, params$bias_factor)
cat("   Number of monitored sets:", sum(catches_trip$monitored), "\n")
monitored_trips <- length(unique(paste(catches_trip$vessel_id, catches_trip$trip_id)[catches_trip$monitored == 1]))
total_trips <- length(unique(paste(catches_trip$vessel_id, catches_trip$trip_id)))
cat("   Number of monitored trips:", monitored_trips, "out of", total_trips, "\n")
cat("   Proportion of trips monitored:", round(monitored_trips / total_trips, 2), "\n\n")

# Calculate catch statistics
cat("5. Testing catch statistics estimation...\n")
stats_sets <- estimate_catch_statistics_tidy(catches_set)
stats_vessels <- estimate_catch_statistics_tidy(catches_vessel)
stats_trips <- estimate_catch_statistics_tidy(catches_trip)

cat("   Set monitoring - True rate:", round(stats_sets$true_catch_rate, 3), 
    ", Estimated rate:", round(stats_sets$estimated_catch_rate, 3), 
    ", Bias %:", round(stats_sets$bias_percent, 2), "\n")
cat("   Vessel monitoring - True rate:", round(stats_vessels$true_catch_rate, 3), 
    ", Estimated rate:", round(stats_vessels$estimated_catch_rate, 3), 
    ", Bias %:", round(stats_vessels$bias_percent, 2), "\n")
cat("   Trip monitoring - True rate:", round(stats_trips$true_catch_rate, 3), 
    ", Estimated rate:", round(stats_trips$estimated_catch_rate, 3), 
    ", Bias %:", round(stats_trips$bias_percent, 2), "\n\n")

# Test the full tidy simulation
cat("6. Testing full tidy simulation function...\n")
results_tidy <- run_single_simulation_tidy(params, 1)
print(results_tidy)

# Extract the attached tidy data objects
tidy_data <- attr(results_tidy, "tidy_data")
cat("\n   Tidy data objects attached to results:", paste(names(tidy_data), collapse=", "), "\n")
cat("   Dimensions of tidy trips data:", dim(tidy_data$trips)[1], "rows,", dim(tidy_data$trips)[2], "columns\n")

