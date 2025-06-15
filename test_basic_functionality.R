# MSC Monitoring Simulation Model - Basic Functionality Test
# Author: Bilby (R Workshop Expert)
# Description: Quick test to verify the basic functionality works

# Load required scripts
source("simulation_engine.R")
source("parameter_management.R")

cat("=== Testing Basic Functionality ===\n")
cat("Coffee is better in Australia! ☕\n\n")

# Test 1: Parameter loading
cat("Test 1: Loading parameters...\n")
tryCatch({
  params <- load_parameters("parameters/baseline_parameters.json")
  cat("  ✓ Parameters loaded successfully\n")
  cat("  - Fleet size:", params$simulation_parameters$fleet_size$vessels, "vessels\n")
  cat("  - Species count:", length(params$catch_model_parameters$species), "\n")
}, error = function(e) {
  cat("  ✗ Error loading parameters:", e$message, "\n")
  stop("Cannot proceed without parameters")
})

# Test 2: Fleet generation
cat("\nTest 2: Generating fleet...\n")
tryCatch({
  fleet <- generate_fleet(
    n_vessels = 10,
    n_trips_per_vessel = 5,
    n_sets_per_trip = 50
  )
  cat("  ✓ Fleet generated successfully\n")
  cat("  - Vessels:", attr(fleet, "n_vessels"), "\n")
  cat("  - Total trips:", attr(fleet, "total_trips"), "\n")
  cat("  - Total sets:", attr(fleet, "total_sets"), "\n")
}, error = function(e) {
  cat("  ✗ Error generating fleet:", e$message, "\n")
})

# Test 3: Random effects generation
cat("\nTest 3: Generating random effects...\n")
tryCatch({
  species_params <- setNames(params$catch_model_parameters$species, 
                           sapply(params$catch_model_parameters$species, function(x) x$name))
  random_effects <- generate_random_effects(fleet, species_params)
  cat("  ✓ Random effects generated successfully\n")
  cat("  - Species with effects:", length(random_effects$vessel), "\n")
  cat("  - Vessel effects for first species:", length(random_effects$vessel[[1]]), "\n")
}, error = function(e) {
  cat("  ✗ Error generating random effects:", e$message, "\n")
})

# Test 4: Catch data generation
cat("\nTest 4: Generating catch data...\n")
tryCatch({
  catch_data <- generate_catch_data(fleet, random_effects, species_params)
  cat("  ✓ Catch data generated successfully\n")
  cat("  - Total observations:", nrow(catch_data), "\n")
  cat("  - Species:", paste(unique(catch_data$species), collapse = ", "), "\n")
  
  # Show sample of data
  cat("  - Sample data:\n")
  sample_data <- catch_data[1:3, c("vessel_id", "species", "count", "monitored")]
  print(sample_data)
}, error = function(e) {
  cat("  ✗ Error generating catch data:", e$message, "\n")
})

# Test 5: Monitoring strategies
cat("\nTest 5: Testing monitoring strategies...\n")
strategies <- c("random_sets", "random_vessels", "random_trips")

for (strategy in strategies) {
  tryCatch({
    monitored_data <- apply_monitoring_strategy(
      catch_data, 
      strategy = strategy, 
      coverage_percentage = 30
    )
    coverage <- 100 * sum(monitored_data$monitored) / nrow(monitored_data)
    cat("  ✓", strategy, "- Coverage:", round(coverage, 1), "%\n")
  }, error = function(e) {
    cat("  ✗", strategy, "- Error:", e$message, "\n")
  })
}

# Test 6: Single simulation
cat("\nTest 6: Running single simulation...\n")
tryCatch({
  # Use smaller parameters for quick test
  test_params <- params
  test_params$simulation_parameters$fleet_size$vessels <- 5
  test_params$simulation_parameters$fleet_size$trips_per_vessel <- 3
  test_params$simulation_parameters$fleet_size$sets_per_trip <- 2
  
  result <- run_single_simulation(test_params)
  cat("  ✓ Single simulation completed successfully\n")
  cat("  - Catch data rows:", nrow(result$catch_data), "\n")
  cat("  - Monitored sets:", sum(result$catch_data$monitored), "\n")
}, error = function(e) {
  cat("  ✗ Error in single simulation:", e$message, "\n")
})

cat("\n=== Basic Functionality Test Complete ===\n")
cat("If all tests passed, you can run the full demo with:\n")
cat("  Rscript demo_simulation.R\n")