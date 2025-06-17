# MSC Monitoring Simulation Model - Parameter Management
# Author: Bilby (R Workshop Expert)
# Description: Functions for loading, validating, and managing simulation parameters

# Load required libraries
library(jsonlite)
library(dplyr)

#' Load Parameters from File
#' 
#' Loads and validates parameters from a JSON file
#' 
#' @param parameter_file Path to JSON parameter file
#' @param validate Whether to validate parameters (default: TRUE)
#' @return List of validated parameters
#' @export
load_parameters <- function(parameter_file, validate = TRUE) {
  
  # Check if file exists
  if (!file.exists(parameter_file)) {
    stop("Parameter file not found: ", parameter_file)
  }
  
  # Load JSON file
  tryCatch({
    parameters <- fromJSON(parameter_file, simplifyVector = FALSE)
  }, error = function(e) {
    stop("Error reading JSON file: ", e$message)
  })
  
  # Validate if requested
  if (validate) {
    parameters <- validate_parameters(parameters)
  }
  
  return(parameters)
}

#' Validate Parameters
#' 
#' Validates parameter structure and values
#' 
#' @param parameters List of parameters to validate
#' @return Validated parameters with defaults filled in
#' @export
validate_parameters <- function(parameters) {
  
  # Check main structure
  required_sections <- c("simulation_parameters", "catch_model_parameters", "monitoring_parameters")
  missing_sections <- setdiff(required_sections, names(parameters))
  
  if (length(missing_sections) > 0) {
    stop("Missing required parameter sections: ", paste(missing_sections, collapse = ", "))
  }
  
  # Validate simulation parameters
  parameters$simulation_parameters <- validate_simulation_parameters(parameters$simulation_parameters)
  
  # Validate catch model parameters
  parameters$catch_model_parameters <- validate_catch_model_parameters(parameters$catch_model_parameters)
  
  # Validate monitoring parameters
  parameters$monitoring_parameters <- validate_monitoring_parameters(parameters$monitoring_parameters)
  
  return(parameters)
}

#' Validate Simulation Parameters
#' 
#' @param sim_params Simulation parameters section
#' @return Validated simulation parameters
validate_simulation_parameters <- function(sim_params) {
  
  # Set defaults
  defaults <- list(
    n_simulations = 1000,
    seed = NULL,
    fleet_size = list(
      vessels = 100,
      trips_per_vessel = 10,
      sets_per_trip = 5
    )
  )
  
  # Merge with defaults
  sim_params <- merge_with_defaults(sim_params, defaults)
  
  # Validate values
  if (sim_params$n_simulations <= 0) {
    stop("n_simulations must be positive")
  }
  
  if (sim_params$fleet_size$vessels <= 0) {
    stop("Number of vessels must be positive")
  }
  
  if (sim_params$fleet_size$trips_per_vessel <= 0) {
    stop("Number of trips per vessel must be positive")
  }
  
  if (sim_params$fleet_size$sets_per_trip <= 0) {
    stop("Number of sets per trip must be positive")
  }
  
  return(sim_params)
}

#' Validate Catch Model Parameters
#' 
#' @param catch_params Catch model parameters section
#' @return Validated catch model parameters
validate_catch_model_parameters <- function(catch_params) {
  
  # Check species list exists
  if (is.null(catch_params$species) || length(catch_params$species) == 0) {
    stop("At least one species must be specified")
  }
  
  # Validate each species
  for (i in seq_along(catch_params$species)) {
    species <- catch_params$species[[i]]
    
    # Set defaults for species
    species_defaults <- list(
      name = paste0("Species_", i),
      distribution = "poisson",
      beta0 = 0,
      sigma2_vessel = 0.1,
      sigma2_trip = 0.1,
      sigma2_residual = 0.1,
      phi = 1,  # for negative binomial
      n_trials = 100  # for binomial
    )
    
    species <- merge_with_defaults(species, species_defaults)
    
    # Validate distribution
    valid_distributions <- c("poisson", "negative_binomial", "binomial", "zero_inflated")
    if (!species$distribution %in% valid_distributions) {
      stop("Invalid distribution for species ", species$name, ": ", species$distribution)
    }
    
    # Validate variance parameters
    if (species$sigma2_vessel < 0) {
      stop("sigma2_vessel must be non-negative for species ", species$name)
    }
    
    if (species$sigma2_trip < 0) {
      stop("sigma2_trip must be non-negative for species ", species$name)
    }
    
    if (species$sigma2_residual < 0) {
      stop("sigma2_residual must be non-negative for species ", species$name)
    }
    
    # Validate distribution-specific parameters
    if (species$distribution == "negative_binomial" && species$phi <= 0) {
      stop("phi must be positive for negative binomial distribution (species ", species$name, ")")
    }
    
    if (species$distribution == "binomial" && (species$n_trials <= 0 || species$n_trials != round(species$n_trials))) {
      stop("n_trials must be a positive integer for binomial distribution (species ", species$name, ")")
    }
    
    catch_params$species[[i]] <- species
  }
  
  return(catch_params)
}

#' Validate Monitoring Parameters
#' 
#' @param monitor_params Monitoring parameters section
#' @return Validated monitoring parameters
validate_monitoring_parameters <- function(monitor_params) {
  
  # Set defaults
  defaults <- list(
    coverage_percentage = 30,
    strategy = "random_sets",
    bias_parameters = list(
      delta_monitoring = 0,
      gamma_selection = 0
    )
  )
  
  # Merge with defaults
  monitor_params <- merge_with_defaults(monitor_params, defaults)
  
  # Validate coverage percentage
  if (monitor_params$coverage_percentage < 0 || monitor_params$coverage_percentage > 100) {
    stop("coverage_percentage must be between 0 and 100")
  }
  
  # Validate strategy
  valid_strategies <- c("random_sets", "random_vessels", "random_trips")
  if (!monitor_params$strategy %in% valid_strategies) {
    stop("Invalid monitoring strategy: ", monitor_params$strategy)
  }
  
  return(monitor_params)
}

#' Merge Parameters with Defaults
#' 
#' Recursively merges parameter list with defaults
#' 
#' @param params User parameters
#' @param defaults Default parameters
#' @return Merged parameters
merge_with_defaults <- function(params, defaults) {
  
  if (is.null(params)) {
    return(defaults)
  }
  
  for (name in names(defaults)) {
    if (is.null(params[[name]])) {
      params[[name]] <- defaults[[name]]
    } else if (is.list(defaults[[name]]) && is.list(params[[name]])) {
      params[[name]] <- merge_with_defaults(params[[name]], defaults[[name]])
    }
  }
  
  return(params)
}

#' Create Default Parameter Set
#' 
#' Creates a default parameter set for testing
#' 
#' @param scenario Scenario name ("baseline", "high_vessel_var", "high_trip_var", "biased_monitoring", "rare_events")
#' @return List of parameters
#' @export
create_default_parameters <- function(scenario = "baseline") {
  
  base_params <- list(
    simulation_parameters = list(
      n_simulations = 100,
      seed = 12345,
      fleet_size = list(
        vessels = 50,
        trips_per_vessel = 8,
        sets_per_trip = 6
      )
    ),
    catch_model_parameters = list(
      species = list(
        list(
          name = "Common_Tuna",
          distribution = "poisson",
          beta0 = 2.5,
          sigma2_vessel = 0.2,
          sigma2_trip = 0.15,
          sigma2_residual = 0.1
        ),
        list(
          name = "Rare_Shark",
          distribution = "negative_binomial",
          beta0 = 0.5,
          sigma2_vessel = 0.3,
          sigma2_trip = 0.2,
          sigma2_residual = 0.15,
          phi = 1.5
        )
      )
    ),
    monitoring_parameters = list(
      coverage_percentage = 30,
      strategy = "random_sets",
      bias_parameters = list(
        delta_monitoring = 0,
        gamma_selection = 0
      )
    )
  )
  
  # Modify based on scenario
  params <- switch(scenario,
    "baseline" = base_params,
    
    "high_vessel_var" = {
      base_params$catch_model_parameters$species[[1]]$sigma2_vessel <- 0.5
      base_params$catch_model_parameters$species[[2]]$sigma2_vessel <- 0.6
      base_params
    },
    
    "high_trip_var" = {
      base_params$catch_model_parameters$species[[1]]$sigma2_trip <- 0.4
      base_params$catch_model_parameters$species[[2]]$sigma2_trip <- 0.5
      base_params
    },
    
    "biased_monitoring" = {
      base_params$monitoring_parameters$bias_parameters$delta_monitoring <- 0.5
      base_params
    },
    
    "rare_events" = {
      base_params$catch_model_parameters$species[[2]]$distribution <- "binomial"
      base_params$catch_model_parameters$species[[2]]$beta0 <- -2
      base_params$catch_model_parameters$species[[2]]$n_trials <- 50
      base_params
    },
    
    stop("Unknown scenario: ", scenario)
  )
  
  return(params)
}

#' Save Parameters to File
#' 
#' Saves parameters to a JSON file
#' 
#' @param parameters Parameter list
#' @param filename Output filename
#' @param pretty Whether to format JSON nicely (default: TRUE)
#' @export
save_parameters <- function(parameters, filename, pretty = TRUE) {
  
  # Validate parameters before saving
  parameters <- validate_parameters(parameters)
  
  # Write to JSON file
  tryCatch({
    write_json(parameters, filename, pretty = pretty, auto_unbox = TRUE)
    message("Parameters saved to: ", filename)
  }, error = function(e) {
    stop("Error writing parameter file: ", e$message)
  })
  
  invisible(filename)
}

#' Generate All Default Parameter Files
#' 
#' Creates all default parameter files in the specified directory
#' 
#' @param output_dir Directory to save parameter files (default: "parameters")
#' @export
generate_default_parameter_files <- function(output_dir = "parameters") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  scenarios <- c("baseline", "high_vessel_var", "high_trip_var", "biased_monitoring", "rare_events")
  
  for (scenario in scenarios) {
    params <- create_default_parameters(scenario)
    filename <- file.path(output_dir, paste0(scenario, "_parameters.json"))
    save_parameters(params, filename)
  }
  
  message("Generated ", length(scenarios), " parameter files in: ", output_dir)
  invisible(output_dir)
}

#' Print Parameter Summary
#' 
#' Prints a summary of the parameters
#' 
#' @param parameters Parameter list
#' @export
print_parameter_summary <- function(parameters) {
  
  cat("=== MSC Simulation Parameters Summary ===\n\n")
  
  # Simulation parameters
  sim <- parameters$simulation_parameters
  cat("Simulation Settings:\n")
  cat("  - Number of simulations:", sim$n_simulations, "\n")
  cat("  - Random seed:", ifelse(is.null(sim$seed), "Not set", sim$seed), "\n")
  cat("  - Fleet size:", sim$fleet_size$vessels, "vessels\n")
  cat("  - Trips per vessel:", sim$fleet_size$trips_per_vessel, "\n")
  cat("  - Sets per trip:", sim$fleet_size$sets_per_trip, "\n")
  
  # Catch model parameters
  catch <- parameters$catch_model_parameters
  cat("\nCatch Model:\n")
  cat("  - Number of species:", length(catch$species), "\n")
  
  for (i in seq_along(catch$species)) {
    species <- catch$species[[i]]
    cat("  - Species", i, "(", species$name, "):\n")
    cat("    * Distribution:", species$distribution, "\n")
    cat("    * Baseline rate (beta0):", species$beta0, "\n")
    cat("    * Vessel variance:", species$sigma2_vessel, "\n")
    cat("    * Trip variance:", species$sigma2_trip, "\n")
    cat("    * Residual variance:", species$sigma2_residual, "\n")
  }
  
  # Monitoring parameters
  monitor <- parameters$monitoring_parameters
  cat("\nMonitoring Strategy:\n")
  cat("  - Coverage percentage:", monitor$coverage_percentage, "%\n")
  cat("  - Strategy:", monitor$strategy, "\n")
  cat("  - Monitoring bias (delta):", monitor$bias_parameters$delta_monitoring, "\n")
  cat("  - Selection bias (gamma):", monitor$bias_parameters$gamma_selection, "\n")
  
  cat("\n===========================================\n")
}

#' Compare Parameters
#' 
#' Compares two parameter sets and highlights differences
#' 
#' @param params1 First parameter set
#' @param params2 Second parameter set
#' @param name1 Name for first parameter set (default: "Set 1")
#' @param name2 Name for second parameter set (default: "Set 2")
#' @export
compare_parameters <- function(params1, params2, name1 = "Set 1", name2 = "Set 2") {
  
  cat("=== Parameter Comparison ===\n")
  cat("Comparing:", name1, "vs", name2, "\n\n")
  
  # Helper function to compare nested lists
  compare_nested <- function(p1, p2, path = "") {
    all_keys <- unique(c(names(p1), names(p2)))
    
    for (key in all_keys) {
      current_path <- if (path == "") key else paste(path, key, sep = "$")
      
      if (is.null(p1[[key]]) && !is.null(p2[[key]])) {
        cat("DIFF:", current_path, "- Missing in", name1, "\n")
      } else if (!is.null(p1[[key]]) && is.null(p2[[key]])) {
        cat("DIFF:", current_path, "- Missing in", name2, "\n")
      } else if (is.list(p1[[key]]) && is.list(p2[[key]])) {
        compare_nested(p1[[key]], p2[[key]], current_path)
      } else if (!identical(p1[[key]], p2[[key]])) {
        cat("DIFF:", current_path, "-", name1, ":", p1[[key]], "vs", name2, ":", p2[[key]], "\n")
      }
    }
  }
  
  compare_nested(params1, params2)
  cat("\n=========================\n")
}