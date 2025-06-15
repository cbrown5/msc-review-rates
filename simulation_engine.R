# MSC Monitoring Simulation Model - Core Simulation Engine
# Author: Bilby (R Workshop Expert)
# Description: Core functions for simulating catch events and monitoring strategies

# Load required libraries
library(dplyr)
library(purrr)
library(stats)

#' Generate Fleet Structure
#' 
#' Creates a hierarchical fleet structure with vessels, trips, and sets
#' 
#' @param n_vessels Number of vessels in the fleet
#' @param n_trips_per_vessel Number of trips per vessel (can be vector for variability)
#' @param n_sets_per_trip Number of sets per trip (can be vector for variability)
#' @param company_structure Optional list defining company affiliations
#' @return A nested list representing the fleet structure
#' @export
generate_fleet <- function(n_vessels, 
                          n_trips_per_vessel = 10, 
                          n_sets_per_trip = 5,
                          company_structure = NULL) {
  
    sets_per_trip <- n_sets_per_trip
  
  # Generate fleet structure
  fleet <- list()
  trips_per_vessel <- rpois(n_vessels, n_trips_per_vessel)
  for (v in 1:n_vessels) {
    vessel_id <- paste0("vessel_", sprintf("%03d", v))
    
    # Generate trips for this vessel
    trips <- list()
    n_trips <- trips_per_vessel[v]
    
    for (t in 1:n_trips) {
      trip_id <- paste0(vessel_id, "_trip_", sprintf("%02d", t))
    
        n_sets <- sample(sets_per_trip, 1)
      
      sets <- 1:n_sets
      names(sets) <- paste0(trip_id, "_set_", sprintf("%02d", sets))
      
      trips[[trip_id]] <- list(
        sets = sets,
        n_sets = n_sets
      )
    }
    
    fleet[[vessel_id]] <- list(
      trips = trips,
      n_trips = n_trips,
      company = NA
    )
  }
  
  # Add fleet metadata
  attr(fleet, "n_vessels") <- n_vessels
  attr(fleet, "total_trips") <- sum(trips_per_vessel)
  attr(fleet, "total_sets") <- sum(sapply(fleet, function(v) {
    sum(sapply(v$trips, function(t) t$n_sets))
  }))
  
  return(fleet)
}

#' Generate Random Effects
#' 
#' Generates hierarchical random effects for vessels, trips, and residuals
#' 
#' @param fleet Fleet structure from generate_fleet()
#' @param species_parameters List of parameters for each species
#' @return List of random effects for each hierarchical level
#' @export
generate_random_effects <- function(fleet, species_parameters) {
  
  # Validate inputs
  if (!is.list(fleet)) stop("Fleet must be a list")
  if (!is.list(species_parameters)) stop("Species parameters must be a list")
  
  n_vessels <- attr(fleet, "n_vessels")
  vessel_ids <- names(fleet)
  
  # Initialize random effects structure
  random_effects <- list(
    vessel = list(),
    trip = list(),
    residual = list()
  )
  
  # Generate random effects for each species
  for (species in names(species_parameters)) {
    params <- species_parameters[[species]]
    
    # Vessel-level random effects
    sigma_vessel <- sqrt(params$sigma2_vessel %||% 0.1)
    vessel_effects <- rnorm(n_vessels, mean = 0, sd = sigma_vessel)
    names(vessel_effects) <- vessel_ids
    random_effects$vessel[[species]] <- vessel_effects
    
    # Trip-level random effects
    sigma_trip <- sqrt(params$sigma2_trip %||% 0.1)
    trip_effects <- list()
    
    for (vessel_id in vessel_ids) {
      vessel <- fleet[[vessel_id]]
      trip_ids <- names(vessel$trips)
      n_trips <- length(trip_ids)
      
      trip_effects_vessel <- rnorm(n_trips, mean = 0, sd = sigma_trip)
      names(trip_effects_vessel) <- trip_ids
      trip_effects <- c(trip_effects, trip_effects_vessel)
    }
    
    random_effects$trip[[species]] <- trip_effects
    
    # Set-level residual effects
    sigma_residual <- sqrt(params$sigma2_residual %||% 0.1)
    residual_effects <- list()
    
    for (vessel_id in vessel_ids) {
      vessel <- fleet[[vessel_id]]
      for (trip_id in names(vessel$trips)) {
        trip <- vessel$trips[[trip_id]]
        set_ids <- names(trip$sets)
        n_sets <- length(set_ids)
        
        residual_effects_trip <- rnorm(n_sets, mean = 0, sd = sigma_residual)
        names(residual_effects_trip) <- set_ids
        residual_effects <- c(residual_effects, residual_effects_trip)
      }
    }
    
    random_effects$residual[[species]] <- residual_effects
  }
  
  return(random_effects)
}

#' Generate Catch Data
#' 
#' Simulates catch data based on the hierarchical model
#' 
#' @param fleet Fleet structure from generate_fleet()
#' @param random_effects Random effects from generate_random_effects()
#' @param species_parameters List of parameters for each species
#' @return Data frame with simulated catch data
#' @export
generate_catch_data <- function(fleet, random_effects, species_parameters) {
  
  # Validate inputs
  if (!is.list(fleet)) stop("Fleet must be a list")
  if (!is.list(random_effects)) stop("Random effects must be a list")
  if (!is.list(species_parameters)) stop("Species parameters must be a list")
  
  # Initialize results data frame
  catch_data <- data.frame()
  
  # Generate catch data for each species
  for (species in names(species_parameters)) {
    params <- species_parameters[[species]]
    
    # Extract parameters
    beta0 <- params$beta0 %||% 0
    distribution <- params$distribution %||% "poisson"
    
    # Get random effects for this species
    vessel_effects <- random_effects$vessel[[species]]
    trip_effects <- random_effects$trip[[species]]
    residual_effects <- random_effects$residual[[species]]
    
    # Generate catch for each set
    for (vessel_id in names(fleet)) {
      vessel <- fleet[[vessel_id]]
      vessel_effect <- vessel_effects[vessel_id]
      
      for (trip_id in names(vessel$trips)) {
        trip <- vessel$trips[[trip_id]]
        trip_effect <- trip_effects[[trip_id]]
        
        for (set_id in names(trip$sets)) {
          residual_effect <- residual_effects[[set_id]]
          
          # Calculate linear predictor
          linear_pred <- beta0 + vessel_effect + trip_effect + residual_effect
          
          # Transform to expected count
          mu <- exp(linear_pred)
          
          # Generate count based on distribution
          count <- switch(distribution,
            "poisson" = rpois(1, lambda = mu),
            "negative_binomial" = {
              phi <- params$phi %||% 1
              rnbinom(1, size = phi, mu = mu)
            },
            "binomial" = {
              n_trials <- params$n_trials %||% 100
              prob <- plogis(linear_pred)  # Use logit link for binomial
              rbinom(1, size = n_trials, prob = prob)
            },
            stop("Unsupported distribution: ", distribution)
          )
          
          # Add to results
          catch_data <- rbind(catch_data, data.frame(
            vessel_id = vessel_id,
            trip_id = trip_id,
            set_id = set_id,
            species = species,
            count = count,
            mu = mu,
            linear_pred = linear_pred,
            vessel_effect = vessel_effect,
            trip_effect = trip_effect,
            residual_effect = residual_effect,
            monitored = FALSE,  # Will be set by monitoring functions
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  return(catch_data)
}

#' Apply Monitoring Strategy
#' 
#' Applies a monitoring strategy to determine which sets are monitored
#' 
#' @param catch_data Data frame from generate_catch_data()
#' @param strategy Monitoring strategy ("random_sets", "random_vessels", "random_trips")
#' @param coverage_percentage Percentage of sets to monitor (0-100)
#' @param bias_parameters List with delta_monitoring and gamma_selection parameters
#' @return Data frame with updated monitoring status
#' @export
apply_monitoring_strategy <- function(catch_data, 
                                    strategy = "random_sets",
                                    coverage_percentage = 30,
                                    bias_parameters = NULL) {
  
  # Apply the specified strategy
  result <- switch(strategy,
    "random_sets" = monitor_random_sets(catch_data, coverage_percentage, bias_parameters),
    "random_vessels" = monitor_random_vessels(catch_data, coverage_percentage, bias_parameters),
    "random_trips" = monitor_random_trips(catch_data, coverage_percentage, bias_parameters),
    stop("Unsupported monitoring strategy: ", strategy)
  )
  
  return(result)
}

#' Monitor Random Sets
#' 
#' Implements random sets monitoring strategy
#' 
#' @param catch_data Data frame with catch data
#' @param coverage_percentage Percentage of sets to monitor
#' @param bias_parameters Optional bias parameters
#' @return Data frame with monitoring status
monitor_random_sets <- function(catch_data, coverage_percentage, bias_parameters = NULL) {
  
  # Get unique sets
  unique_sets <- unique(catch_data$set_id)
  n_sets <- length(unique_sets)
  n_monitor <- round(n_sets * coverage_percentage / 100)
  
  # Apply bias if specified
  if (!is.null(bias_parameters) && !is.null(bias_parameters$delta_monitoring)) {
    delta <- bias_parameters$delta_monitoring
    
    # Calculate weights based on vessel and trip effects
    set_weights <- catch_data %>%
      group_by(set_id) %>%
      summarise(
        weight = exp(delta * (first(vessel_effect) + first(trip_effect))),
        .groups = "drop"
      )
    
    # Sample with weights
    monitored_sets <- sample(unique_sets, size = n_monitor, 
                           prob = set_weights$weight, replace = FALSE)
  } else {
    # Simple random sampling
    monitored_sets <- sample(unique_sets, size = n_monitor, replace = FALSE)
  }
  
  # Update monitoring status
  catch_data$monitored <- catch_data$set_id %in% monitored_sets
  
  return(catch_data)
}

#' Monitor Random Vessels
#' 
#' Implements random vessels monitoring strategy
#' 
#' @param catch_data Data frame with catch data
#' @param coverage_percentage Percentage of sets to monitor (achieved through vessel selection)
#' @param bias_parameters Optional bias parameters
#' @return Data frame with monitoring status
monitor_random_vessels <- function(catch_data, coverage_percentage, bias_parameters = NULL) {
  
  # Calculate sets per vessel
  vessel_summary <- catch_data %>%
    group_by(vessel_id) %>%
    summarise(
      n_sets = n_distinct(set_id),
      avg_vessel_effect = first(vessel_effect),
      .groups = "drop"
    )
  
  # Determine how many vessels to select to achieve target coverage
  total_sets <- sum(vessel_summary$n_sets)
  target_sets <- round(total_sets * coverage_percentage / 100)
  
  # Apply bias if specified
  if (!is.null(bias_parameters) && !is.null(bias_parameters$delta_monitoring)) {
    delta <- bias_parameters$delta_monitoring
    vessel_summary$weight <- exp(delta * vessel_summary$avg_vessel_effect)
  } else {
    vessel_summary$weight <- 1
  }
  
  # Greedy selection to get close to target coverage
  vessel_summary <- vessel_summary[order(-vessel_summary$weight), ]
  cumulative_sets <- cumsum(vessel_summary$n_sets)
  n_vessels_to_select <- which(cumulative_sets >= target_sets)[1]
  
  if (is.na(n_vessels_to_select)) n_vessels_to_select <- nrow(vessel_summary)
  
  # Sample vessels with weights
  monitored_vessels <- sample(vessel_summary$vessel_id[1:n_vessels_to_select], 
                            size = min(n_vessels_to_select, nrow(vessel_summary)),
                            prob = vessel_summary$weight[1:n_vessels_to_select],
                            replace = FALSE)
  
  # Update monitoring status
  catch_data$monitored <- catch_data$vessel_id %in% monitored_vessels
  
  return(catch_data)
}

#' Monitor Random Trips
#' 
#' Implements random trips monitoring strategy
#' 
#' @param catch_data Data frame with catch data
#' @param coverage_percentage Percentage of sets to monitor (achieved through trip selection)
#' @param bias_parameters Optional bias parameters
#' @return Data frame with monitoring status
monitor_random_trips <- function(catch_data, coverage_percentage, bias_parameters = NULL) {
  
  # Calculate sets per trip
  trip_summary <- catch_data %>%
    group_by(trip_id) %>%
    summarise(
      n_sets = n_distinct(set_id),
      avg_trip_effect = first(trip_effect),
      avg_vessel_effect = first(vessel_effect),
      .groups = "drop"
    )
  
  # Determine how many trips to select
  total_sets <- sum(trip_summary$n_sets)
  target_sets <- round(total_sets * coverage_percentage / 100)
  
  # Apply bias if specified
  if (!is.null(bias_parameters) && !is.null(bias_parameters$delta_monitoring)) {
    delta <- bias_parameters$delta_monitoring
    trip_summary$weight <- exp(delta * (trip_summary$avg_vessel_effect + trip_summary$avg_trip_effect))
  } else {
    trip_summary$weight <- 1
  }
  
  # Greedy selection to get close to target coverage
  trip_summary <- trip_summary[order(-trip_summary$weight), ]
  cumulative_sets <- cumsum(trip_summary$n_sets)
  n_trips_to_select <- which(cumulative_sets >= target_sets)[1]
  
  if (is.na(n_trips_to_select)) n_trips_to_select <- nrow(trip_summary)
  
  # Sample trips with weights
  monitored_trips <- sample(trip_summary$trip_id[1:n_trips_to_select],
                          size = min(n_trips_to_select, nrow(trip_summary)),
                          prob = trip_summary$weight[1:n_trips_to_select],
                          replace = FALSE)
  
  # Update monitoring status
  catch_data$monitored <- catch_data$trip_id %in% monitored_trips
  
  return(catch_data)
}

#' Run Single Simulation
#' 
#' Runs a complete simulation with specified parameters
#' 
#' @param parameters List of simulation parameters
#' @return List with simulation results
#' @export
run_single_simulation <- function(parameters) {
  
  # Extract parameters
  sim_params <- parameters$simulation_parameters
  catch_params <- parameters$catch_model_parameters
  monitor_params <- parameters$monitoring_parameters
  
  # Generate fleet
  fleet <- generate_fleet(
    n_vessels = sim_params$fleet_size$vessels,
    n_trips_per_vessel = sim_params$fleet_size$trips_per_vessel,
    n_sets_per_trip = sim_params$fleet_size$sets_per_trip
  )
  
  # Generate random effects
  species_params <- setNames(catch_params$species, 
                           sapply(catch_params$species, function(x) x$name))
  random_effects <- generate_random_effects(fleet, species_params)
  
  # Generate catch data
  catch_data <- generate_catch_data(fleet, random_effects, species_params)
  
  # Apply monitoring strategy
  monitored_data <- apply_monitoring_strategy(
    catch_data,
    strategy = monitor_params$strategy,
    coverage_percentage = monitor_params$coverage_percentage,
    bias_parameters = monitor_params$bias_parameters
  )
  
  return(list(
    fleet = fleet,
    catch_data = monitored_data,
    parameters = parameters
  ))
}

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x