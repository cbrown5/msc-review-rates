# MSC Review Rates Simulation - Core Functions
# This script contains the core functions for the three modules of the simulation model:
# 1. Catch event module
# 2. Monitoring module
# 3. Catch statistic estimation

#' Generate fleet structure (vessels, trips, sets)
#' @param params List of parameters
#' @return List containing fleet structure information
generate_fleet_structure <- function(params) {
  # Extract parameters
  V <- params$V  # Number of vessels
  mu_trips <- params$mu_trips  # Expected number of trips per vessel
  mu_sets <- params$mu_sets  # Expected number of sets per trip
  
  # Generate number of trips for each vessel
  T <- rpois(V, mu_trips)
  
  # Initialize list to store number of sets for each trip of each vessel
  S <- list()
  
  # Generate number of sets for each trip
  for (v in 1:V) {
    S[[v]] <- rpois(T[v], mu_sets)
  }
  
  # Return structured data
  return(list(
    V = V,  # Number of vessels
    T = T,  # Number of trips per vessel
    S = S   # Number of sets per trip
  ))
}

#' Generate random effects for vessels and trips
#' @param params List of parameters
#' @param fleet_structure Fleet structure from generate_fleet_structure()
#' @return List containing random effects
generate_random_effects <- function(params, fleet_structure) {
  # Extract parameters
  V <- fleet_structure$V  # Number of vessels
  T <- fleet_structure$T  # Number of trips per vessel
  sigma_x <- params$sigma_x  # SD for vessel-level random effects
  sigma_z <- params$sigma_z  # SD for trip-level random effects
  
  # Generate vessel-level random effects
  x <- rnorm(V, 0, sigma_x)
  
  # Initialize list to store trip-level random effects
  z <- list()
  
  # Generate trip-level random effects
  for (v in 1:V) {
    z[[v]] <- rnorm(T[v], 0, sigma_z)
  }
  
  # Return structured data
  return(list(
    x = x,  # Vessel-level random effects
    z = z   # Trip-level random effects
  ))
}

#' Simulate catches for each set
#' @param params List of parameters
#' @param fleet_structure Fleet structure
#' @param random_effects Random effects
#' @return List containing catches and expected catch rates
simulate_catches <- function(params, fleet_structure, random_effects) {
  # Extract parameters
  V <- fleet_structure$V  # Number of vessels
  T <- fleet_structure$T  # Number of trips per vessel
  S <- fleet_structure$S  # Number of sets per trip
  beta_0 <- params$beta_0  # Baseline expected catch rate
  tau <- params$tau  # Dispersion parameter for negative binomial
  
  # Extract random effects
  x <- random_effects$x  # Vessel-level random effects
  z <- random_effects$z  # Trip-level random effects
  
  # Initialize lists to store catches and expected catch rates
  y <- list()
  mu <- list()
  
  # Calculate expected catch rates and simulate catches
  for (v in 1:V) {
    y[[v]] <- list()
    mu[[v]] <- list()
    
    for (t in 1:T[v]) {
      # Calculate expected catch rate for this trip
      mu_vt <- exp(beta_0 + x[v] + z[[v]][t])
      mu[[v]][[t]] <- rep(mu_vt, S[[v]][t])
      
      # Simulate catches using negative binomial distribution
      # For negative binomial, we need size (dispersion) and prob parameters
      # Convert mu and tau to size and prob
      size <- tau
      prob <- size / (size + mu_vt)
      
      y[[v]][[t]] <- rnbinom(S[[v]][t], size = size, prob = prob)
    }
  }
  
  # Return structured data
  return(list(
    y = y,    # Catches
    mu = mu   # Expected catch rates
  ))
}

#' Apply random monitoring across sets
#' @param params List of parameters
#' @param fleet_structure Fleet structure
#' @param catches Array of catches
#' @param random_effects Random effects
#' @return List containing monitoring status
apply_set_monitoring <- function(params, fleet_structure, catches, random_effects) {
  # Extract parameters
  V <- fleet_structure$V  # Number of vessels
  T <- fleet_structure$T  # Number of trips per vessel
  S <- fleet_structure$S  # Number of sets per trip
  p_monitor <- params$p_monitor  # Proportion of sets to be monitored
  
  # Initialize list to store monitoring status
  M <- list()
  
  # Apply random monitoring across all sets
  for (v in 1:V) {
    M[[v]] <- list()
    
    for (t in 1:T[v]) {
      # Generate random monitoring status for each set
      M[[v]][[t]] <- rbinom(S[[v]][t], 1, p_monitor)
    }
  }
  
  # Return monitoring matrix
  return(list(
    M = M,  # Monitoring status
    strategy = "sets"  # Strategy name
  ))
}

#' Apply vessel-based monitoring with potential bias
#' @param params List of parameters
#' @param fleet_structure Fleet structure
#' @param catches Array of catches
#' @param random_effects Random effects
#' @return List containing monitoring status
apply_vessel_monitoring <- function(params, fleet_structure, catches, random_effects) {
  # Extract parameters
  V <- fleet_structure$V  # Number of vessels
  T <- fleet_structure$T  # Number of trips per vessel
  S <- fleet_structure$S  # Number of sets per trip
  p_monitor <- params$p_monitor  # Proportion of sets to be monitored
  bias_v <- params$bias_v  # Bias factor for vessel selection
  
  # Extract random effects
  x <- random_effects$x  # Vessel-level random effects
  
  # Initialize list to store monitoring status
  M <- list()
  
  # Calculate probability of monitoring for each vessel with bias
  phi_vessels <- plogis(qlogis(p_monitor) + bias_v * x)
  
  # Select vessels for monitoring based on biased probabilities
  vessel_monitored <- rbinom(V, 1, phi_vessels)
  
  # Apply monitoring to all sets of selected vessels
  for (v in 1:V) {
    M[[v]] <- list()
    
    for (t in 1:T[v]) {
      # If vessel is selected, monitor all sets
      M[[v]][[t]] <- rep(vessel_monitored[v], S[[v]][t])
    }
  }
  
  # Return monitoring matrix
  return(list(
    M = M,  # Monitoring status
    strategy = "vessels"  # Strategy name
  ))
}

#' Apply trip-based monitoring with potential bias
#' @param params List of parameters
#' @param fleet_structure Fleet structure
#' @param catches Array of catches
#' @param random_effects Random effects
#' @return List containing monitoring status
apply_trip_monitoring <- function(params, fleet_structure, catches, random_effects) {
  # Extract parameters
  V <- fleet_structure$V  # Number of vessels
  T <- fleet_structure$T  # Number of trips per vessel
  S <- fleet_structure$S  # Number of sets per trip
  p_monitor <- params$p_monitor  # Proportion of sets to be monitored
  bias_factor <- params$bias_factor  # Bias factor for trip selection
  
  # Extract random effects
  z <- random_effects$z  # Trip-level random effects
  
  # Initialize list to store monitoring status
  M <- list()
  
  # Apply trip-based monitoring with bias based on trip random effects
  for (v in 1:V) {
    M[[v]] <- list()
    
    # Calculate probability of monitoring for each trip with bias
    phi_trips <- plogis(qlogis(p_monitor) + bias_factor * z[[v]])
    
    # Select trips for monitoring based on biased probabilities
    trip_monitored <- rbinom(T[v], 1, phi_trips)
    
    for (t in 1:T[v]) {
      # If trip is selected, monitor all sets
      M[[v]][[t]] <- rep(trip_monitored[t], S[[v]][t])
    }
  }
  
  # Return monitoring matrix
  return(list(
    M = M,  # Monitoring status
    strategy = "trips"  # Strategy name
  ))
}

#' Estimate catch statistics based on monitoring
#' @param catches List of catches
#' @param monitoring List of monitoring status
#' @param fleet_structure Fleet structure
#' @return List of statistics (estimated rate, true rate, bias, etc.)
estimate_catch_statistics <- function(catches, monitoring, fleet_structure) {
  # Extract data
  y <- catches$y  # Catches
  M <- monitoring$M  # Monitoring status
  strategy <- monitoring$strategy  # Monitoring strategy
  
  # Initialize counters
  total_catch <- 0
  total_sets <- 0
  monitored_catch <- 0
  monitored_sets <- 0
  
  # Calculate totals
  for (v in 1:length(y)) {
    for (t in 1:length(y[[v]])) {
      total_catch <- total_catch + sum(y[[v]][[t]])
      total_sets <- total_sets + length(y[[v]][[t]])
      
      monitored_catch <- monitored_catch + sum(y[[v]][[t]] * M[[v]][[t]])
      monitored_sets <- monitored_sets + sum(M[[v]][[t]])
    }
  }
  
  # Calculate rates
  true_catch_rate <- total_catch / total_sets
  
  # Avoid division by zero
  if (monitored_sets > 0) {
    estimated_catch_rate <- monitored_catch / monitored_sets
  } else {
    estimated_catch_rate <- NA
  }
  
  # Calculate bias
  bias <- estimated_catch_rate - true_catch_rate
  bias_percent <- (bias / true_catch_rate) * 100
  
  # Return statistics
  return(list(
    strategy = strategy,
    estimated_catch_rate = estimated_catch_rate,
    true_catch_rate = true_catch_rate,
    bias = bias,
    bias_percent = bias_percent,
    monitored_sets = monitored_sets,
    total_sets = total_sets,
    monitored_catch = monitored_catch,
    total_catch = total_catch
  ))
}

#' Run a single simulation
#' @param params List of parameters
#' @param rep Replication number
#' @return Data frame with results
run_single_simulation <- function(params, rep) {
  # Generate fleet structure
  fleet_structure <- generate_fleet_structure(params)
  
  # Generate random effects
  random_effects <- generate_random_effects(params, fleet_structure)
  
  # Simulate catches
  catches <- simulate_catches(params, fleet_structure, random_effects)
  
  # Apply monitoring strategies
  monitoring_sets <- apply_set_monitoring(params, fleet_structure, catches, random_effects)
  monitoring_vessels <- apply_vessel_monitoring(params, fleet_structure, catches, random_effects)
  monitoring_trips <- apply_trip_monitoring(params, fleet_structure, catches, random_effects)
  
  # Estimate catch statistics
  stats_sets <- estimate_catch_statistics(catches, monitoring_sets, fleet_structure)
  stats_vessels <- estimate_catch_statistics(catches, monitoring_vessels, fleet_structure)
  stats_trips <- estimate_catch_statistics(catches, monitoring_trips, fleet_structure)
  
  # Combine results
  results <- data.frame(
    parameter_set_id = params$parameter_set_id,
    description = params$description,
    replication = rep,
    strategy = c(stats_sets$strategy, stats_vessels$strategy, stats_trips$strategy),
    estimated_catch_rate = c(stats_sets$estimated_catch_rate, 
                           stats_vessels$estimated_catch_rate, 
                           stats_trips$estimated_catch_rate),
    true_catch_rate = c(stats_sets$true_catch_rate, 
                      stats_vessels$true_catch_rate, 
                      stats_trips$true_catch_rate),
    bias = c(stats_sets$bias, stats_vessels$bias, stats_trips$bias),
    bias_percent = c(stats_sets$bias_percent, 
                   stats_vessels$bias_percent, 
                   stats_trips$bias_percent),
    monitored_sets = c(stats_sets$monitored_sets, 
                     stats_vessels$monitored_sets, 
                     stats_trips$monitored_sets),
    total_sets = c(stats_sets$total_sets, 
                 stats_vessels$total_sets, 
                 stats_trips$total_sets)
  )
  
  return(results)
}