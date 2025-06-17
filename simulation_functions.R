# MSC Review Rates Simulation - Core Functions
# This script contains the core functions for the three modules of the simulation model:
# 1. Catch event module
# 2. Monitoring module
# 3. Catch statistic estimation

#' Generate fleet structure, random effects and simulated catches in a single function
#' @param params List of parameters
#' @param rep Replication number for this simulation
#' @return Dataframe containing tidy format catch data
simulate_fleet_catches <- function(params, rep = 1) {
  # --- FLEET STRUCTURE GENERATION ---
  # Extract parameters
  V <- params$V  # Number of vessels
  mu_trips <- params$mu_trips  # Expected number of trips per vessel
  mu_sets <- params$mu_sets  # Expected number of sets per trip
  sigma_x <- params$sigma_x  # SD for vessel-level random effects
  sigma_z <- params$sigma_z  # SD for trip-level random effects
  beta_0 <- params$beta_0  # Baseline expected catch rate
  theta <- params$theta  # Dispersion parameter for negative binomial
  theta_trips <- params$theta_trips  # Dispersion parameter for number of trips
  theta_sets <- params$theta_sets  # Dispersion parameter for number of sets
  
  # Generate number of trips for each vessel using negative binomial
  T_vec <- MASS::rnegbin(V, mu_trips, theta_trips)
  # Ensure at least one trip per vessel
    T_vec[T_vec < 1] <- 1

  # Initialize list to store number of sets for each trip of each vessel
  S_list <- list()
  
  # Generate number of sets for each trip using negative binomial
  for (v in 1:V) {
    S_list[[v]] <- MASS::rnegbin(T_vec[v], mu_sets, theta_sets)
    # Ensure at least one set per trip
    S_list[[v]][S_list[[v]] < 1] <- 1
  }
  
  # --- RANDOM EFFECTS GENERATION ---
  # Generate vessel-level random effects
  x_vec <- rnorm(V, 0, sigma_x)
  
  # Initialize list to store trip-level random effects
  z_list <- list()
  
  # Generate trip-level random effects
  for (v in 1:V) {
    z_list[[v]] <- rnorm(T_vec[v], 0, sigma_z)
  }
  
  # --- CATCH SIMULATION ---
  # Initialize a data frame to store the results in tidy format
  max_rows <- sum(sapply(S_list, sum))  # Calculate total number of sets
  catches_df <- data.frame(
    replication = integer(max_rows),
    vessel_id = integer(max_rows),
    trip_id = integer(max_rows),
    set_id = integer(max_rows),
    vessel_effect = numeric(max_rows),
    trip_effect = numeric(max_rows),
    expected_catch_rate = numeric(max_rows),
    catch = integer(max_rows),
    stringsAsFactors = FALSE
  )
  
  # Fill the data frame with simulated data
  row_index <- 1
  for (v in 1:V) {
    for (t in 1:T_vec[v]) {
      # Calculate expected catch rate for this trip
      mu_vt <- exp(beta_0 + x_vec[v] + z_list[[v]][t])
      
      # Simulate catches using negative binomial distribution with MASS::rnegbin
      # Only simulate if we have sets for this trip (S_list[[v]][t] could be 0)
      if (S_list[[v]][t] > 0) {
        catches <- MASS::rnegbin(S_list[[v]][t], mu_vt, theta)
        
        # Add to the data frame
        # Calculate row indices for this trip's sets
        rows <- row_index:(row_index + S_list[[v]][t] - 1)
        
        # Assign values in a vectorized way
        catches_df$replication[rows] <- rep
        catches_df$vessel_id[rows] <- v
        catches_df$trip_id[rows] <- t
        catches_df$set_id[rows] <- 1:S_list[[v]][t]
        catches_df$vessel_effect[rows] <- x_vec[v]
        catches_df$trip_effect[rows] <- z_list[[v]][t]
        catches_df$expected_catch_rate[rows] <- mu_vt
        catches_df$catch[rows] <- catches
        
        # Update row index
        row_index <- row_index + S_list[[v]][t]
      }
    }
  }


  # Trim the data frame to the actual number of rows used
  catches_df <- catches_df[1:(row_index-1), ]
  
  # Also return the fleet structure and random effects for backward compatibility
  fleet_structure <- list(
    V = V,
    T = T_vec,
    S = S_list
  )
  
  random_effects <- list(
    x = x_vec,
    z = z_list
  )
  
  # Return all data
  return(list(
    catches_df = catches_df,           # New tidy format
    fleet_structure = fleet_structure, # For backward compatibility
    random_effects = random_effects    # For backward compatibility
  ))
}

#' Generate fleet structure (vessels, trips, sets)
#' @param params List of parameters
#' @return List containing fleet structure information
#' @deprecated Use simulate_fleet_catches instead
generate_fleet_structure <- function(params) {
  # Extract parameters
  V <- params$V  # Number of vessels
  mu_trips <- params$mu_trips  # Expected number of trips per vessel
  mu_sets <- params$mu_sets  # Expected number of sets per trip
  theta_trips <- params$theta_trips  # Dispersion parameter for number of trips
  theta_sets <- params$theta_sets  # Dispersion parameter for number of sets
  
  # Generate number of trips for each vessel using negative binomial
  T <- MASS::rnegbin(V, mu_trips, theta_trips)
  
  # Initialize list to store number of sets for each trip of each vessel
  S <- list()
  
  # Generate number of sets for each trip using negative binomial
  for (v in 1:V) {
    S[[v]] <- MASS::rnegbin(T[v], mu_sets, theta_sets)
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
#' @deprecated Use simulate_fleet_catches instead
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
#' @deprecated Use simulate_fleet_catches instead
simulate_catches <- function(params, fleet_structure, random_effects) {
  # Extract parameters
  V <- fleet_structure$V  # Number of vessels
  T <- fleet_structure$T  # Number of trips per vessel
  S <- fleet_structure$S  # Number of sets per trip
  beta_0 <- params$beta_0  # Baseline expected catch rate
  theta <- params$theta  # Dispersion parameter for negative binomial
  
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
      
      # Simulate catches using negative binomial distribution with MASS::rnegbin
      y[[v]][[t]] <- MASS::rnegbin(S[[v]][t], mu_vt, theta)
    }
  }
  
  # Return structured data
  return(list(
    y = y,    # Catches
    mu = mu   # Expected catch rates
  ))
}

# --- TIDY MONITORING FUNCTIONS ---

#' Apply random monitoring across sets using tidy format
#' @param catches_df Tidy dataframe of catches
#' @param p_monitor Proportion of sets to be monitored
#' @return Dataframe with monitoring status added
apply_set_monitoring_tidy <- function(catches_df, p_monitor) {
  # Generate random monitoring status for each set
  set.seed(42)  # For reproducibility
  catches_df$monitored <- rbinom(nrow(catches_df), 1, p_monitor)
  catches_df$strategy <- "sets"
  
  return(catches_df)
}

#' Apply vessel-based monitoring with potential bias using tidy format
#' @param catches_df Tidy dataframe of catches
#' @param p_monitor Proportion of sets to be monitored
#' @param bias_v Bias factor for vessel selection
#' @return Dataframe with monitoring status added
apply_vessel_monitoring_tidy <- function(catches_df, p_monitor, bias_v = 0) {
  # Get unique vessels
  vessels <- unique(catches_df$vessel_id)
  
  # Get vessel random effects
  vessel_effects <- sapply(vessels, function(v) {
    unique(catches_df$vessel_effect[catches_df$vessel_id == v])[1]
  })
  
  # Calculate probability of monitoring for each vessel with bias
  phi_vessels <- plogis(qlogis(p_monitor) + bias_v * vessel_effects)
  
  # Select vessels for monitoring based on biased probabilities
  set.seed(42)  # For reproducibility
  vessel_monitored <- rbinom(length(vessels), 1, phi_vessels)
  names(vessel_monitored) <- vessels
  
  # Apply monitoring status to each row based on vessel
  catches_df$monitored <- sapply(catches_df$vessel_id, function(v) {
    vessel_monitored[as.character(v)]
  })
  
  catches_df$strategy <- "vessels"
  
  return(catches_df)
}

#' Apply trip-based monitoring with potential bias using tidy format
#' @param catches_df Tidy dataframe of catches
#' @param p_monitor Proportion of sets to be monitored
#' @param bias_factor Bias factor for trip selection
#' @return Dataframe with monitoring status added
apply_trip_monitoring_tidy <- function(catches_df, p_monitor, bias_factor = 0) {
  # Create a dataframe of unique vessel-trip combinations with their trip effects
  trips <- unique(catches_df[, c("vessel_id", "trip_id", "trip_effect")])
  
  # Calculate probability of monitoring for each trip with bias
  trips$phi_trip <- plogis(qlogis(p_monitor) + bias_factor * trips$trip_effect)
  
  # Select trips for monitoring based on biased probabilities
  set.seed(42)  # For reproducibility
  trips$monitored <- rbinom(nrow(trips), 1, trips$phi_trip)
  
  # Merge monitoring status back to original dataframe
  catches_df$monitored <- NA
  for (i in 1:nrow(trips)) {
    v <- trips$vessel_id[i]
    t <- trips$trip_id[i]
    m <- trips$monitored[i]
    catches_df$monitored[catches_df$vessel_id == v & catches_df$trip_id == t] <- m
  }
  
  catches_df$strategy <- "trips"
  
  return(catches_df)
}

#' Estimate catch statistics from tidy dataframe
#' @param catches_df Dataframe with catches and monitoring status
#' @return List of statistics
estimate_catch_statistics_tidy <- function(catches_df) {
  # Get strategy name
  strategy <- unique(catches_df$strategy)
  
  # Calculate totals
  total_catch <- sum(catches_df$catch)
  total_sets <- nrow(catches_df)
  monitored_catch <- sum(catches_df$catch[catches_df$monitored == 1])
  monitored_sets <- sum(catches_df$monitored)
  
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

#' Run a single simulation with tidy data structures
#' @param params List of parameters
#' @param rep Replication number
#' @return Data frame with results
run_single_simulation_tidy <- function(params, rep) {
  # Generate fleet structure, random effects, and catches all in one step
  sim_data <- simulate_fleet_catches(params, rep)
  catches_df <- sim_data$catches_df
  
  # Apply monitoring strategies
  catches_set <- apply_set_monitoring_tidy(catches_df, params$p_monitor)
  catches_vessel <- apply_vessel_monitoring_tidy(catches_df, params$p_monitor, params$bias_v)
  catches_trip <- apply_trip_monitoring_tidy(catches_df, params$p_monitor, params$bias_factor)
  
  # Estimate catch statistics
  stats_sets <- estimate_catch_statistics_tidy(catches_set)
  stats_vessels <- estimate_catch_statistics_tidy(catches_vessel)
  stats_trips <- estimate_catch_statistics_tidy(catches_trip)
  
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
  
  # Add the tidy format dataframes for additional analysis if needed
  attr(results, "tidy_data") <- list(
    sets = catches_set,
    vessels = catches_vessel,
    trips = catches_trip
  )
  
  return(results)
}