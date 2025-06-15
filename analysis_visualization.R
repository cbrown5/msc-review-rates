# MSC Monitoring Simulation Model - Analysis and Visualization
# Author: Bilby (R Workshop Expert)
# Description: Functions for analyzing simulation results and creating visualizations

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

#' Calculate Metrics
#' 
#' Calculates bias and detection metrics from simulation results
#' 
#' @param simulation_results List of simulation results
#' @return Data frame with calculated metrics
#' @export
calculate_metrics <- function(simulation_results) {
  
  if (!is.list(simulation_results)) {
    stop("simulation_results must be a list")
  }
  
  # Extract catch data
  catch_data <- simulation_results$catch_data
  
  if (!is.data.frame(catch_data)) {
    stop("catch_data must be a data frame")
  }
  
  # Calculate true vs estimated catch rates by species
  metrics <- catch_data %>%
    group_by(species) %>%
    summarise(
      # True catch rates (all sets)
      true_mean_catch = mean(count),
      true_total_catch = sum(count),
      true_sets = n(),
      
      # Monitored catch rates
      monitored_mean_catch = mean(count[monitored]),
      monitored_total_catch = sum(count[monitored]),
      monitored_sets = sum(monitored),
      
      # Coverage
      coverage_percentage = 100 * sum(monitored) / n(),
      
      # Bias metrics
      absolute_bias = monitored_mean_catch - true_mean_catch,
      relative_bias = (monitored_mean_catch - true_mean_catch) / true_mean_catch * 100,
      
      # Detection metrics for rare events (count > 0)
      true_detection_rate = mean(count > 0) * 100,
      monitored_detection_rate = ifelse(sum(monitored) > 0, 
                                      mean(count[monitored] > 0) * 100, 
                                      NA),
      
      # Variance metrics
      true_variance = var(count),
      monitored_variance = ifelse(sum(monitored) > 1, 
                                var(count[monitored]), 
                                NA),
      
      .groups = "drop"
    )
  
  # Add overall fleet metrics
  overall_metrics <- catch_data %>%
    summarise(
      species = "Overall",
      true_mean_catch = mean(count),
      true_total_catch = sum(count),
      true_sets = n(),
      monitored_mean_catch = mean(count[monitored]),
      monitored_total_catch = sum(count[monitored]),
      monitored_sets = sum(monitored),
      coverage_percentage = 100 * sum(monitored) / n(),
      absolute_bias = monitored_mean_catch - true_mean_catch,
      relative_bias = (monitored_mean_catch - true_mean_catch) / true_mean_catch * 100,
      true_detection_rate = mean(count > 0) * 100,
      monitored_detection_rate = ifelse(sum(monitored) > 0, 
                                      mean(count[monitored] > 0) * 100, 
                                      NA),
      true_variance = var(count),
      monitored_variance = ifelse(sum(monitored) > 1, 
                                var(count[monitored]), 
                                NA)
    )
  
  # Combine species and overall metrics
  metrics <- bind_rows(metrics, overall_metrics)
  
  # Add simulation metadata
  params <- simulation_results$parameters
  metrics$monitoring_strategy <- params$monitoring_parameters$strategy
  metrics$coverage_target <- params$monitoring_parameters$coverage_percentage
  metrics$delta_monitoring <- params$monitoring_parameters$bias_parameters$delta_monitoring
  
  return(metrics)
}

#' Run Multiple Simulations
#' 
#' Runs multiple simulations and calculates summary statistics
#' 
#' @param parameters Parameter list
#' @param n_simulations Number of simulations to run (overrides parameter file)
#' @param verbose Whether to show progress (default: TRUE)
#' @return List with summary results and individual simulation data
#' @export
run_multiple_simulations <- function(parameters, n_simulations = NULL, verbose = TRUE) {
  
  # Use n_simulations parameter if provided, otherwise use from parameters
  if (!is.null(n_simulations)) {
    parameters$simulation_parameters$n_simulations <- n_simulations
  }
  
  n_sims <- parameters$simulation_parameters$n_simulations
  
  if (verbose) {
    cat("Running", n_sims, "simulations...\n")
  }
  
  # Set seed if specified
  if (!is.null(parameters$simulation_parameters$seed)) {
    set.seed(parameters$simulation_parameters$seed)
  }
  
  # Run simulations
  all_metrics <- list()
  
  for (i in 1:n_sims) {
    if (verbose && i %% max(1, floor(n_sims/10)) == 0) {
      cat("  Completed", i, "of", n_sims, "simulations\n")
    }
    
    # Run single simulation
    sim_result <- run_single_simulation(parameters)
    
    # Calculate metrics
    metrics <- calculate_metrics(sim_result)
    metrics$simulation_id <- i
    
    all_metrics[[i]] <- metrics
  }
  
  # Combine all metrics
  combined_metrics <- bind_rows(all_metrics)
  
  # Calculate summary statistics
  summary_stats <- combined_metrics %>%
    group_by(species, monitoring_strategy) %>%
    summarise(
      n_simulations = n(),
      
      # Bias statistics
      mean_absolute_bias = mean(absolute_bias, na.rm = TRUE),
      sd_absolute_bias = sd(absolute_bias, na.rm = TRUE),
      mean_relative_bias = mean(relative_bias, na.rm = TRUE),
      sd_relative_bias = sd(relative_bias, na.rm = TRUE),
      
      # Coverage statistics
      mean_coverage = mean(coverage_percentage, na.rm = TRUE),
      sd_coverage = sd(coverage_percentage, na.rm = TRUE),
      
      # Detection rate statistics
      mean_true_detection = mean(true_detection_rate, na.rm = TRUE),
      mean_monitored_detection = mean(monitored_detection_rate, na.rm = TRUE),
      detection_bias = mean_monitored_detection - mean_true_detection,
      
      # Confidence intervals for bias
      bias_ci_lower = quantile(relative_bias, 0.025, na.rm = TRUE),
      bias_ci_upper = quantile(relative_bias, 0.975, na.rm = TRUE),
      
      .groups = "drop"
    )
  
  if (verbose) {
    cat("Simulations completed!\n")
  }
  
  return(list(
    summary_statistics = summary_stats,
    individual_metrics = combined_metrics,
    parameters = parameters
  ))
}

#' Compare Monitoring Strategies
#' 
#' Compares multiple monitoring strategies
#' 
#' @param base_parameters Base parameter set
#' @param strategies Vector of strategy names to compare
#' @param n_simulations Number of simulations per strategy
#' @param verbose Whether to show progress
#' @return Comparison results
#' @export
compare_monitoring_strategies <- function(base_parameters, 
                                        strategies = c("random_sets", "random_vessels", "random_trips"),
                                        n_simulations = 100,
                                        verbose = TRUE) {
  
  if (verbose) {
    cat("Comparing monitoring strategies:", paste(strategies, collapse = ", "), "\n")
  }
  
  all_results <- list()
  
  for (strategy in strategies) {
    if (verbose) {
      cat("\nRunning simulations for strategy:", strategy, "\n")
    }
    
    # Modify parameters for this strategy
    params <- base_parameters
    params$monitoring_parameters$strategy <- strategy
    
    # Run simulations
    results <- run_multiple_simulations(params, n_simulations, verbose = verbose)
    all_results[[strategy]] <- results
  }
  
  # Combine summary statistics
  combined_summary <- map_dfr(all_results, ~.x$summary_statistics, .id = "strategy_comparison")
  
  # Combine individual metrics
  combined_individual <- map_dfr(all_results, ~.x$individual_metrics, .id = "strategy_comparison")
  
  return(list(
    summary_comparison = combined_summary,
    individual_comparison = combined_individual,
    strategy_results = all_results,
    parameters = base_parameters
  ))
}

#' Plot Bias Comparison
#' 
#' Creates a plot comparing bias across monitoring strategies
#' 
#' @param comparison_results Results from compare_monitoring_strategies()
#' @param species_filter Optional species to filter (default: all)
#' @return ggplot object
#' @export
plot_bias_comparison <- function(comparison_results, species_filter = NULL) {
  
  data <- comparison_results$individual_comparison
  
  # Filter species if specified
  if (!is.null(species_filter)) {
    data <- data %>% filter(species %in% species_filter)
  }
  
  # Remove overall category for cleaner plot
  data <- data %>% filter(species != "Overall")
  
  p <- ggplot(data, aes(x = monitoring_strategy, y = relative_bias, fill = monitoring_strategy)) +
    geom_boxplot(alpha = 0.7) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
    facet_wrap(~species, scales = "free_y") +
    labs(
      title = "Relative Bias by Monitoring Strategy",
      subtitle = "Comparison of estimated vs. true catch rates",
      x = "Monitoring Strategy",
      y = "Relative Bias (%)",
      fill = "Strategy"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(face = "bold")
    ) +
    scale_fill_brewer(type = "qual", palette = "Set2")
  
  return(p)
}

#' Plot Coverage Analysis
#' 
#' Creates a plot showing actual vs. target coverage
#' 
#' @param comparison_results Results from compare_monitoring_strategies()
#' @return ggplot object
#' @export
plot_coverage_analysis <- function(comparison_results) {
  
  data <- comparison_results$summary_comparison
  target_coverage <- comparison_results$parameters$monitoring_parameters$coverage_percentage
  
  p <- ggplot(data, aes(x = monitoring_strategy, y = mean_coverage, fill = monitoring_strategy)) +
    geom_col(alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_coverage - sd_coverage, 
                     ymax = mean_coverage + sd_coverage),
                 width = 0.2) +
    geom_hline(yintercept = target_coverage, linetype = "dashed", color = "red", size = 1) +
    facet_wrap(~species) +
    labs(
      title = "Monitoring Coverage by Strategy",
      subtitle = paste("Target coverage:", target_coverage, "%"),
      x = "Monitoring Strategy",
      y = "Actual Coverage (%)",
      fill = "Strategy"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(face = "bold")
    ) +
    scale_fill_brewer(type = "qual", palette = "Set1")
  
  return(p)
}

#' Plot Detection Rates
#' 
#' Creates a plot comparing detection rates for rare events
#' 
#' @param comparison_results Results from compare_monitoring_strategies()
#' @return ggplot object
#' @export
plot_detection_rates <- function(comparison_results) {
  
  data <- comparison_results$summary_comparison %>%
    filter(species != "Overall") %>%
    select(species, monitoring_strategy, mean_true_detection, mean_monitored_detection) %>%
    pivot_longer(cols = c(mean_true_detection, mean_monitored_detection),
                names_to = "detection_type",
                values_to = "detection_rate") %>%
    mutate(detection_type = case_when(
      detection_type == "mean_true_detection" ~ "True Rate",
      detection_type == "mean_monitored_detection" ~ "Monitored Rate"
    ))
  
  p <- ggplot(data, aes(x = monitoring_strategy, y = detection_rate, 
                       fill = detection_type)) +
    geom_col(position = "dodge", alpha = 0.7) +
    facet_wrap(~species) +
    labs(
      title = "Detection Rates by Monitoring Strategy",
      subtitle = "Comparison of true vs. monitored detection rates for rare events",
      x = "Monitoring Strategy",
      y = "Detection Rate (%)",
      fill = "Detection Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(face = "bold")
    ) +
    scale_fill_manual(values = c("True Rate" = "#2E86AB", "Monitored Rate" = "#A23B72"))
  
  return(p)
}

#' Create Summary Report
#' 
#' Creates a comprehensive summary of simulation results
#' 
#' @param comparison_results Results from compare_monitoring_strategies()
#' @param output_file Optional file to save the report
#' @return List with summary tables and plots
#' @export
create_summary_report <- function(comparison_results, output_file = NULL) {
  
  # Summary statistics table
  summary_table <- comparison_results$summary_comparison %>%
    select(species, monitoring_strategy, mean_relative_bias, sd_relative_bias, 
           mean_coverage, detection_bias) %>%
    mutate(
      mean_relative_bias = round(mean_relative_bias, 2),
      sd_relative_bias = round(sd_relative_bias, 2),
      mean_coverage = round(mean_coverage, 1),
      detection_bias = round(detection_bias, 2)
    )
  
  # Create plots
  plots <- list(
    bias_comparison = plot_bias_comparison(comparison_results),
    coverage_analysis = plot_coverage_analysis(comparison_results),
    detection_rates = plot_detection_rates(comparison_results)
  )
  
  # Strategy recommendations
  recommendations <- comparison_results$summary_comparison %>%
    filter(species != "Overall") %>%
    group_by(species) %>%
    slice_min(abs(mean_relative_bias), n = 1) %>%
    select(species, monitoring_strategy, mean_relative_bias) %>%
    rename(recommended_strategy = monitoring_strategy,
           bias_with_recommended = mean_relative_bias)
  
  report <- list(
    summary_table = summary_table,
    plots = plots,
    recommendations = recommendations,
    parameters = comparison_results$parameters
  )
  
  # Save to file if requested
  if (!is.null(output_file)) {
    saveRDS(report, output_file)
    message("Report saved to: ", output_file)
  }
  
  return(report)
}

#' Print Summary Statistics
#' 
#' Prints a formatted summary of simulation results
#' 
#' @param comparison_results Results from compare_monitoring_strategies()
#' @export
print_summary_statistics <- function(comparison_results) {
  
  cat("=== MSC Monitoring Simulation Results ===\n\n")
  
  # Parameters summary
  params <- comparison_results$parameters
  cat("Simulation Parameters:\n")
  cat("  - Fleet size:", params$simulation_parameters$fleet_size$vessels, "vessels\n")
  cat("  - Target coverage:", params$monitoring_parameters$coverage_percentage, "%\n")
  cat("  - Bias parameter (delta):", params$monitoring_parameters$bias_parameters$delta_monitoring, "\n\n")
  
  # Results by strategy and species
  summary_data <- comparison_results$summary_comparison
  
  for (strategy in unique(summary_data$monitoring_strategy)) {
    cat("Strategy:", strategy, "\n")
    cat(rep("-", nchar(strategy) + 10), "\n", sep = "")
    
    strategy_data <- summary_data %>% filter(monitoring_strategy == strategy)
    
    for (i in 1:nrow(strategy_data)) {
      row <- strategy_data[i, ]
      cat("  ", row$species, ":\n")
      cat("    * Mean relative bias: ", round(row$mean_relative_bias, 2), "% (±", 
          round(row$sd_relative_bias, 2), "%)\n")
      cat("    * Actual coverage: ", round(row$mean_coverage, 1), "%\n")
      cat("    * Detection bias: ", round(row$detection_bias, 2), "%\n")
    }
    cat("\n")
  }
  
  # Recommendations
  cat("Recommendations:\n")
  cat("================\n")
  
  recommendations <- comparison_results$summary_comparison %>%
    filter(species != "Overall") %>%
    group_by(species) %>%
    slice_min(abs(mean_relative_bias), n = 1)
  
  for (i in 1:nrow(recommendations)) {
    row <- recommendations[i, ]
    cat("  ", row$species, ": Use '", row$monitoring_strategy, 
        "' (bias: ", round(row$mean_relative_bias, 2), "%)\n")
  }
  
  cat("\n==========================================\n")
}

#' Save Results
#' 
#' Saves simulation results to various formats
#' 
#' @param results Simulation results
#' @param output_file Base filename (without extension)
#' @param formats Vector of formats to save ("rds", "csv", "json")
#' @export
save_results <- function(results, output_file, formats = c("rds", "csv")) {
  
  for (format in formats) {
    filename <- paste0(output_file, ".", format)
    
    switch(format,
      "rds" = {
        saveRDS(results, filename)
        message("Results saved as RDS: ", filename)
      },
      "csv" = {
        if ("summary_comparison" %in% names(results)) {
          write.csv(results$summary_comparison, filename, row.names = FALSE)
        } else if ("summary_statistics" %in% names(results)) {
          write.csv(results$summary_statistics, filename, row.names = FALSE)
        } else {
          warning("No suitable data frame found for CSV export")
        }
        message("Results saved as CSV: ", filename)
      },
      "json" = {
        jsonlite::write_json(results, filename, pretty = TRUE, auto_unbox = TRUE)
        message("Results saved as JSON: ", filename)
      },
      warning("Unsupported format: ", format)
    )
  }
  
  invisible(output_file)
}