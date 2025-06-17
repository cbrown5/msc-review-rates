# Conceptual Figure for MSC Review Rates
# This script generates a conceptual figure illustrating different monitoring strategies
# and their impact on estimated catch rates

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork) # for combining plots
library(tidyr)

# Set seed for reproducibility
set.seed(123)

# Define colors
vessel_colors <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E")
catch_colors <- c("low" = "#91bfdb", "medium" = "#fee090", "high" = "#fc8d59")

# Function to create simulated data for a small fleet
create_demonstration_data <- function(n_vessels = 5, 
                                      n_trips_per_vessel = 5, 
                                      n_sets_per_trip = 4, 
                                      vessel_effect_sd = 0.5,
                                      trip_effect_sd = 0.3,
                                      base_catch_rate = 1) {
  
  # Generate vessel IDs
  vessels <- 1:n_vessels
  
  # Generate vessel effects (some vessels catch more than others)
  vessel_effects <- rnorm(n_vessels, 0, vessel_effect_sd)
  
  # Create empty dataframe to store all data
  total_sets <- n_vessels * n_trips_per_vessel * n_sets_per_trip
  data <- data.frame(
    vessel_id = integer(total_sets),
    vessel_name = character(total_sets),
    trip_id = integer(total_sets),
    set_id = integer(total_sets),
    catch_rate = numeric(total_sets),
    catch = integer(total_sets),
    catch_category = character(total_sets),
    stringsAsFactors = FALSE
  )
  
  row <- 1
  for (v in 1:n_vessels) {
    vessel_name <- paste("Vessel", LETTERS[v])
    vessel_effect <- vessel_effects[v]
    
    for (t in 1:n_trips_per_vessel) {
      # Generate trip effect (some trips catch more than others)
      trip_effect <- rnorm(1, 0, trip_effect_sd)
      
      for (s in 1:n_sets_per_trip) {
        # Calculate catch rate for this set
        catch_rate <- exp(log(base_catch_rate) + vessel_effect + trip_effect)
        
        # Simulate catch using Poisson distribution
        catch <- rpois(1, catch_rate)
        
        # Categorize catch for visual representation
        catch_category <- if (catch <= quantile(dpois(0:10, base_catch_rate), 0.33)) {
          "low"
        } else if (catch <= quantile(dpois(0:10, base_catch_rate), 0.67)) {
          "medium"
        } else {
          "high"
        }
        
        # Store in dataframe
        data$vessel_id[row] <- v
        data$vessel_name[row] <- vessel_name
        data$trip_id[row] <- t
        data$set_id[row] <- s
        data$catch_rate[row] <- catch_rate
        data$catch[row] <- catch
        data$catch_category[row] <- catch_category
        
        row <- row + 1
      }
    }
  }
  
  return(data)
}

# Create demo data
fleet_data <- create_demonstration_data()

# Function to apply different monitoring strategies
apply_monitoring_strategies <- function(data, p_monitor = 0.3, bias_factor = -5) {
  # Make copies for each strategy
  data_sets <- data
  data_vessels <- data
  data_trips <- data
  
  # Strategy 1: Random sets
  set.seed(42)
  data_sets$monitored <- rbinom(nrow(data_sets), 1, p_monitor)
  data_sets$strategy <- "Random Sets"
  
  # Strategy 2: Vessels (with potential bias toward vessels with lower catch rates)
  vessels <- unique(data$vessel_id)
  vessel_avg_catch <- sapply(vessels, function(v) mean(data$catch[data$vessel_id == v]))
  # Normalize to 0-1 scale for bias calculation
  normalized_catch <- (max(vessel_avg_catch) - vessel_avg_catch) / (max(vessel_avg_catch) - min(vessel_avg_catch))
  # Apply bias (higher values of normalized_catch = more likely to be monitored)
  vessel_prob <- p_monitor + bias_factor * (normalized_catch - mean(normalized_catch))
  vessel_prob <- pmin(pmax(vessel_prob, 0), 1)  # Bound between 0 and 1
  
  set.seed(42)
  vessel_monitored <- rbinom(length(vessels), 1, vessel_prob)
  data_vessels$monitored <- 0
  for (i in 1:length(vessels)) {
    if (vessel_monitored[i] == 1) {
      data_vessels$monitored[data_vessels$vessel_id == vessels[i]] <- 1
    }
  }
  data_vessels$strategy <- "Vessels"
  
  # Strategy 3: Trips (with potential bias toward trips with lower catch rates)
  trips <- unique(data[, c("vessel_id", "trip_id")])
  trip_avg_catch <- numeric(nrow(trips))
  for (i in 1:nrow(trips)) {
    v <- trips$vessel_id[i]
    t <- trips$trip_id[i]
    trip_avg_catch[i] <- mean(data$catch[data$vessel_id == v & data$trip_id == t])
  }
  
  # Normalize to 0-1 scale for bias calculation
  normalized_trip_catch <- (max(trip_avg_catch) - trip_avg_catch) / 
                          (max(trip_avg_catch) - min(trip_avg_catch))
  # Apply bias (higher values of normalized_catch = more likely to be monitored)
  trip_prob <- p_monitor + bias_factor * (normalized_trip_catch - mean(normalized_trip_catch))
  trip_prob <- pmin(pmax(trip_prob, 0), 1)  # Bound between 0 and 1
  
  set.seed(42)
  trip_monitored <- rbinom(nrow(trips), 1, trip_prob)
  data_trips$monitored <- 0
  for (i in 1:nrow(trips)) {
    v <- trips$vessel_id[i]
    t <- trips$trip_id[i]
    if (trip_monitored[i] == 1) {
      data_trips$monitored[data_trips$vessel_id == v & data_trips$trip_id == t] <- 1
    }
  }
  data_trips$strategy <- "Trips"
  
  # Combine all data
  combined_data <- rbind(data_sets, data_vessels, data_trips)
  return(combined_data)
}

# Apply monitoring strategies
monitored_data <- apply_monitoring_strategies(fleet_data)

# Calculate catch statistics for each strategy
calculate_catch_stats <- function(data) {
  data %>%
    group_by(strategy) %>%
    summarize(
      true_catch_rate = mean(catch),
      estimated_catch_rate = if(sum(monitored) > 0) mean(catch[monitored == 1]) else NA,
      bias = estimated_catch_rate - true_catch_rate,
      bias_percent = (bias / true_catch_rate) * 100,
      monitored_sets = sum(monitored),
      total_sets = n()
    )
}

catch_stats <- calculate_catch_stats(monitored_data)

# Create visualization functions

# 1. Fleet Visualization with dots representing sets
plot_fleet_structure <- function(data, strategy_name) {
  subset_data <- data[data$strategy == strategy_name,]
  
  ggplot(subset_data, aes(x = trip_id, y = vessel_id)) +
    geom_point(aes(color = factor(monitored), size = catch), alpha = 0.8) +
    scale_color_manual(values = c("0" = "#CCCCCC", "1" = "#FF5733"), 
                       name = "Monitored", 
                       labels = c("No", "Yes")) +
    scale_size_continuous(name = "Catch Amount", range = c(3, 8)) +  # Increased point size range
    labs(title = paste("Monitoring Strategy:", strategy_name),
         x = "Trip Number", 
         y = "Vessel") + 
    scale_y_continuous(labels = function(x) paste("Vessel", LETTERS[x])) +
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_blank())
}

# 2. Create comparison of estimated vs true catch rates
plot_catch_comparison <- function(stats) {
  stats_long <- pivot_longer(stats, 
                           cols = c(true_catch_rate, estimated_catch_rate),
                           names_to = "rate_type",
                           values_to = "value")
  
  ggplot(stats_long, aes(x = strategy, y = value, fill = rate_type)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_manual(values = c("steelblue", "coral"),
                     name = "Catch Rate",
                     labels = c("Estimated", "True")) +
    labs(title = "Comparison of True vs. Estimated Catch Rates",
         x = "Monitoring Strategy", 
         y = "Catch Rate") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# 3. Create a plot showing bias percentage by strategy
plot_bias <- function(stats) {
  ggplot(stats, aes(x = strategy, y = bias_percent)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "",
         x = "Monitoring Strategy", 
         y = "Percent Bias (%)") +
    theme_minimal()
}

# Create the plots
plot1 <- plot_fleet_structure(monitored_data, "Random Sets")
plot2 <- plot_fleet_structure(monitored_data, "Vessels")
plot3 <- plot_fleet_structure(monitored_data, "Trips")
plot4 <- plot_catch_comparison(catch_stats)
plot5 <- plot_bias(catch_stats)

# Combine plots using patchwork
combined_plot <- (plot1 + plot2 + plot3) / (plot4 + plot5 + plot_spacer())
combined_plot <- combined_plot + 
  plot_layout(heights = c(3, 1)) +
  plot_annotation(
    title = "Impact of Different Monitoring Strategies on Catch Rate Estimation",
    subtitle = "30% monitoring coverage with different allocation strategies",
    caption = "Each dot represents a fishing set. Larger dots = higher catch. Brighter dots = monitored sets."
  )

# Save the plot
ggsave("plots/conceptual_figure.png", combined_plot, width = 12, height = 8, dpi = 300)
ggsave("plots/lowres/conceptual_figure_lowres.png", combined_plot, width = 12, height = 8, dpi = 100)

# Also create a simpler conceptual figure just showing the different strategies
simple_plot <- (plot1 + plot2 + plot3) + 
  plot_layout(nrow = 1) +
  plot_annotation(
    title = "Different Monitoring Strategies for Fishing Fleet Coverage",
    subtitle = "Random Sets vs. Vessels vs. Trips",
    caption = "Brighter dots represent monitored sets. Size and color indicate catch amount."
  )

# Save the simple plot
ggsave("plots/conceptual_figure_simple.png", simple_plot, width = 15, height = 5, dpi = 300)
ggsave("plots/lowres/conceptual_figure_simple_lowres.png", simple_plot, width = 15, height = 5, dpi = 100)

# Print summary statistics
print(catch_stats)
