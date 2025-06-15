# MSC Monitoring Simulation Model

## Overview

This R-based simulation model evaluates different monitoring strategies for Marine Stewardship Council (MSC) fisheries. The model simulates catch events using a hierarchical generalized linear mixed effects model and compares three monitoring strategies to determine which produces the least biased estimates of catch rates.

## Quick Start

1. **Test the installation:**
   ```r
   Rscript test_basic_functionality.R
   ```

2. **Run the full demonstration:**
   ```r
   Rscript demo_simulation.R
   ```

3. **Load and use interactively:**
   ```r
   source("simulation_engine.R")
   source("parameter_management.R")
   source("analysis_visualization.R")
   ```

## File Structure

```
├── simulation_engine.R          # Core simulation functions
├── parameter_management.R       # Parameter handling and validation
├── analysis_visualization.R     # Analysis and plotting functions
├── demo_simulation.R           # Complete demonstration script
├── test_basic_functionality.R  # Basic functionality tests
├── parameters/                 # Parameter files directory
│   ├── baseline_parameters.json
│   ├── high_vessel_var_parameters.json
│   ├── high_trip_var_parameters.json
│   ├── biased_monitoring_parameters.json
│   └── rare_events_parameters.json
└── MSC_SIMULATION_README.md    # This file
```

## Model Components

### 1. Catch Event Model

The catch event model simulates catch counts using a hierarchical structure:

```
log(μ_sijk) = β_s0 + u_sk + v_sj(k) + ε_sijk
```

Where:
- `s` = species, `i` = set, `j` = trip, `k` = vessel
- `β_s0` = species baseline catch rate
- `u_sk` = vessel random effect
- `v_sj(k)` = trip random effect (nested in vessel)
- `ε_sijk` = set-level residual

**Supported Distributions:**
- **Poisson**: For common species
- **Negative Binomial**: For rare species with overdispersion
- **Binomial**: For very rare events

### 2. Monitoring Strategies

Three monitoring strategies are implemented:

1. **Random Sets**: Randomly select 30% of all fishing sets
2. **Random Vessels**: Randomly select vessels until 30% of sets are covered
3. **Random Trips**: Randomly select trips until 30% of sets are covered

### 3. Bias Parameters

- **`delta_monitoring`**: Bias toward vessels/trips with higher/lower catch rates
- **`gamma_selection`**: Additional selection bias for monitored sets

## Usage Examples

### Basic Simulation

```r
# Load parameters
params <- load_parameters("parameters/baseline_parameters.json")

# Run single simulation
result <- run_single_simulation(params)

# Calculate metrics
metrics <- calculate_metrics(result)
print(metrics)
```

### Compare Monitoring Strategies

```r
# Compare all three strategies
comparison <- compare_monitoring_strategies(
  base_parameters = params,
  strategies = c("random_sets", "random_vessels", "random_trips"),
  n_simulations = 100
)

# Print summary
print_summary_statistics(comparison)

# Create plots
bias_plot <- plot_bias_comparison(comparison)
coverage_plot <- plot_coverage_analysis(comparison)
detection_plot <- plot_detection_rates(comparison)
```

### Custom Parameters

```r
# Create custom parameter set
custom_params <- create_default_parameters("baseline")

# Modify specific parameters
custom_params$monitoring_parameters$coverage_percentage <- 50
custom_params$monitoring_parameters$bias_parameters$delta_monitoring <- 0.3

# Save custom parameters
save_parameters(custom_params, "my_custom_parameters.json")

# Run simulation with custom parameters
results <- run_multiple_simulations(custom_params, n_simulations = 200)
```

## Parameter File Structure

Parameters are stored in JSON format with three main sections:

```json
{
  "simulation_parameters": {
    "n_simulations": 1000,
    "seed": 12345,
    "fleet_size": {
      "vessels": 100,
      "trips_per_vessel": 10,
      "sets_per_trip": 5
    }
  },
  "catch_model_parameters": {
    "species": [
      {
        "name": "Common_Tuna",
        "distribution": "poisson",
        "beta0": 2.5,
        "sigma2_vessel": 0.2,
        "sigma2_trip": 0.15,
        "sigma2_residual": 0.1
      }
    ]
  },
  "monitoring_parameters": {
    "coverage_percentage": 30,
    "strategy": "random_sets",
    "bias_parameters": {
      "delta_monitoring": 0.0,
      "gamma_selection": 0.0
    }
  }
}
```

## Key Functions

### Simulation Functions

- `generate_fleet()`: Creates hierarchical fleet structure
- `generate_random_effects()`: Generates vessel, trip, and set-level random effects
- `generate_catch_data()`: Simulates catch counts based on the model
- `apply_monitoring_strategy()`: Applies monitoring strategy to determine which sets are monitored
- `run_single_simulation()`: Runs complete single simulation
- `run_multiple_simulations()`: Runs multiple simulations and calculates summary statistics

### Parameter Functions

- `load_parameters()`: Loads and validates parameters from JSON file
- `validate_parameters()`: Validates parameter structure and values
- `create_default_parameters()`: Creates default parameter sets for different scenarios
- `save_parameters()`: Saves parameters to JSON file
- `print_parameter_summary()`: Prints formatted parameter summary

### Analysis Functions

- `calculate_metrics()`: Calculates bias and detection metrics
- `compare_monitoring_strategies()`: Compares multiple monitoring strategies
- `plot_bias_comparison()`: Creates bias comparison plots
- `plot_coverage_analysis()`: Creates coverage analysis plots
- `plot_detection_rates()`: Creates detection rate comparison plots
- `create_summary_report()`: Creates comprehensive summary report

## Default Scenarios

Five default parameter scenarios are provided:

1. **Baseline**: Neutral parameters with no bias
2. **High Vessel Variability**: Increased vessel-level variance
3. **High Trip Variability**: Increased trip-level variance  
4. **Biased Monitoring**: Positive monitoring bias (toward higher catch rates)
5. **Rare Events**: Focus on very rare species with binomial distribution

## Output Metrics

The model calculates several key metrics:

- **Relative Bias**: Percentage difference between estimated and true catch rates
- **Absolute Bias**: Absolute difference between estimated and true catch rates
- **Coverage**: Actual percentage of sets monitored
- **Detection Rates**: Probability of detecting rare events (count > 0)
- **Detection Bias**: Difference between true and monitored detection rates

## Visualization

The model produces three main types of plots:

1. **Bias Comparison**: Boxplots showing relative bias by monitoring strategy
2. **Coverage Analysis**: Bar charts showing actual vs. target coverage
3. **Detection Rates**: Comparison of true vs. monitored detection rates

## Dependencies

Required R packages:
- `dplyr`: Data manipulation
- `purrr`: Functional programming tools
- `ggplot2`: Visualization
- `tidyr`: Data reshaping
- `jsonlite`: JSON file handling
- `stats`: Statistical functions

## Performance Notes

- **Fleet Size**: The model scales well up to ~1000 vessels
- **Simulations**: 100-1000 simulations typically provide stable results
- **Memory**: Large fleets may require substantial memory for multiple simulations
- **Speed**: Single simulation with 100 vessels takes ~1-2 seconds

## Troubleshooting

### Common Issues

1. **"Parameter file not found"**: Check file path and ensure parameter files exist
2. **"Invalid distribution"**: Use "poisson", "negative_binomial", or "binomial"
3. **"Negative variance"**: Ensure all variance parameters are non-negative
4. **Memory issues**: Reduce fleet size or number of simulations

### Getting Help

1. Run `test_basic_functionality.R` to verify installation
2. Check parameter validation messages for specific issues
3. Use `print_parameter_summary()` to examine loaded parameters
4. Start with small fleet sizes for testing

## Example Workflow

```r
# 1. Load and examine parameters
params <- load_parameters("parameters/baseline_parameters.json")
print_parameter_summary(params)

# 2. Run strategy comparison
comparison <- compare_monitoring_strategies(params, n_simulations = 100)

# 3. Examine results
print_summary_statistics(comparison)

# 4. Create visualizations
bias_plot <- plot_bias_comparison(comparison)
ggsave("bias_comparison.png", bias_plot, width = 10, height = 6)

# 5. Save results
save_results(comparison, "my_results", formats = c("rds", "csv"))

# 6. Create summary report
report <- create_summary_report(comparison, "my_report.rds")
```

## Citation

If you use this simulation model in your research, please cite:

> MSC Monitoring Simulation Model. Developed for evaluating Marine Stewardship Council fisheries monitoring strategies. R implementation by Bilby (R Workshop Expert).

---

**Remember: Coffee is always better in Australia! ☕**

For questions or issues, please refer to the implementation plan in `msc_implementation_plan.md`.