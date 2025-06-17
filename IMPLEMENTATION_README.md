# MSC Review Rates Simulation - Implementation Guide

This implementation provides a complete simulation framework for analyzing how different monitoring strategies affect the accuracy of estimated catch rates in Marine Stewardship Council (MSC) fisheries.

## Files Overview

### Core Files
- **`simulation_functions.R`** - Contains all core functions for the three simulation modules
- **`parameters.csv`** - Parameter sets defining different simulation scenarios
- **`run_simulations.R`** - Main script to run the full simulation study
- **`visualize_results.R`** - Script to create visualizations of results
- **`test_implementation.R`** - Test script to verify the implementation works correctly

### Documentation
- **`msc_implementation_plan.md`** - Detailed implementation plan and code documentation
- **`README.md`** - Original project description and model formulation

## Quick Start

### 1. Test the Implementation
First, verify everything works correctly:
```r
source("test_implementation.R")
```

### 2. Run the Full Simulation
Run all 6 parameter sets with 1000 replications each:
```r
source("run_simulations.R")
```
**Note:** This will take some time to complete (estimated 10-30 minutes depending on your system).

### 3. Create Visualizations
After the simulation completes, generate plots:
```r
source("visualize_results.R")
```

## Parameter Sets

The simulation includes 6 parameter sets designed to explore different scenarios:

1. **Baseline** - Standard parameters with no bias
2. **High vessel variance** - Increased variation between vessels (σ_x = 0.8)
3. **High trip variance** - Increased variation between trips (σ_z = 0.6)
4. **High vessel bias** - Strong bias in vessel selection (bias_v = 2.0)
5. **High trip bias** - Strong bias in trip selection (bias_factor = 2.0)
6. **Rare events** - Low catch rates with high dispersion (β_0 = 0.5, τ = 3.0)

## Monitoring Strategies

The simulation compares three monitoring strategies:

1. **Sets** - Random monitoring across individual fishing sets
2. **Vessels** - Monitoring selected vessels (all their sets)
3. **Trips** - Monitoring selected trips (all sets within selected trips)

Each strategy can incorporate bias where vessels/trips with lower catch rates are preferentially selected for monitoring.

## Output Files

### Simulation Results
- **`simulation_results_full.csv`** - Complete results for all replications
- **`simulation_results_summary.csv`** - Summary statistics (means, confidence intervals)

### Visualizations
- **`bias_comparison.png`** - Bias comparison across strategies and parameter sets
- **`estimated_vs_true.png`** - Estimated vs. true catch rates scatter plot
- **`coverage_comparison.png`** - Mean estimated catch rates by strategy
- **`bias_magnitude.png`** - Absolute bias comparison
- **`bias_distribution_X.png`** - Bias distribution plots for each parameter set
- **`combined_results.png`** - Combined summary plot

## Key Functions

### Catch Event Module
- `generate_fleet_structure()` - Creates vessel/trip/set structure
- `generate_random_effects()` - Generates vessel and trip random effects
- `simulate_catches()` - Simulates catches using negative binomial distribution

### Monitoring Module
- `apply_set_monitoring()` - Random monitoring across sets
- `apply_vessel_monitoring()` - Vessel-based monitoring with optional bias
- `apply_trip_monitoring()` - Trip-based monitoring with optional bias

### Estimation Module
- `estimate_catch_statistics()` - Calculates estimated vs. true catch rates
- `run_single_simulation()` - Runs complete simulation for one replication

## Model Details

### Hierarchical Structure
```
Fleet → Vessels → Trips → Sets → Catches
```

### Catch Model
```r
# For each vessel v, trip t, set s:
mu[v,t] = exp(β_0 + x[v] + z[v,t])
y[v,t,s] ~ NegBin(mu[v,t], τ)
```

Where:
- `β_0` = baseline catch rate
- `x[v]` = vessel random effect ~ N(0, σ_x²)
- `z[v,t]` = trip random effect ~ N(0, σ_z²)
- `τ` = dispersion parameter

### Monitoring Model
```r
# Probability of monitoring with bias:
p_monitor[v,t,s] = logit⁻¹(logit(p_base) + bias × effect)
```

Where `effect` is the vessel or trip random effect, and `bias` determines selection preference.

## Expected Results

The simulation will demonstrate:
- **Random set monitoring** typically provides unbiased estimates
- **Vessel/trip monitoring with bias** can lead to systematic under- or over-estimation
- **High variance scenarios** increase uncertainty in all strategies
- **Rare event scenarios** may show different bias patterns

## Customization

### Adding New Parameter Sets
Edit `parameters.csv` to add new rows with different parameter combinations.

### Modifying Simulation Settings
In `run_simulations.R`, adjust:
- `n_replications` - Number of replications per parameter set
- Parameter ranges or distributions in the functions

### Adding New Monitoring Strategies
Create new functions following the pattern of existing monitoring functions in `simulation_functions.R`.

## Performance Notes

- Full simulation (6 parameter sets × 1000 replications × 3 strategies) = 18,000 individual simulations
- Each simulation generates ~2,500 fishing sets on average
- Total computation involves ~45 million individual catch events
- Runtime depends on system performance but typically 10-30 minutes
