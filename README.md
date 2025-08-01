# msc-review-rates

## TODO
Update model for simulating sets per trip? 
Add technical annex to document. 
Note for editing parameters - sigma_z is trip variancSD, sigma_x is vessel SD


### Scientific Context

The Marine Stewardship Council (MSC) requires fisheries to monitor fishing effort with a nominal 30% monitoring coverage rule. However, they haven't specified how that 30% should be distributed across the fleet. There's concern that fishing fleets may select their best 30% of vessels (those with low rates of bycatch) for monitoring, resulting in biased outcomes in terms of estimated bycatch rates versus actual rates.

The aim is to illustrate how different sampling strategies for monitoring tuna fisheries affects the estimated rates of catch and the ability to detect compliance events (false negative rates). We want to determine the optimal monitoring approach for accurate bycatch rates and compliance detection, which is likely to be 30% random allocation across all sets rather than structuring by trips or vessels.

Different monitoring systems have different capabilities:
- Electronic monitoring: Can potentially achieve 100% vessel coverage with random review of 30% of sets
- Electronic monitoring: in practice there is less than 100% coverage, so gaming may occur where the 'best' 30% of vessels are picked to have cameras
- Human observation: Typically covers whole trips (30% of trips) but is subject to potential gaming where vessels may select shorter trips or those with lower catch rates

### Model Formulation

The model is split into two modules, a catch event model and a monitoring module. 
Used FSM as this had the most reliable data in logbooks. 

#### Parameter calculations 

Parameters were calculated from a longline tuna fleet. Note that below I assume sets per trip are negative binomially distributed. However data indicated an unusual peaked distribution with high dispersion. 

See `tuna-bycatch-study/fleet-parameters-msc-analysis.R` for calculation of parameters from real data. 
Note that the distribution of sets per trip is odd, more peaked than a negbin, but also with long tails. Using negbin for now. 

#### Catch event module 

The model was formulated as follows: 
```
for (v in 1:V){ # for each vessel
  T[v] ~ dnegbin(mu_trips, theta_trip) #sample number of trips in a year
  x[v] ~ dnorm(0, sigma_x^(-2)) # sample vessel level RE
  
  for (t in 1:T[v]){ # for each trip this vessel did
    S[v,t] ~ dnegbin(mu_sets, theta_sets) #sample number of sets on the trip 
    z[v,t] ~ dnorm(0, sigma_z^(-2)) # sample trip level random effect 

    mu[v,t] = beta_0 + z[v,t] + x[v] #calculate expected catch rate

    for (s in 1:S[v,t]){ # for each set
      y[v,t,s] ~ dnegbin(mu[v,t], theta) # sample catch for this set theta is the dispersion parameter
    }
  }
}

```

**Parameter Explanations:**
- `V`: Total number of vessels in the fleet
- `T[v]`: Number of trips taken by vessel v in a year, sampled from a negative binomial distribution
- `mu_trips`: Expected number of trips per vessel per year
- `theta_trips`: Dispersion parameter for the negative binomial distribution of trips
- `x[v]`: Vessel-level random effect for vessel v, sampled from a normal distribution
- `sigma_x`: Standard deviation parameter for the vessel-level random effects
- `S[v,t]`: Number of fishing sets on trip t of vessel v, sampled from a negative binomial distribution
- `mu_sets`: Expected number of sets per trip
- `theta_sets`: Dispersion parameter for the negative binomial distribution of sets
- `z[v,t]`: Trip-level random effect for trip t of vessel v, sampled from a normal distribution
- `sigma_z`: Standard deviation parameter for the trip-level random effects
- `mu[v,t]`: Expected catch rate for trip t of vessel v, combining fixed effect (beta_0) and random effects
- `beta_0`: Baseline expected catch rate (fixed effect)
- `y[v,t,s]`: Observed catch for set s on trip t of vessel v, following a negative binomial distribution
- `theta`: Dispersion parameter for the negative binomial distribution of catch, controlling overdispersion

**Sampling Distributions:**
- Catch per set: Negative binomial
- Sets per trip and  trips per vessel: Negative binomial

Each sample represents one fishing set (e.g., longline set, purse seine net).

#### Monitoring Module

The monitoring module determines how monitoring of catches is distributed across the fleet with several parameters:

**Parameters:**
- Total amount of monitoring (percentage of fishing sets monitored)
- Distribution of monitoring across fleet components, with option for bias
- Coverage within selected units (percentage of trips/sets monitored within selected vessels)

The monitoring module was formulated as follows:

```

# Strategy 1 sampling across sets
#matrix for storing monitoring status
M_sets = matrix(V,T,S)
phi_sets = logit(p_monitor)
M_sets[v,t,s] ~ dbern(inverse_logit(phi))

# Strategy 2 sampling random vessels
M_vessels = matrix(V,T,S)
phi_vessels = array(dim=c(V,T,S))
for (v in 1:V) {
  phi_vessels[v,,] = logit(p_monitor) + bias_v * x[v]
}
M_vessels[v,t,s] ~ dbern(inverse_logit(phi_vessels[v,t,s]))
# Strategy 3: Random selection of trips within vessels
M_trips = matrix(V,T,S)
phi_trips = array(dim=c(V,T,S))
for (v in 1:V) {
  for (t in 1:T[v]) {
    phi_trips[v,t,] = logit(p_monitor) + bias_factor * z[v,t]
  }
}
M_trips[v,t,s] ~ dbern(inverse_logit(phi_trips[v,t,s]))
     
```

**Parameter Explanations:**
- `M_sets`, `M_vessels`, `M_trips`: Matrices indicating monitoring status for each set under different strategies (1 = monitored, 0 = not monitored)
- `p_monitor`: Base proportion of sets to be monitored (e.g., 0.3 for 30% coverage)
- `phi_sets`, `phi_vessels`, `phi_trips`: Logit-transformed probabilities of monitoring for each strategy
- `bias_v`: Bias factor for vessel-based selection (when > 0, vessels with lower catch rates are more likely selected)
- `bias_factor`: Bias factor for trip-based selection (when > 0, trips with lower catch rates are more likely selected)
- `x[v]`: Vessel-level random effect used in the bias mechanism for vessel selection
- `z[v,t]`: Trip-level random effect used in the bias mechanism for trip selection

**Bias Mechanism:**
- When bias_factor > 0, vessels/trips with lower catch rates (negative random effects) are more likely to be selected
- Higher bias_factor values lead to stronger selection bias
- The bias operates through the vessel random effect (x[v]) or trip random effect (z[v,t]) depending on strategy
- logit_inverse(x) = 1/(1+exp(-x)) transforms logit values back to probabilities

**Distribution Strategies:**
- Random across sets: Each set has equal probability of being monitored
- Random across vessels: Selected vessels have all their sets monitored
- Random across trips within vessels: Selected trips have all their sets monitored

### Catch statistic estimation

For a given monitoring strategy `M` and catch data `y`, the estimated catch rate per set can be calculated as follows:

```
# Apply monitoring to estimate catch rate
estimated_catch_rate = sum(y[v,t,s] * M[v,t,s]) / sum(M[v,t,s]) 
true_catch_rate = sum(y[v,t,s])/prod(dim(y))
bias = estimated_total_catch - true_total_catch
bias_percent = bias / true_catch_rate * 100
```

## Directory Structure and Script Usage

### Main Directories

- `/` - Root directory containing main R scripts and configuration files
- `/outputs/` - Contains simulation results in CSV format
- `/plots/` - Contains generated figures from the simulation results
  - `/plots/lowres/` - Low-resolution versions of the plots

### Main Files

#### R Scripts

1. **`1_run_simulations.R`**
   - **Purpose**: Main script to run the full simulation with multiple parameter sets
   - **Usage**: 
     ```bash
     Rscript 1_run_simulations.R
     ```
   - This script runs simulations for each parameter set defined in `parameters.csv`, with 250 replications per set. Results are saved to CSV files in the `/outputs/` directory.

2. **`2_plots.R`**
   - **Purpose**: Creates visualizations of the simulation results
   - **Usage**:
     ```bash
     Rscript 2_plots.R
     ```
   - Generates various plots showing bias in catch rate estimation across different monitoring strategies and saves them to the `/plots/` directory.

3. **`simulation_functions.R`**
   - **Purpose**: Core functions for the simulation model
   - **Usage**: Not run directly; imported by other scripts
   - Contains functions for three modules:
     - Catch event module - Simulates fleet structure and catches
     - Monitoring module - Implements different monitoring strategies
     - Catch statistic estimation - Calculates estimated vs true catch rates

4. **`quick_run_test.R`**
   - **Purpose**: Quick test script for running a small simulation
   - **Usage**:
     ```bash
     Rscript quick_run_test.R
     ```
   - Runs a small simulation with just 5 replications using the first parameter set for testing purposes.

5. **`test_single_run.R`**
   - **Purpose**: Script to verify the monitoring functions work correctly
   - **Usage**:
     ```bash
     Rscript test_single_run.R
     ```
   - Tests the functionality of a single simulation run using a specific parameter set.

6. **`conceptual_figure.R`**
   - **Purpose**: Generates conceptual figures illustrating different monitoring strategies
   - **Usage**:
     ```bash
     Rscript conceptual_figure.R
     ```
   - Creates visualizations that explain the conceptual framework of monitoring strategies.

7. **`render-ms.R`**
   - **Purpose**: Renders the R Markdown report into a Word document
   - **Usage**:
     ```bash
     Rscript render-ms.R
     ```
   - Compiles `report.rmd` into `msc-review-rates-report.docx`.

#### Configuration and Data Files

1. **`parameters.csv`**
   - Contains parameter sets for different simulation scenarios
   - Each row represents a different parameter set with values for fleet size, trip frequency, catch rates, etc.

#### How to Run the Complete Workflow

For a complete analysis workflow, run the scripts in the following order:

1. First, run the simulations:
   ```bash
   Rscript 1_run_simulations.R
   ```

2. Next, generate the plots:
   ```bash
   Rscript 2_plots.R
   ```

3. Finally, render the report:
   ```bash
   Rscript render-ms.R
   ```

#### Output Files

The simulation generates two main output files:

1. `outputs/simulation_results_full_tidy.csv`: Complete simulation results with data for each replication
2. `outputs/simulation_results_summary_tidy.csv`: Summary statistics across replications

Various plots are saved to the `plots/` directory, showing different aspects of the simulation results.

