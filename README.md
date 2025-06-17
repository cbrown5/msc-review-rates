# msc-review-rates

## TODO
Up to implementing the model. I'm happy with formulation below
One tihng to check is that the selection for monitoring is properly centered so that the proportion selected is close to the target proportion.

### Scientific Context

The Marine Stewardship Council (MSC) requires fisheries to monitor fishing effort with a nominal 30% monitoring coverage rule. However, they haven't specified how that 30% should be distributed across the fleet. There's concern that fishing fleets may select their best 30% of vessels (those with low rates of bycatch) for monitoring, resulting in biased outcomes in terms of estimated bycatch rates versus actual rates.

The aim is to illustrate how different sampling strategies for monitoring tuna fisheries affects the estimated rates of catch and the ability to detect compliance events (false negative rates). We want to determine the optimal monitoring approach for accurate bycatch rates and compliance detection, which is likely to be 30% random allocation across all sets rather than structuring by trips or vessels.

Different monitoring systems have different capabilities:
- Electronic monitoring: Can potentially achieve 100% vessel coverage with random review of 30% of sets
- Electronic monitoring: in practice there is less than 100% coverage, so gaming may occur where the 'best' 30% of vessels are picked to have cameras
- Human observation: Typically covers whole trips (30% of trips) but is subject to potential gaming where vessels may select shorter trips or those with lower catch rates

### Model Formulation

The model is split into two modules, a catch event model and a monitoring module. 
Further details are in the file `model-formulation.md`

#### Catch event module 

The model was formulated as follows: 
```
for (v in 1:V){ # for each vessel
  T[v] ~ dpois(mu_trips) #sample number of trips in a year
  x[v] ~ dnorm(0, sigma_x^(-2)) # sample vessel level RE
  
  for (t in 1:T[v]){ # for each trip this vessel did
    S[v,t] ~ dpois(mu_sets) #sample number of sets on the trip 
    z[v,t] ~ dnorm(0, sigma_z^(-2)) # sample trip level random effect 

    mu[v,t] = beta_0 + z[v,t] + x[v] #calculate expected catch rate

    for (s in 1:S[v,t]){ # for each set
      y[v,t,s] ~ dnegbin(mu[v,t], tau) # sample catch for this set tau is the dispersion parameter
    }
  }
}

```

**Parameter Explanations:**
- `V`: Total number of vessels in the fleet
- `T[v]`: Number of trips taken by vessel v in a year, sampled from a Poisson distribution
- `mu_trips`: Expected number of trips per vessel per year
- `x[v]`: Vessel-level random effect for vessel v, sampled from a normal distribution
- `sigma_x`: Standard deviation parameter for the vessel-level random effects
- `S[v,t]`: Number of fishing sets on trip t of vessel v, sampled from a Poisson distribution
- `mu_sets`: Expected number of sets per trip
- `z[v,t]`: Trip-level random effect for trip t of vessel v, sampled from a normal distribution
- `sigma_z`: Standard deviation parameter for the trip-level random effects
- `mu[v,t]`: Expected catch rate for trip t of vessel v, combining fixed effect (beta_0) and random effects
- `beta_0`: Baseline expected catch rate (fixed effect)
- `y[v,t,s]`: Observed catch for set s on trip t of vessel v, following a negative binomial distribution
- `tau`: Dispersion parameter for the negative binomial distribution, controlling overdispersion

**Sampling Distribution:**
- Typical species: Negative binomial
- Very rare events or species: Binomial or zero-inflated distributions

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

### Implementation Plan

The code implementation needs to be straightforward to easily simulate different scenarios:

**Code Structure:**
- Modular implementation with 1-3 different scripts
- Keep scripts short and modular, not super long mega-scripts
- Plug-and-play distributions for sampling

**Parameter Files:**
- Clear definition of parameters needed for the model
- Parameters will require literature research to obtain values

**Function Design:**
- Intuitive, easy-to-use functions
- Simple parameter modification for different scenarios
- Functions that work in an intuitive way without obtuse parameter structures

### Expected Outcomes

The goal is not to do a comprehensive sensitivity analysis of all parameters, but to:
- Run simulations with different parameter sets
- Narrow down to 3-6 parameter sets that present a range of different situations
- Develop narratives or "stories" around these parameter sets
- Create one-page descriptions for each narrative that:
  - Describes the context
  - Puts parameter values into real-world context
  - Shows implications of different monitoring strategies
  - Presents conclusions about optimal monitoring allocation
- Provide overall recommendations on optimal ways of allocating monitoring

### Visualization Strategy

Develop figures that are:
- Colorful, attractive, and informative
- Simple and high-impact
- Suitable for information sheets
- Visually appealing, quick to interpret, and compelling

**Conceptual Figure Ideas:**
- Figures with colored dots representing different types of vessels/trips/sets
- Illustrations showing how different sampling approaches affect estimated catch rates
- Simple figures showing a fleet with different colors for vessels/trips
- Circles illustrating different sampling protocols
- Bar graphs showing estimated catch rates
- These could later be enhanced by graphic designers with realistic fishing vessels and fish

### Workflow

The implementation will be divided into phases:

**1. Planning Phase:**
- Structure notes into sections (scientific context, aims, model formulation, etc.)
- Define parameters and research literature values

**2. Implementation Phase:**
- Develop R code with focus on usability
- Create core functions for model and monitoring frameworks
- Implement in phases: reading parameter sets, running the model, visualizations

**3. Reporting Phase:**
- Generate figures
- Use R Markdown for reporting (easy to replicate and update)
- Integrate figures directly into R Markdown without including entire process flow
- Conduct sensitivity analysis to identify extreme parameter sets
- Create one page per narrative describing outcomes
- Add a summary page with overall recommendations