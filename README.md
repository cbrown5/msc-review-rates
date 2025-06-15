# msc-review-rates

## TODO
The code works ok. 
Need to check logic in each step. 
Need to decide if sampling is needed for trips and sets per vessel, or if to fix these? Maybe use Sols data to decide. 
Then run to make plots to see if they make sense
Then update parameters

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

The simulation model resembles a generalized linear mixed effects model with these components:

**Sampling Distribution:**
- Common species: Poisson distribution
- Rare species: Negative binomial
- Very rare events: Binomial or zero-inflated distributions

Each sample represents one fishing set (e.g., longline set, purse seine net).

**Variance Components:**
- Residual variance (set-level variability)
- Trip variance (trip-level variability)
- Vessel variance (vessel-level variability)
- Optional: Company-level variability for fishing companies

**Bias Parameters:**
- Interaction between monitoring methods and variance components
- Represents bias toward vessels/trips with higher or lower than average catch rates

### Monitoring Module

The monitoring module determines how monitoring of catches is distributed across the fleet with several parameters:

**Parameters:**
- Total amount of monitoring (percentage of fishing sets monitored)
- Distribution of monitoring across fleet components
- Coverage within selected units (percentage of trips/sets monitored within selected vessels)

**Distribution Strategies:**
- Random across sets
- Random across vessels (all sets of particular vessels are sampled)
- Random across trips within vessels

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