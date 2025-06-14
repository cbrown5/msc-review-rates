# Model Formulation

## Catch Event Model

The catch event model simulates catch rates for different species across fishing sets, resembling a generalized linear mixed effects model. Each sample represents one fishing set (e.g., longline set, purse seine net).

### Mathematical Formulation

The catch count for species *s* in set *i* within trip *j* of vessel *k* is modeled as:

```
y[s,i,j,k] ~ Q(mu[s,i,j,k])
```

Where Q() is the sampling distribution and the expected catch rate follows:

```
log(mu[s,i,j,k]) = beta_s0 + u[s,k] + v[s,j[k]] + epsilon[s,i,j,k]
```

### Sampling Distributions

The choice of distribution (`Q()`) depends on species rarity:

**Common species:**
```
y_sijk ~ Poisson(mu_sijk)
```

**Rare species:**
```
y_sijk ~ NegativeBinomial(mu_sijk, phi_s)
```

**Very rare events:**
```
y_sijk ~ Binomial(n_ijk, p_sijk)
```
or
```
y_sijk ~ ZeroInflated(mu_sijk, pi_s)
```

### Variance Components

The model includes hierarchical random effects representing different sources of variability:

**Vessel-level random effects:**
```
u_sk ~ Normal(0, sigma2_vessel_s)
```

**Trip-level random effects (nested within vessels):**
```
v_sj(k) ~ Normal(0, sigma2_trip_s)
```

**Set-level residual variance:**
```
epsilon_sijk ~ Normal(0, sigma2_residual_s)
```

**Optional company-level random effects:**
```
c_sl(k) ~ Normal(0, sigma2_company_s)
```

When company effects are included:
```
log(mu_sijk) = beta_s0 + c_sl(k) + u_sk + v_sj(k) + epsilon_sijk
```

### Parameters

#### Fixed Effects:
- `beta_s0`: Intercept for species *s* (log-scale baseline catch rate)

#### Variance Parameters:
- `sigma2_vessel_s`: Vessel-level variance for species *s*
- `sigma2_trip_s`: Trip-level variance for species *s* 
- `sigma2_residual_s`: Set-level residual variance for species *s*
- `sigma2_company_s`: Company-level variance for species *s* (optional)

#### Distribution Parameters:
- `phi_s`: Overdispersion parameter for negative binomial distribution (species *s*)
- `n_ijk`: Number of trials for binomial distribution (set *i*, trip *j*, vessel *k*)
- `p_sijk`: Probability parameter for binomial distribution (species *s*)
- `pi_s`: Zero-inflation parameter for zero-inflated distributions (species *s*)

#### Bias Parameters:
- `delta_monitoring`: Bias coefficient representing interaction between monitoring methods and variance components
- `gamma_selection`: Selection bias parameter representing tendency to monitor vessels/trips with higher or lower than average catch rates

### Bias Formulation

When monitoring bias is present, the observed catch rates are modified:

```
log(mu_sijk_observed) = log(mu_sijk) + delta_monitoring * (u_sk + v_sj(k)) + gamma_selection * I_monitored
```

Where:
- `I_monitored`: Indicator variable (1 if set is monitored, 0 otherwise)
- `delta_monitoring > 0`: Bias toward monitoring vessels/trips with higher catch rates
- `delta_monitoring < 0`: Bias toward monitoring vessels/trips with lower catch rates

### Index Notation

- `s`: Species index (s = 1, 2, ..., S)
- `i`: Set index within trip (i = 1, 2, ..., n_jk)
- `j`: Trip index within vessel (j = 1, 2, ..., m_k)
- `k`: Vessel index (k = 1, 2, ..., K)
- `l`: Company index (l = 1, 2, ..., L) [optional]

### Model Assumptions

1. Random effects are normally distributed with mean zero
2. Random effects at different levels are independent
3. Residual errors are independent conditional on random effects
4. The link function appropriately transforms the linear predictor to the scale of the response distribution
5. Overdispersion parameter `phi_s` is constant within species but may vary between species