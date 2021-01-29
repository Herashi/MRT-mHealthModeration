# Assessing Time-Varying Causal Effect Moderation in the Presence of Cluster-Level Treatment Effect Heterogeneity

##### Case study, simulations and estimation functions

The directories above correspond to the simulation in the paper *Assessing Time-Varying Causal Effects in the Presenceof Cluster-Level Treatment Effect Heterogeneity*. Results for each scenario are generated by the following R files.

File | Description
---- | ----
[test.R](test.R) | test file
[group.R](group.R) | Group structures
[init.R](init.R) | Loads required packages and reads source files
[sim-final.R](sim-final.R) | Simulation related functions
[xgeepack.R](xgeepack.R) | Extensions for the geepack R package; extract, from a geepack model object, elements (e.g. working covariance, estimating function) needed for variance calculations
[new-meat.R](new-meat.R) | Extensions for the geepack R package; extract, from a geepack model object, elements (e.g. working covariance, estimating function) needed for variance calculations
[xzoo.R](xzoo.R) | Extensions for the zoo R package; apply lags, difference, rolling summaries to a sample of time series





