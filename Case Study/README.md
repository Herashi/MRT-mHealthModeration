# Case Study: Intern Health Study

This folder contains all the code for generating both the direct and indirect effect estimates using the Intern Health Study.

File | Description
---- | ----
[direct_effect.R](direct_effect.R) | Input is the imputed IHS datasets and computes direct effects on average weekly mood score, log step count, and log sleep minutes.
[indirect_effect.R](direct_effect.R) | Input is the imputed IHS datasets and generates the datasets necessary to estimate indirect effects.
[indirect_effect_estimation.R](indirect_effect_estimation.R) | Input is the imputed indirect IHS datasets and computes indirect effects on average weekly mood score, log step count, and log sleep minutes.
[indirect_effect_multimp_comb.R](indirect_effect_multimp_comb.R) | Input is the imputed indirect effect estimates and computes combined indirect effects on average weekly mood score, log step count, and log sleep minutes.
