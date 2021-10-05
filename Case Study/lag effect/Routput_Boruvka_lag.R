> testEstimates(mood_results)

Call:
  
  testEstimates(model = mood_results)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                        1.018     0.164     6.191   387.437     0.000     0.284     0.225 
week_categoryactivity              0.081     0.101     0.809    97.316     0.420     0.792     0.453 
week_categorymood                  0.082     0.104     0.786    90.034     0.434     0.850     0.471 
week_categorysleep                 0.129     0.100     1.299   107.987     0.197     0.723     0.430 
STEP_COUNTprev                    -0.001     0.004    -0.214    55.805     0.831     1.401     0.598 
SLEEP_COUNTprev                   -0.006     0.005    -1.250   111.514     0.214     0.703     0.423 
MOODprev                           0.507     0.016    32.019   209.484     0.000     0.431     0.308 
Sex                                0.015     0.018     0.825   647.327     0.409     0.207     0.174 
PHQtot0                            0.002     0.004     0.412   188.438     0.681     0.465     0.325 
Neu0                              -0.006     0.001    -4.614   464.521     0.000     0.254     0.206 
depr0                              0.082     0.020     4.074   161.385     0.000     0.522     0.351 
pre_intern_mood                    0.309     0.015    20.867   400.748     0.000     0.278     0.222 
pre_intern_sleep                   0.000     0.000     2.472   133.036     0.015     0.607     0.387 
pre_intern_sqrt_step               0.001     0.001     0.924    45.833     0.360     1.808     0.658 
week_categoryactivity:MOODprev    -0.009     0.013    -0.694   105.280     0.489     0.739     0.435 
week_categorymood:MOODprev        -0.012     0.014    -0.850    92.619     0.397     0.828     0.464 
week_categorysleep:MOODprev       -0.018     0.013    -1.334   114.903     0.185     0.685     0.417 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(mood_results2)

Call:
  
  testEstimates(model = mood_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                        0.512     0.468     1.095   181.761     0.275     0.478     0.331 
week_categoryactivity              0.374     0.362     1.033   329.348     0.302     0.316     0.245 
week_categorymood                  0.378     0.352     1.074   436.831     0.283     0.264     0.212 
week_categorysleep                 0.419     0.304     1.379   168.823     0.170     0.505     0.343 
STEP_COUNTprev                     0.003     0.009     0.359    79.482     0.721     0.957     0.501 
SLEEP_COUNTprev                   -0.012     0.017    -0.702    68.209     0.485     1.118     0.541 
MOODprev                           0.545     0.042    13.077   382.549     0.000     0.287     0.227 
Sex                                0.114     0.057     1.992  2143.254     0.047     0.104     0.095 
PHQtot0                           -0.008     0.008    -1.016   385.327     0.310     0.285     0.226 
Neu0                              -0.009     0.004    -2.438   638.322     0.015     0.208     0.175 
depr0                              0.032     0.060     0.533   558.469     0.594     0.226     0.187 
pre_intern_mood                    0.342     0.038     8.993   752.404     0.000     0.189     0.161 
pre_intern_sleep                   0.000     0.000     0.590   125.652     0.556     0.636     0.398 
pre_intern_sqrt_step               0.002     0.001     1.236   304.774     0.218     0.333     0.255 
week_categoryactivity:MOODprev    -0.050     0.047    -1.057   301.313     0.291     0.335     0.256 
week_categorymood:MOODprev        -0.047     0.044    -1.053   353.551     0.293     0.302     0.236 
week_categorysleep:MOODprev       -0.062     0.040    -1.540   198.059     0.125     0.449     0.317 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(mood_results3)

Call:
  
  testEstimates(model = mood_results3)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                           0.655     0.691     0.947   990.481     0.344     0.161     0.140 
week_categoryactivity                 0.505     0.547     0.923   479.303     0.357     0.249     0.202 
week_categorymood                     0.758     0.584     1.297   307.801     0.196     0.331     0.253 
week_categorysleep                    0.350     0.518     0.674   240.766     0.501     0.391     0.287 
STEP_COUNTprev                        0.003     0.009     0.330    69.379     0.742     1.098     0.536 
SLEEP_COUNTprev                      -0.011     0.017    -0.665    62.906     0.508     1.220     0.563 
MOODprev                              0.546     0.049    11.259   847.770     0.000     0.176     0.152 
Sex                                   0.111     0.063     1.762  2660.422     0.078     0.092     0.085 
PHQtot0                              -0.006     0.008    -0.830   278.470     0.407     0.354     0.266 
Neu0                                 -0.010     0.003    -3.280   336.183     0.001     0.312     0.242 
depr0                                 0.021     0.054     0.381   464.106     0.704     0.254     0.206 
pre_intern_mood                       0.333     0.032    10.507   358.000     0.000     0.299     0.235 
pre_intern_sleep                      0.000     0.000     0.476   100.823     0.635     0.767     0.445 
pre_intern_sqrt_step                  0.002     0.001     1.504   208.209     0.134     0.433     0.309 
week_categoryactivity:MOODprev       -0.055     0.052    -1.052   376.156     0.294     0.290     0.229 
week_categorymood:MOODprev           -0.062     0.048    -1.293   294.873     0.197     0.340     0.259 
week_categorysleep:MOODprev          -0.059     0.046    -1.275   214.854     0.204     0.423     0.304 
week_categoryNone:aggMOODprev        -0.006     0.048    -0.123  6923.431     0.902     0.055     0.053 
week_categoryactivity:aggMOODprev    -0.021     0.029    -0.710   579.557     0.478     0.221     0.184 
week_categorymood:aggMOODprev        -0.049     0.023    -2.122    81.323     0.037     0.936     0.496 
week_categorysleep:aggMOODprev        0.002     0.030     0.058   293.764     0.954     0.341     0.259 

Unadjusted hypothesis test as appropriate in larger samples. 

> 
  > saveRDS(mood_results, file = "direct_mood_results.RDS")
> saveRDS(mood_results2, file = "direct_mood_results2.RDS")
> saveRDS(mood_results3, file = "direct_mood_results3.RDS")
> 
  > testEstimates(mood_agg_results)

Call:
  
  testEstimates(model = mood_agg_results)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                    1.018     0.165     6.189   383.339     0.000     0.286     0.227 
week_category_new              0.098     0.086     1.137    84.151     0.259     0.905     0.487 
STEP_COUNTprev                -0.001     0.004    -0.217    55.727     0.829     1.403     0.598 
SLEEP_COUNTprev               -0.006     0.005    -1.254   110.998     0.212     0.706     0.424 
MOODprev                       0.507     0.016    32.016   209.421     0.000     0.431     0.308 
Sex                            0.015     0.018     0.825   647.243     0.410     0.207     0.174 
PHQtot0                        0.002     0.004     0.414   188.818     0.680     0.465     0.324 
Neu0                          -0.006     0.001    -4.615   464.707     0.000     0.253     0.206 
depr0                          0.082     0.020     4.074   161.761     0.000     0.521     0.351 
pre_intern_mood                0.309     0.015    20.874   402.652     0.000     0.278     0.221 
pre_intern_sleep               0.000     0.000     2.475   133.329     0.015     0.606     0.387 
pre_intern_sqrt_step           0.001     0.001     0.924    45.803     0.360     1.810     0.659 
week_category_new:MOODprev    -0.013     0.011    -1.134    89.643     0.260     0.853     0.472 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(mood_agg_results2)

Call:
  
  testEstimates(model = mood_agg_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                    0.516     0.465     1.109   177.799     0.269     0.486     0.334 
week_category_new              0.391     0.291     1.345   562.787     0.179     0.225     0.187 
STEP_COUNTprev                 0.003     0.009     0.363    79.679     0.718     0.954     0.501 
SLEEP_COUNTprev               -0.012     0.017    -0.711    68.268     0.480     1.117     0.541 
MOODprev                       0.545     0.042    13.076   378.155     0.000     0.289     0.228 
Sex                            0.114     0.057     1.992  2118.266     0.047     0.105     0.096 
PHQtot0                       -0.008     0.008    -1.016   391.676     0.310     0.282     0.224 
Neu0                          -0.009     0.004    -2.444   636.092     0.015     0.209     0.175 
depr0                          0.032     0.060     0.530   557.384     0.597     0.226     0.188 
pre_intern_mood                0.342     0.038     8.990   727.125     0.000     0.193     0.164 
pre_intern_sleep               0.000     0.000     0.594   124.671     0.554     0.640     0.400 
pre_intern_sqrt_step           0.002     0.001     1.229   311.008     0.220     0.328     0.252 
week_category_new:MOODprev    -0.053     0.037    -1.417   504.257     0.157     0.241     0.197 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(mood_agg_results3)

Call:
  
  testEstimates(model = mood_agg_results3)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                       0.609     0.449     1.355   137.031     0.177     0.593     0.381 
week_category_new                 0.585     0.292     2.002   354.745     0.046     0.301     0.236 
STEP_COUNTprev                    0.003     0.009     0.313    71.376     0.755     1.066     0.529 
SLEEP_COUNTprev                  -0.012     0.017    -0.697    64.216     0.488     1.193     0.558 
MOODprev                          0.548     0.042    13.089   393.548     0.000     0.282     0.224 
Sex                               0.113     0.062     1.823  2603.557     0.068     0.093     0.086 
PHQtot0                          -0.006     0.007    -0.869   268.276     0.386     0.363     0.272 
Neu0                             -0.010     0.003    -2.911   452.711     0.004     0.258     0.208 
depr0                             0.021     0.057     0.366   512.321     0.714     0.239     0.196 
pre_intern_mood                   0.333     0.033    10.247   368.989     0.000     0.294     0.231 
pre_intern_sleep                  0.000     0.000     0.494   107.219     0.622     0.727     0.431 
pre_intern_sqrt_step              0.002     0.001     1.423   271.324     0.156     0.360     0.270 
week_category_new:MOODprev       -0.061     0.036    -1.702   377.523     0.090     0.289     0.228 
week_category_new:aggMOODprev    -0.022     0.021    -1.038  4431.133     0.299     0.070     0.066 

Unadjusted hypothesis test as appropriate in larger samples. 

> 
  > saveRDS(mood_agg_results, file = "direct_mood_agg_results.RDS")
> saveRDS(mood_agg_results2, file = "direct_mood_agg_results2.RDS")
> saveRDS(mood_agg_results3, file = "direct_mood_agg_results3.RDS")
> 
  > testEstimates(step_results)

Call:
  
  testEstimates(model = step_results)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                          9.650     0.654    14.755    52.541     0.000     1.508     0.616 
week_category_new                   -0.067     0.241    -0.280   214.724     0.780     0.423     0.304 
STEP_COUNTprev                       0.346     0.017    20.032   220.411     0.000     0.416     0.300 
SLEEP_COUNTprev                      0.010     0.017     0.579    52.171     0.565     1.522     0.618 
MOODprev                             0.027     0.029     0.935    53.459     0.354     1.476     0.610 
Sex                                 -0.060     0.050    -1.217   493.489     0.224     0.244     0.199 
PHQtot0                              0.008     0.013     0.590    72.286     0.557     1.052     0.526 
Neu0                                -0.005     0.003    -1.532   420.125     0.126     0.270     0.216 
depr0                                0.006     0.060     0.104    98.700     0.917     0.782     0.450 
pre_intern_mood                     -0.037     0.040    -0.931    56.224     0.356     1.388     0.595 
pre_intern_sleep                    -0.001     0.001    -1.051    32.151     0.301     3.324     0.782 
pre_intern_sqrt_step                 0.036     0.002    17.072    60.748     0.000     1.269     0.573 
week_category_new:STEP_COUNTprev    -0.000     0.012    -0.008   257.154     0.994     0.373     0.277 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(step_results2)

Call:
  
  testEstimates(model = step_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                          9.661     1.365     7.076    75.575     0.000     1.006     0.514 
week_category_new                   -0.086     0.851    -0.101    75.029     0.920     1.013     0.516 
STEP_COUNTprev                       0.320     0.039     8.213   105.780     0.000     0.736     0.434 
SLEEP_COUNTprev                      0.014     0.048     0.291    71.757     0.772     1.060     0.528 
MOODprev                             0.084     0.082     1.023    63.444     0.310     1.209     0.561 
Sex                                  0.020     0.149     0.134    59.934     0.894     1.289     0.577 
PHQtot0                              0.007     0.021     0.316    63.911     0.753     1.199     0.559 
Neu0                                -0.012     0.009    -1.318    63.045     0.192     1.217     0.563 
depr0                               -0.061     0.144    -0.424    79.896     0.673     0.952     0.500 
pre_intern_mood                      0.005     0.093     0.058   109.814     0.954     0.712     0.426 
pre_intern_sleep                    -0.001     0.001    -1.080    76.767     0.284     0.990     0.510 
pre_intern_sqrt_step                 0.035     0.003    11.197   113.782     0.000     0.691     0.419 
week_category_new:STEP_COUNTprev    -0.002     0.042    -0.043    83.431     0.966     0.913     0.489 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(step_results3)

Call:
  
  testEstimates(model = step_results3)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                             9.876     1.301     7.589    69.891     0.000     1.089     0.535 
week_category_new                       0.107     0.951     0.113    65.704     0.911     1.163     0.551 
STEP_COUNTprev                          0.319     0.039     8.190   108.143     0.000     0.722     0.430 
SLEEP_COUNTprev                         0.014     0.048     0.288    71.254     0.774     1.068     0.529 
MOODprev                                0.079     0.082     0.955    56.079     0.344     1.393     0.596 
Sex                                     0.010     0.148     0.067    47.613     0.947     1.715     0.646 
PHQtot0                                 0.008     0.022     0.373    67.726     0.710     1.126     0.543 
Neu0                                   -0.014     0.009    -1.598    62.371     0.115     1.232     0.566 
depr0                                  -0.080     0.139    -0.579    80.001     0.564     0.951     0.500 
pre_intern_mood                        -0.006     0.086    -0.065    90.415     0.949     0.846     0.470 
pre_intern_sleep                       -0.001     0.001    -1.114    72.467     0.269     1.049     0.525 
pre_intern_sqrt_step                    0.036     0.003    11.925   105.369     0.000     0.738     0.435 
week_category_new:STEP_COUNTprev       -0.001     0.041    -0.033    84.877     0.974     0.898     0.485 
week_category_new:aggSTEP_COUNTprev    -0.011     0.018    -0.629    83.208     0.531     0.915     0.490 

Unadjusted hypothesis test as appropriate in larger samples. 

> 
  > saveRDS(step_results, file = "direct_step_results.RDS")
> saveRDS(step_results2, file = "direct_step_results2.RDS")
> saveRDS(step_results3, file = "direct_step_results3.RDS")
> 
  > testEstimates(sleep_results)

Call:
  
  testEstimates(model = sleep_results)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                          13.628     0.445    30.613    46.036     0.000     1.797     0.657 
week_category_new                     0.079     0.297     0.265    89.691     0.792     0.853     0.472 
STEP_COUNTprev                        0.005     0.005     0.996    68.829     0.323     1.107     0.539 
SLEEP_COUNTprev                       0.179     0.018    10.201    54.418     0.000     1.444     0.605 
MOODprev                             -0.011     0.014    -0.767    87.342     0.445     0.874     0.478 
Sex                                   0.225     0.036     6.260    60.950     0.000     1.264     0.572 
PHQtot0                              -0.007     0.006    -1.049    77.656     0.298     0.979     0.507 
Neu0                                 -0.001     0.002    -0.578   205.336     0.564     0.437     0.311 
depr0                                -0.046     0.033    -1.391    84.659     0.168     0.900     0.486 
pre_intern_mood                      -0.020     0.025    -0.815    43.884     0.420     1.924     0.673 
pre_intern_sleep                      0.006     0.001    11.531    30.353     0.000     3.789     0.804 
pre_intern_sqrt_step                  0.001     0.001     0.664    29.944     0.512     3.915     0.809 
week_category_new:SLEEP_COUNTprev    -0.004     0.015    -0.298    91.562     0.767     0.837     0.467 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(sleep_results2)

Call:
  
  testEstimates(model = sleep_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                          14.595     0.854    17.081    82.504     0.000     0.923     0.492 
week_category_new                    -0.194     0.810    -0.240   112.767     0.811     0.696     0.421 
STEP_COUNTprev                        0.008     0.012     0.659    76.366     0.512     0.995     0.511 
SLEEP_COUNTprev                       0.133     0.039     3.432    92.064     0.001     0.832     0.466 
MOODprev                             -0.027     0.036    -0.747   101.928     0.457     0.760     0.443 
Sex                                   0.156     0.075     2.084    54.844     0.042     1.431     0.603 
PHQtot0                              -0.005     0.010    -0.504   103.856     0.615     0.747     0.438 
Neu0                                 -0.001     0.004    -0.211    68.386     0.834     1.115     0.540 
depr0                                -0.081     0.073    -1.107    55.306     0.273     1.416     0.600 
pre_intern_mood                      -0.017     0.036    -0.471   117.731     0.639     0.671     0.412 
pre_intern_sleep                      0.006     0.001    10.738    40.923     0.000     2.139     0.696 
pre_intern_sqrt_step                  0.001     0.002     0.411    45.463     0.683     1.829     0.661 
week_category_new:SLEEP_COUNTprev     0.010     0.041     0.242   113.575     0.810     0.692     0.419 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(sleep_results3)

Call:
  
  testEstimates(model = sleep_results3)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                             14.573     0.855    17.054    77.810     0.000     0.977     0.507 
week_category_new                       -0.211     0.802    -0.263   130.980     0.793     0.615     0.390 
STEP_COUNTprev                           0.008     0.012     0.653    76.449     0.516     0.994     0.511 
SLEEP_COUNTprev                          0.132     0.038     3.448    93.121     0.001     0.824     0.463 
MOODprev                                -0.026     0.036    -0.712    98.216     0.478     0.785     0.451 
Sex                                      0.157     0.076     2.070    49.325     0.044     1.636     0.635 
PHQtot0                                 -0.005     0.010    -0.513   101.889     0.609     0.760     0.443 
Neu0                                    -0.001     0.004    -0.179    68.450     0.859     1.114     0.540 
depr0                                   -0.079     0.071    -1.108    52.675     0.273     1.504     0.615 
pre_intern_mood                         -0.016     0.036    -0.442   122.290     0.659     0.651     0.404 
pre_intern_sleep                         0.006     0.001    10.941    41.701     0.000     2.077     0.690 
pre_intern_sqrt_step                     0.001     0.002     0.417    43.324     0.679     1.961     0.677 
week_category_new:SLEEP_COUNTprev        0.009     0.040     0.233   112.627     0.816     0.697     0.421 
week_category_new:aggSLEEP_COUNTprev     0.001     0.007     0.193    63.529     0.848     1.207     0.560 

Unadjusted hypothesis test as appropriate in larger samples. 

> 
  > saveRDS(sleep_results, file = "direct_sleep_results.RDS")
> saveRDS(sleep_results2, file = "direct_sleep_results2.RDS")
> saveRDS(sleep_results3, file = "direct_sleep_results3.RDS")
> 
  > specialties = unique(temp$Specialty)
> result = vector(length = length(specialties))
> for (i in 1:length(specialties)) {
  +   specialty = specialties[i]
  +   result[i] = length(unique(temp$UserID[temp$Specialty == specialty]))
  + }
> result
[1] 333 199  38 170 121  77  67 110   7   9 134 135  22   3  55   6  23  22  11   1   2   1  15   1