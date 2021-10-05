> testEstimates(mood_results)

Call:
  
  testEstimates(model = mood_results)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                        1.011     0.171     5.917   406.791     0.000     0.276     0.220 
week_categoryactivity              0.150     0.116     1.299    97.799     0.197     0.788     0.452 
week_categorymood                  0.157     0.115     1.363   110.692     0.176     0.707     0.425 
week_categorysleep                 0.231     0.124     1.868    87.291     0.065     0.875     0.478 
STEP_COUNTprev                    -0.001     0.004    -0.177    63.036     0.860     1.217     0.563 
SLEEP_COUNTprev                   -0.005     0.005    -0.978   143.605     0.330     0.572     0.372 
MOODprev                           0.509     0.016    31.899   211.336     0.000     0.428     0.306 
Sex                                0.015     0.020     0.770   316.379     0.442     0.325     0.250 
PHQtot0                            0.002     0.004     0.635   283.553     0.526     0.349     0.264 
Neu0                              -0.006     0.001    -4.474   443.633     0.000     0.261     0.211 
depr0                              0.082     0.020     4.142   235.616     0.000     0.397     0.290 
pre_intern_mood                    0.306     0.015    19.795   303.784     0.000     0.333     0.255 
pre_intern_sleep                   0.000     0.000     2.302   156.967     0.023     0.534     0.356 
pre_intern_sqrt_step               0.001     0.001     0.800    50.525     0.427     1.586     0.628 
week_categoryactivity:MOODprev    -0.019     0.015    -1.211   102.671     0.229     0.755     0.441 
week_categorymood:MOODprev        -0.023     0.015    -1.501   115.700     0.136     0.681     0.415 
week_categorysleep:MOODprev       -0.033     0.017    -1.999    89.005     0.049     0.859     0.474 

Unadjusted hypothesis test as appropriate in larger samples. 

There were 20 warnings (use warnings() to see them)
> testEstimates(mood_results2)

Call:
  
  testEstimates(model = mood_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                        0.508     0.496     1.024   174.054     0.307     0.493     0.338 
week_categoryactivity              0.482     0.448     1.077   603.759     0.282     0.216     0.180 
week_categorymood                  0.376     0.318     1.183   510.882     0.237     0.239     0.196 
week_categorysleep                 0.395     0.325     1.213   132.910     0.227     0.608     0.387 
STEP_COUNTprev                     0.004     0.009     0.459    92.513     0.647     0.829     0.465 
SLEEP_COUNTprev                   -0.010     0.018    -0.560    72.280     0.577     1.052     0.526 
MOODprev                           0.552     0.045    12.222   421.344     0.000     0.270     0.216 
Sex                                0.100     0.056     1.795  1439.564     0.073     0.130     0.116 
PHQtot0                           -0.007     0.008    -0.885   271.735     0.377     0.359     0.270 
Neu0                              -0.008     0.003    -2.349   443.653     0.019     0.261     0.210 
depr0                              0.040     0.061     0.664   470.305     0.507     0.252     0.204 
pre_intern_mood                    0.331     0.039     8.519   443.899     0.000     0.261     0.210 
pre_intern_sleep                   0.000     0.000     0.667   178.798     0.505     0.484     0.333 
pre_intern_sqrt_step               0.001     0.001     1.011   178.599     0.313     0.484     0.334 
week_categoryactivity:MOODprev    -0.060     0.056    -1.072   497.518     0.284     0.243     0.199 
week_categorymood:MOODprev        -0.046     0.041    -1.143   442.674     0.253     0.261     0.211 
week_categorysleep:MOODprev       -0.063     0.043    -1.450   162.864     0.149     0.519     0.349 

Unadjusted hypothesis test as appropriate in larger samples. 

There were 20 warnings (use warnings() to see them)
> testEstimates(mood_results3)

Call:
  
  testEstimates(model = mood_results3)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                           0.705     0.719     0.980   702.507     0.327     0.197     0.167 
week_categoryactivity                 0.918     0.718     1.279   639.011     0.201     0.208     0.175 
week_categorymood                     0.874     0.568     1.539   342.404     0.125     0.308     0.240 
week_categorysleep                    0.188     0.579     0.325   267.923     0.746     0.363     0.272 
STEP_COUNTprev                        0.004     0.009     0.382    85.839     0.703     0.888     0.482 
SLEEP_COUNTprev                      -0.011     0.018    -0.579    76.230     0.564     0.997     0.512 
MOODprev                              0.554     0.053    10.449  1044.482     0.000     0.156     0.137 
Sex                                   0.087     0.058     1.491  1453.044     0.136     0.129     0.116 
PHQtot0                              -0.004     0.008    -0.573   189.252     0.568     0.464     0.324 
Neu0                                 -0.009     0.003    -3.178   250.905     0.002     0.380     0.281 
depr0                                 0.027     0.054     0.498   313.899     0.619     0.326     0.251 
pre_intern_mood                       0.321     0.031    10.231   285.886     0.000     0.347     0.263 
pre_intern_sleep                      0.000     0.000     0.559   164.482     0.577     0.515     0.348 
pre_intern_sqrt_step                  0.001     0.001     1.236   122.486     0.219     0.650     0.404 
week_categoryactivity:MOODprev       -0.077     0.060    -1.286   434.236     0.199     0.265     0.213 
week_categorymood:MOODprev           -0.071     0.046    -1.558   352.397     0.120     0.302     0.237 
week_categorysleep:MOODprev          -0.054     0.051    -1.076   209.324     0.283     0.431     0.308 
week_categoryNone:aggMOODprev        -0.006     0.050    -0.126  7311.160     0.899     0.054     0.051 
week_categoryactivity:aggMOODprev    -0.057     0.022    -2.600    76.985     0.011     0.987     0.509 
week_categorymood:aggMOODprev        -0.056     0.026    -2.190    95.354     0.031     0.806     0.458 
week_categorysleep:aggMOODprev        0.017     0.025     0.693   202.025     0.489     0.442     0.313 

Unadjusted hypothesis test as appropriate in larger samples. 

There were 20 warnings (use warnings() to see them)
> 
  > saveRDS(mood_results, file = "direct_mood_results0.RDS")
> saveRDS(mood_results2, file = "direct_mood_results02.RDS")
> saveRDS(mood_results3, file = "direct_mood_results03.RDS")
> 
  > testEstimates(mood_agg_results)

Call:
  
  testEstimates(model = mood_agg_results)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                    1.012     0.171     5.927   407.557     0.000     0.275     0.220 
week_category_new              0.180     0.097     1.856    78.713     0.067     0.966     0.504 
STEP_COUNTprev                -0.001     0.004    -0.178    62.955     0.859     1.219     0.563 
SLEEP_COUNTprev               -0.005     0.005    -0.985   143.160     0.326     0.573     0.373 
MOODprev                       0.509     0.016    31.901   211.306     0.000     0.428     0.306 
Sex                            0.015     0.020     0.768   317.702     0.443     0.324     0.249 
PHQtot0                        0.002     0.004     0.636   283.007     0.525     0.350     0.264 
Neu0                          -0.006     0.001    -4.476   438.347     0.000     0.263     0.212 
depr0                          0.082     0.020     4.131   236.789     0.000     0.395     0.289 
pre_intern_mood                0.306     0.015    19.808   305.998     0.000     0.332     0.254 
pre_intern_sleep               0.000     0.000     2.305   156.364     0.023     0.535     0.357 
pre_intern_sqrt_step           0.001     0.001     0.796    50.389     0.430     1.591     0.629 
week_category_new:MOODprev    -0.025     0.013    -1.944    82.842     0.055     0.919     0.491 

Unadjusted hypothesis test as appropriate in larger samples. 

There were 20 warnings (use warnings() to see them)
> testEstimates(mood_agg_results2)

Call:
  
  testEstimates(model = mood_agg_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                    0.508     0.496     1.023   175.019     0.308     0.491     0.337 
week_category_new              0.419     0.300     1.398   470.148     0.163     0.252     0.204 
STEP_COUNTprev                 0.004     0.010     0.463    91.839     0.645     0.834     0.466 
SLEEP_COUNTprev               -0.010     0.018    -0.562    72.108     0.576     1.055     0.526 
MOODprev                       0.551     0.045    12.255   411.184     0.000     0.274     0.219 
Sex                            0.101     0.057     1.774  1456.061     0.076     0.129     0.115 
PHQtot0                       -0.008     0.008    -0.896   278.319     0.371     0.354     0.267 
Neu0                          -0.008     0.004    -2.320   454.453     0.021     0.257     0.208 
depr0                          0.040     0.062     0.648   477.988     0.517     0.249     0.203 
pre_intern_mood                0.332     0.039     8.499   440.041     0.000     0.262     0.211 
pre_intern_sleep               0.000     0.000     0.665   175.280     0.507     0.491     0.337 
pre_intern_sqrt_step           0.001     0.001     1.011   186.022     0.313     0.470     0.327 
week_category_new:MOODprev    -0.057     0.038    -1.502   465.833     0.134     0.253     0.205 

Unadjusted hypothesis test as appropriate in larger samples. 

There were 20 warnings (use warnings() to see them)
> testEstimates(mood_agg_results3)

Call:
  
  testEstimates(model = mood_agg_results3)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                       0.608     0.476     1.276   136.294     0.204     0.596     0.382 
week_category_new                 0.665     0.321     2.072   174.466     0.040     0.493     0.338 
STEP_COUNTprev                    0.004     0.010     0.389    84.467     0.698     0.902     0.486 
SLEEP_COUNTprev                  -0.010     0.018    -0.524    70.036     0.602     1.087     0.534 
MOODprev                          0.555     0.045    12.210   466.049     0.000     0.253     0.205 
Sex                               0.099     0.061     1.614  1710.436     0.107     0.118     0.106 
PHQtot0                          -0.006     0.008    -0.726   187.670     0.468     0.467     0.325 
Neu0                             -0.010     0.004    -2.712   380.141     0.007     0.288     0.228 
depr0                             0.027     0.059     0.467   419.626     0.641     0.270     0.217 
pre_intern_mood                   0.321     0.034     9.413   324.102     0.000     0.319     0.247 
pre_intern_sleep                  0.000     0.000     0.549   166.365     0.584     0.510     0.346 
pre_intern_sqrt_step              0.002     0.001     1.205   183.574     0.230     0.474     0.329 
week_category_new:MOODprev       -0.067     0.037    -1.804   321.760     0.072     0.321     0.248 
week_category_new:aggMOODprev    -0.027     0.019    -1.393   343.899     0.164     0.307     0.239 

Unadjusted hypothesis test as appropriate in larger samples. 

There were 20 warnings (use warnings() to see them)
> 
  > saveRDS(mood_agg_results, file = "direct_mood_agg_results0.RDS")
> saveRDS(mood_agg_results2, file = "direct_mood_agg_results02.RDS")
> saveRDS(mood_agg_results3, file = "direct_mood_agg_results03.RDS")
> 
  > testEstimates(step_results)

Call:
  
  testEstimates(model = step_results)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                          9.588     0.664    14.446    57.658     0.000     1.348     0.588 
week_category_new                    0.030     0.268     0.113   189.363     0.910     0.464     0.324 
STEP_COUNTprev                       0.349     0.017    20.017   237.074     0.000     0.395     0.289 
SLEEP_COUNTprev                      0.013     0.019     0.723    54.208     0.473     1.451     0.606 
MOODprev                             0.029     0.032     0.912    50.783     0.366     1.575     0.626 
Sex                                 -0.063     0.055    -1.154   237.761     0.250     0.394     0.289 
PHQtot0                              0.006     0.013     0.487    77.804     0.628     0.977     0.507 
Neu0                                -0.005     0.004    -1.463   301.775     0.145     0.335     0.256 
depr0                               -0.004     0.061    -0.062   122.777     0.950     0.648     0.403 
pre_intern_mood                     -0.037     0.042    -0.889    57.392     0.378     1.355     0.589 
pre_intern_sleep                    -0.001     0.001    -0.884    34.734     0.383     2.840     0.753 
pre_intern_sqrt_step                 0.035     0.002    16.070    69.975     0.000     1.088     0.534 
week_category_new:STEP_COUNTprev    -0.005     0.013    -0.400   215.396     0.689     0.422     0.303 

Unadjusted hypothesis test as appropriate in larger samples. 

There were 20 warnings (use warnings() to see them)
> testEstimates(step_results2)

Call:
  
  testEstimates(model = step_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                          9.708     1.415     6.859    80.982     0.000     0.939     0.497 
week_category_new                    0.105     0.923     0.113    85.748     0.910     0.889     0.483 
STEP_COUNTprev                       0.321     0.040     8.062   102.152     0.000     0.758     0.442 
SLEEP_COUNTprev                      0.010     0.051     0.202    88.803     0.840     0.861     0.474 
MOODprev                             0.094     0.095     0.995    65.777     0.323     1.162     0.551 
Sex                                  0.011     0.168     0.064    49.277     0.949     1.638     0.635 
PHQtot0                              0.005     0.022     0.224    68.884     0.823     1.106     0.538 
Neu0                                -0.013     0.010    -1.240    58.589     0.220     1.323     0.583 
depr0                               -0.042     0.157    -0.267    68.153     0.790     1.119     0.541 
pre_intern_mood                     -0.009     0.108    -0.085    68.716     0.932     1.109     0.539 
pre_intern_sleep                    -0.001     0.001    -0.721    72.316     0.473     1.052     0.526 
pre_intern_sqrt_step                 0.035     0.004     9.847   110.833     0.000     0.707     0.424 
week_category_new:STEP_COUNTprev    -0.011     0.045    -0.252    94.744     0.802     0.811     0.459 

Unadjusted hypothesis test as appropriate in larger samples. 

There were 20 warnings (use warnings() to see them)
> testEstimates(step_results3)

Call:
  
  testEstimates(model = step_results3)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                             9.828     1.308     7.511    85.004     0.000     0.897     0.485 
week_category_new                       0.234     1.039     0.225    69.759     0.823     1.092     0.535 
STEP_COUNTprev                          0.321     0.039     8.140   103.351     0.000     0.751     0.440 
SLEEP_COUNTprev                         0.011     0.051     0.216    87.787     0.829     0.870     0.477 
MOODprev                                0.091     0.097     0.932    56.557     0.355     1.379     0.594 
Sex                                     0.005     0.172     0.026    40.755     0.979     2.152     0.697 
PHQtot0                                 0.006     0.023     0.256    71.460     0.799     1.065     0.529 
Neu0                                   -0.014     0.009    -1.493    64.305     0.140     1.191     0.557 
depr0                                  -0.054     0.149    -0.361    70.789     0.719     1.075     0.531 
pre_intern_mood                        -0.016     0.099    -0.159    65.445     0.874     1.168     0.552 
pre_intern_sleep                       -0.001     0.001    -0.746    69.649     0.458     1.093     0.535 
pre_intern_sqrt_step                    0.035     0.003    10.559   102.323     0.000     0.757     0.442 
week_category_new:STEP_COUNTprev       -0.011     0.045    -0.254    95.959     0.800     0.802     0.456 
week_category_new:aggSTEP_COUNTprev    -0.007     0.021    -0.354    61.770     0.725     1.245     0.568 

Unadjusted hypothesis test as appropriate in larger samples. 

There were 20 warnings (use warnings() to see them)
> 
  > saveRDS(step_results, file = "direct_step_results0.RDS")
> saveRDS(step_results2, file = "direct_step_results02.RDS")
> saveRDS(step_results3, file = "direct_step_results03.RDS")
> 
  > testEstimates(sleep_results)

Call:
  
  testEstimates(model = sleep_results)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                          13.617     0.446    30.537    47.695     0.000     1.711     0.646 
week_category_new                     0.269     0.340     0.792    68.593     0.431     1.111     0.540 
STEP_COUNTprev                        0.005     0.005     0.970   133.011     0.334     0.608     0.387 
SLEEP_COUNTprev                       0.183     0.017    10.677    59.281     0.000     1.305     0.580 
MOODprev                             -0.012     0.015    -0.801    89.370     0.425     0.856     0.473 
Sex                                   0.229     0.036     6.304    68.262     0.000     1.117     0.541 
PHQtot0                              -0.007     0.007    -1.010    77.678     0.315     0.979     0.507 
Neu0                                 -0.001     0.002    -0.678   238.392     0.498     0.393     0.288 
depr0                                -0.040     0.037    -1.091    63.164     0.279     1.215     0.562 
pre_intern_mood                      -0.017     0.025    -0.668    46.608     0.507     1.766     0.653 
pre_intern_sleep                      0.006     0.001    11.356    32.891     0.000     3.167     0.773 
pre_intern_sqrt_step                  0.001     0.001     0.694    33.330     0.492     3.082     0.769 
week_category_new:SLEEP_COUNTprev    -0.014     0.017    -0.832    68.569     0.409     1.111     0.540 

Unadjusted hypothesis test as appropriate in larger samples. 

There were 20 warnings (use warnings() to see them)
> testEstimates(sleep_results2)

Call:
  
  testEstimates(model = sleep_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                          14.568     0.874    16.668    82.619     0.000     0.921     0.492 
week_category_new                    -0.063     0.865    -0.073    97.238     0.942     0.792     0.453 
STEP_COUNTprev                        0.008     0.015     0.526    64.372     0.601     1.190     0.557 
SLEEP_COUNTprev                       0.139     0.039     3.524    89.473     0.001     0.855     0.472 
MOODprev                             -0.025     0.042    -0.599    78.001     0.551     0.975     0.506 
Sex                                   0.165     0.087     1.887    49.590     0.065     1.625     0.633 
PHQtot0                              -0.004     0.011    -0.363   176.243     0.717     0.489     0.336 
Neu0                                 -0.001     0.005    -0.258    65.220     0.797     1.173     0.553 
depr0                                -0.086     0.084    -1.015    50.423     0.315     1.590     0.628 
pre_intern_mood                      -0.010     0.042    -0.241    88.028     0.810     0.868     0.476 
pre_intern_sleep                      0.006     0.001     9.206    43.079     0.000     1.977     0.679 
pre_intern_sqrt_step                  0.001     0.002     0.243    49.351     0.809     1.635     0.635 
week_category_new:SLEEP_COUNTprev     0.003     0.043     0.061    98.662     0.952     0.782     0.450 

Unadjusted hypothesis test as appropriate in larger samples. 

There were 20 warnings (use warnings() to see them)
> testEstimates(sleep_results3)

Call:
  
  testEstimates(model = sleep_results3)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                             14.553     0.860    16.930    79.748     0.000     0.954     0.500 
week_category_new                       -0.073     0.887    -0.082    92.276     0.935     0.831     0.465 
STEP_COUNTprev                           0.008     0.015     0.517    64.659     0.607     1.184     0.556 
SLEEP_COUNTprev                          0.139     0.039     3.530    89.010     0.001     0.859     0.474 
MOODprev                                -0.024     0.043    -0.568    69.143     0.572     1.102     0.537 
Sex                                      0.166     0.089     1.861    44.833     0.069     1.865     0.666 
PHQtot0                                 -0.004     0.011    -0.373   175.901     0.710     0.490     0.336 
Neu0                                    -0.001     0.005    -0.250    68.501     0.803     1.113     0.540 
depr0                                   -0.084     0.080    -1.045    48.164     0.301     1.689     0.643 
pre_intern_mood                         -0.009     0.041    -0.229    99.779     0.820     0.774     0.447 
pre_intern_sleep                         0.006     0.001     9.371    43.498     0.000     1.949     0.675 
pre_intern_sqrt_step                     0.001     0.002     0.252    47.759     0.802     1.708     0.645 
week_category_new:SLEEP_COUNTprev        0.002     0.043     0.049    97.988     0.961     0.787     0.451 
week_category_new:aggSLEEP_COUNTprev     0.001     0.008     0.135    64.113     0.893     1.195     0.558 

Unadjusted hypothesis test as appropriate in larger samples. 

There were 20 warnings (use warnings() to see them)
> 
  > saveRDS(sleep_results, file = "direct_sleep_results0.RDS")
> saveRDS(sleep_results2, file = "direct_sleep_results02.RDS")
> saveRDS(sleep_results3, file = "direct_sleep_results03.RDS")
> 
  > specialties = unique(temp$Specialty)
> result = vector(length = length(specialties))
> for (i in 1:length(specialties)) {
  +   specialty = specialties[i]
  +   result[i] = length(unique(temp$UserID[temp$Specialty == specialty]))
  + }
> result
[1] 333 199  38 170 121  77  67 110   7   9 134 135  22   3  55   6  23  22  11   1   2   1  15   1