> testEstimates(mood_results)

Call:
  
  testEstimates(model = mood_results)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                        0.483     0.134     3.615   144.974     0.000     0.567     0.371 
week_categoryactivity              0.325     0.110     2.949    55.362     0.005     1.414     0.600 
week_categorymood                  0.368     0.107     3.442    66.192     0.001     1.154     0.549 
week_categorysleep                 0.411     0.116     3.556    49.990     0.001     1.608     0.631 
STEP_COUNTprev                    -0.001     0.003    -0.338    43.576     0.737     1.944     0.675 
SLEEP_COUNTprev                   -0.004     0.005    -0.832    44.948     0.410     1.858     0.665 
MOODprev                           0.696     0.013    53.228   110.968     0.000     0.706     0.424 
Sex                                0.008     0.013     0.633   545.865     0.527     0.229     0.190 
PHQtot0                            0.001     0.003     0.252   186.839     0.801     0.468     0.326 
Neu0                              -0.004     0.001    -4.418   596.410     0.000     0.217     0.181 
depr0                              0.056     0.014     4.078   151.778     0.000     0.548     0.362 
pre_intern_mood                    0.214     0.010    20.522   315.776     0.000     0.325     0.250 
pre_intern_sleep                   0.000     0.000     2.137    89.111     0.035     0.858     0.473 
pre_intern_sqrt_step               0.000     0.000     0.895    44.076     0.375     1.912     0.671 
week_categoryactivity:MOODprev    -0.051     0.014    -3.540    58.704     0.001     1.320     0.583 
week_categorymood:MOODprev        -0.055     0.014    -3.821    64.122     0.000     1.195     0.558 
week_categorysleep:MOODprev       -0.058     0.015    -3.969    57.757     0.000     1.345     0.588 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(mood_results2)

Call:
  
  testEstimates(model = mood_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                        0.177     0.413     0.429    63.817     0.669     1.201     0.559 
week_categoryactivity              0.228     0.269     0.849    68.617     0.399     1.111     0.539 
week_categorymood                  0.531     0.309     1.722   143.885     0.087     0.571     0.372 
week_categorysleep                 0.506     0.241     2.098   132.950     0.038     0.608     0.387 
STEP_COUNTprev                     0.003     0.006     0.518    67.234     0.606     1.135     0.545 
SLEEP_COUNTprev                   -0.005     0.015    -0.345    49.665     0.732     1.621     0.633 
MOODprev                           0.707     0.031    22.850    68.850     0.000     1.107     0.539 
Sex                                0.073     0.037     1.960  1507.292     0.050     0.126     0.113 
PHQtot0                           -0.005     0.005    -1.019   345.922     0.309     0.306     0.239 
Neu0                              -0.006     0.002    -2.397   433.901     0.017     0.265     0.213 
depr0                              0.025     0.040     0.628   456.158     0.530     0.256     0.208 
pre_intern_mood                    0.236     0.026     9.134  1290.699     0.000     0.138     0.123 
pre_intern_sleep                   0.000     0.000     0.455    85.407     0.650     0.893     0.484 
pre_intern_sqrt_step               0.001     0.001     1.076   323.142     0.283     0.320     0.247 
week_categoryactivity:MOODprev    -0.035     0.035    -0.979    72.354     0.331     1.051     0.525 
week_categorymood:MOODprev        -0.076     0.040    -1.911   128.634     0.058     0.624     0.394 
week_categorysleep:MOODprev       -0.072     0.033    -2.186   189.137     0.030     0.464     0.324 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(mood_results3)

Call:
  
  testEstimates(model = mood_results3)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                           0.239     0.480     0.498    80.094     0.620     0.950     0.499 
week_categoryactivity                 0.433     0.386     1.122    68.611     0.266     1.111     0.539 
week_categorymood                     0.723     0.459     1.577   117.773     0.118     0.671     0.412 
week_categorysleep                    0.531     0.362     1.466    48.979     0.149     1.651     0.637 
STEP_COUNTprev                        0.003     0.007     0.470    58.481     0.640     1.326     0.584 
SLEEP_COUNTprev                      -0.005     0.015    -0.330    49.351     0.743     1.635     0.635 
MOODprev                              0.709     0.033    21.192    82.358     0.000     0.924     0.492 
Sex                                   0.071     0.041     1.734  1840.488     0.083     0.113     0.103 
PHQtot0                              -0.004     0.005    -0.825   262.806     0.410     0.368     0.274 
Neu0                                 -0.006     0.002    -3.146   273.538     0.002     0.358     0.269 
depr0                                 0.017     0.037     0.466   430.296     0.641     0.266     0.214 
pre_intern_mood                       0.229     0.021    10.998   524.126     0.000     0.235     0.193 
pre_intern_sleep                      0.000     0.000     0.349    77.218     0.728     0.984     0.509 
pre_intern_sqrt_step                  0.001     0.001     1.374   215.919     0.171     0.422     0.303 
week_categoryactivity:MOODprev       -0.043     0.038    -1.153    70.517     0.253     1.079     0.532 
week_categorymood:MOODprev           -0.083     0.042    -1.968   120.113     0.051     0.660     0.408 
week_categorysleep:MOODprev          -0.072     0.033    -2.202    85.107     0.030     0.896     0.484 
week_categoryNone:aggMOODprev         0.000     0.026     0.004   280.311     0.997     0.352     0.266 
week_categoryactivity:aggMOODprev    -0.022     0.033    -0.685  1587.418     0.493     0.123     0.111 
week_categorymood:aggMOODprev        -0.022     0.015    -1.489    42.249     0.144     2.036     0.685 
week_categorysleep:aggMOODprev       -0.003     0.033    -0.087   368.413     0.931     0.294     0.231 

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
(Intercept)                    0.483     0.134     3.613   144.595     0.000     0.569     0.371 
week_category_new              0.369     0.086     4.267    62.029     0.000     1.239     0.567 
STEP_COUNTprev                -0.001     0.003    -0.329    43.454     0.744     1.952     0.676 
SLEEP_COUNTprev               -0.004     0.005    -0.834    44.687     0.409     1.874     0.667 
MOODprev                       0.696     0.013    53.223   111.052     0.000     0.705     0.424 
Sex                            0.008     0.013     0.636   539.817     0.525     0.231     0.191 
PHQtot0                        0.001     0.003     0.252   185.664     0.801     0.470     0.327 
Neu0                          -0.004     0.001    -4.415   591.457     0.000     0.218     0.182 
depr0                          0.056     0.014     4.081   152.064     0.000     0.547     0.362 
pre_intern_mood                0.214     0.010    20.521   314.256     0.000     0.326     0.251 
pre_intern_sleep               0.000     0.000     2.134    88.373     0.036     0.865     0.475 
pre_intern_sqrt_step           0.000     0.000     0.891    44.009     0.378     1.916     0.672 
week_category_new:MOODprev    -0.055     0.011    -4.822    65.361     0.000     1.170     0.553 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(mood_agg_results2)

Call:
  
  testEstimates(model = mood_agg_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                    0.170     0.414     0.411    65.147     0.682     1.174     0.554 
week_category_new              0.421     0.214     1.968    78.976     0.053     0.963     0.503 
STEP_COUNTprev                 0.003     0.006     0.523    68.182     0.602     1.118     0.541 
SLEEP_COUNTprev               -0.005     0.015    -0.320    49.196     0.750     1.642     0.636 
MOODprev                       0.707     0.031    22.861    68.889     0.000     1.106     0.538 
Sex                            0.072     0.037     1.966  1480.826     0.049     0.128     0.114 
PHQtot0                       -0.005     0.005    -1.011   348.970     0.313     0.304     0.238 
Neu0                          -0.006     0.002    -2.421   430.683     0.016     0.266     0.214 
depr0                          0.025     0.039     0.633   452.698     0.527     0.258     0.208 
pre_intern_mood                0.236     0.026     9.134  1314.987     0.000     0.137     0.122 
pre_intern_sleep               0.000     0.000     0.436    83.622     0.664     0.911     0.489 
pre_intern_sqrt_step           0.001     0.001     1.064   313.425     0.288     0.327     0.251 
week_category_new:MOODprev    -0.061     0.028    -2.181    79.691     0.032     0.954     0.501 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(mood_agg_results3)

Call:
  
  testEstimates(model = mood_agg_results3)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                       0.237     0.398     0.594    55.753     0.555     1.403     0.598 
week_category_new                 0.563     0.251     2.239    83.787     0.028     0.909     0.488 
STEP_COUNTprev                    0.003     0.007     0.464    61.953     0.644     1.241     0.568 
SLEEP_COUNTprev                  -0.005     0.015    -0.303    48.583     0.763     1.669     0.640 
MOODprev                          0.709     0.031    23.018    67.300     0.000     1.134     0.545 
Sex                               0.071     0.040     1.778  1919.664     0.076     0.110     0.100 
PHQtot0                          -0.004     0.005    -0.822   225.291     0.412     0.409     0.297 
Neu0                             -0.006     0.002    -2.950   260.240     0.003     0.370     0.276 
depr0                             0.017     0.037     0.453   404.032     0.651     0.277     0.221 
pre_intern_mood                   0.229     0.021    10.694   511.256     0.000     0.239     0.196 
pre_intern_sleep                  0.000     0.000     0.327    73.911     0.744     1.028     0.520 
pre_intern_sqrt_step              0.001     0.001     1.316   262.194     0.189     0.368     0.275 
week_category_new:MOODprev       -0.066     0.027    -2.478    61.263     0.016     1.257     0.571 
week_category_new:aggMOODprev    -0.016     0.017    -0.937  1977.951     0.349     0.109     0.099 

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
(Intercept)                          6.406     0.592    10.826    39.044     0.000     2.307     0.712 
week_category_new                    0.729     0.295     2.472   107.193     0.015     0.727     0.432 
STEP_COUNTprev                       0.538     0.015    35.101    75.957     0.000     1.001     0.513 
SLEEP_COUNTprev                      0.022     0.015     1.408    40.382     0.167     2.184     0.700 
MOODprev                             0.021     0.023     0.898    46.412     0.374     1.776     0.654 
Sex                                 -0.053     0.037    -1.435   509.483     0.152     0.239     0.196 
PHQtot0                              0.006     0.009     0.600    72.378     0.551     1.051     0.525 
Neu0                                -0.004     0.003    -1.664   431.883     0.097     0.265     0.213 
depr0                                0.003     0.045     0.065    96.998     0.948     0.794     0.454 
pre_intern_mood                     -0.026     0.031    -0.841    50.577     0.404     1.583     0.627 
pre_intern_sleep                    -0.001     0.000    -1.241    32.189     0.223     3.316     0.781 
pre_intern_sqrt_step                 0.027     0.002    16.755    59.239     0.000     1.306     0.580 
week_category_new:STEP_COUNTprev    -0.037     0.015    -2.484   100.184     0.015     0.771     0.446 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(step_results2)

Call:
  
  testEstimates(model = step_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                          6.336     1.174     5.396    76.057     0.000     0.999     0.512 
week_category_new                    0.997     0.734     1.357   169.354     0.176     0.504     0.343 
STEP_COUNTprev                       0.529     0.035    14.979   106.125     0.000     0.733     0.434 
SLEEP_COUNTprev                      0.019     0.037     0.511    72.927     0.611     1.043     0.523 
MOODprev                             0.049     0.062     0.782    65.996     0.437     1.158     0.550 
Sex                                  0.018     0.111     0.163    55.953     0.871     1.397     0.597 
PHQtot0                              0.006     0.016     0.355    64.262     0.724     1.192     0.557 
Neu0                                -0.010     0.007    -1.505    62.060     0.137     1.239     0.567 
depr0                               -0.052     0.105    -0.494    76.925     0.622     0.988     0.510 
pre_intern_mood                      0.008     0.068     0.119   119.959     0.905     0.661     0.408 
pre_intern_sleep                    -0.001     0.001    -1.177    69.783     0.243     1.091     0.535 
pre_intern_sqrt_step                 0.027     0.003    10.400   102.271     0.000     0.758     0.442 
week_category_new:STEP_COUNTprev    -0.049     0.037    -1.343   157.839     0.181     0.531     0.355 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(step_results3)

Call:
  
  testEstimates(model = step_results3)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                             6.517     1.153     5.652    68.184     0.000     1.118     0.541 
week_category_new                       1.165     0.782     1.491   125.220     0.139     0.638     0.399 
STEP_COUNTprev                          0.528     0.035    15.202   102.290     0.000     0.757     0.442 
SLEEP_COUNTprev                         0.019     0.036     0.531    73.636     0.597     1.032     0.521 
MOODprev                                0.044     0.064     0.694    54.282     0.491     1.449     0.606 
Sex                                     0.009     0.114     0.077    47.834     0.939     1.704     0.645 
PHQtot0                                 0.007     0.016     0.423    71.740     0.673     1.060     0.528 
Neu0                                   -0.012     0.007    -1.778    64.245     0.080     1.192     0.557 
depr0                                  -0.069     0.101    -0.691    78.305     0.492     0.971     0.505 
pre_intern_mood                        -0.002     0.060    -0.028   101.314     0.978     0.764     0.444 
pre_intern_sleep                       -0.001     0.001    -1.216    67.035     0.228     1.139     0.546 
pre_intern_sqrt_step                    0.027     0.002    11.231   111.677     0.000     0.702     0.423 
week_category_new:STEP_COUNTprev       -0.048     0.036    -1.357   162.867     0.177     0.519     0.349 
week_category_new:aggSTEP_COUNTprev    -0.010     0.014    -0.709    53.924     0.482     1.461     0.608 

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
(Intercept)                          10.146     0.450    22.539    38.097     0.000     2.404     0.721 
week_category_new                     1.325     0.350     3.782    52.151     0.000     1.523     0.618 
STEP_COUNTprev                        0.007     0.005     1.384    45.290     0.173     1.839     0.662 
SLEEP_COUNTprev                       0.378     0.020    19.042    36.802     0.000     2.553     0.733 
MOODprev                             -0.007     0.014    -0.487    45.853     0.629     1.807     0.658 
Sex                                   0.186     0.028     6.713    72.418     0.000     1.050     0.525 
PHQtot0                              -0.005     0.005    -0.892    76.165     0.375     0.998     0.512 
Neu0                                 -0.001     0.002    -0.608   233.019     0.544     0.400     0.292 
depr0                                -0.037     0.026    -1.387    85.706     0.169     0.890     0.483 
pre_intern_mood                      -0.017     0.021    -0.837    41.608     0.408     2.084     0.690 
pre_intern_sleep                      0.005     0.000    11.556    30.749     0.000     3.674     0.799 
pre_intern_sqrt_step                  0.001     0.001     0.502    29.416     0.619     4.094     0.816 
week_category_new:SLEEP_COUNTprev    -0.068     0.017    -3.916    53.874     0.000     1.462     0.608 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(sleep_results2)

Call:
  
  testEstimates(model = sleep_results2)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                          10.487     0.693    15.143    73.137     0.000     1.040     0.523 
week_category_new                     1.543     0.767     2.012   121.126     0.046     0.656     0.406 
STEP_COUNTprev                        0.007     0.014     0.467    44.125     0.643     1.909     0.671 
SLEEP_COUNTprev                       0.366     0.035    10.576    78.082     0.000     0.974     0.506 
MOODprev                             -0.020     0.030    -0.651    74.866     0.517     1.015     0.517 
Sex                                   0.124     0.060     2.067    54.899     0.043     1.429     0.603 
PHQtot0                              -0.003     0.008    -0.369   108.063     0.713     0.722     0.430 
Neu0                                 -0.000     0.004    -0.106    71.254     0.916     1.068     0.529 
depr0                                -0.063     0.060    -1.051    53.872     0.298     1.462     0.608 
pre_intern_mood                      -0.011     0.032    -0.336    76.226     0.738     0.997     0.512 
pre_intern_sleep                      0.005     0.001     9.726    40.846     0.000     2.145     0.697 
pre_intern_sqrt_step                  0.001     0.002     0.343    43.295     0.733     1.963     0.677 
week_category_new:SLEEP_COUNTprev    -0.081     0.039    -2.082   120.267     0.039     0.660     0.407 

Unadjusted hypothesis test as appropriate in larger samples. 

> testEstimates(sleep_results3)

Call:
  
  testEstimates(model = sleep_results3)

Final parameter estimates and inferences obtained from 20 imputed data sets.

Estimate Std.Error   t.value        df   P(>|t|)       RIV       FMI 
(Intercept)                             10.485     0.697    15.043    76.000     0.000     1.000     0.513 
week_category_new                        1.545     0.779     1.984   113.901     0.050     0.690     0.419 
STEP_COUNTprev                           0.007     0.014     0.457    43.493     0.650     1.949     0.676 
SLEEP_COUNTprev                          0.365     0.034    10.604    78.288     0.000     0.971     0.505 
MOODprev                                -0.019     0.031    -0.629    68.306     0.532     1.116     0.541 
Sex                                      0.125     0.061     2.032    49.373     0.048     1.634     0.635 
PHQtot0                                 -0.003     0.008    -0.371   105.065     0.712     0.740     0.436 
Neu0                                    -0.000     0.003    -0.100    74.827     0.921     1.016     0.517 
depr0                                   -0.063     0.057    -1.098    50.383     0.277     1.591     0.629 
pre_intern_mood                         -0.011     0.032    -0.330    80.000     0.743     0.951     0.500 
pre_intern_sleep                         0.005     0.001     9.959    41.534     0.000     2.090     0.691 
pre_intern_sqrt_step                     0.001     0.002     0.363    44.631     0.718     1.877     0.667 
week_category_new:SLEEP_COUNTprev       -0.081     0.039    -2.107   123.907     0.037     0.644     0.401 
week_category_new:aggSLEEP_COUNTprev     0.000     0.006     0.049   107.993     0.961     0.722     0.430 

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
[1] 333 199  38 170 121  77  67 110   7   9 134 135  22   3  55   6  23  22  11   1   2   1  15
[24]   1