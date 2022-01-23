German first order outdegrees
================

Year: 2015, 2016, 2017

 

### Fitted distributions parameters

The following table contains the estimated slope of the tail with
different models. The standard error of the two linear regressions
differ so much because the corrected regression also uses an upward
corrected standard error. Also the p-value of a goodness-of-fit test via
bootstrapping for the powerlaw is reported. A high p-value indicates
that the distribution is a good fit to to the data.

The estimated slopes are much steeper here than for the US outdegree
distribution. Only the power law estimated with optimal tail length has
a slope that is not so steep (1.61).

    ## # A tibble: 15 × 6
    ##    year  distribution     parameter  xmin stderror p_value
    ##    <chr> <chr>                <dbl> <dbl>    <dbl>   <dbl>
    ##  1 2015  lm                    2.46    14    0.229   NA   
    ##  2 2016  lm                    2.57    14    0.237   NA   
    ##  3 2017  lm                    2.42    14    0.256   NA   
    ##  4 2015  lm_GIcorrect          2.90    14    1.10    NA   
    ##  5 2016  lm_GIcorrect          3.03    14    1.15    NA   
    ##  6 2017  lm_GIcorrect          2.83    14    1.07    NA   
    ##  7 2015  ksr                   2.69    14   NA       NA   
    ##  8 2016  ksr                   2.74    14   NA       NA   
    ##  9 2017  ksr                   2.50    14   NA       NA   
    ## 10 2015  powerlaw              1.61    27   NA        0.08
    ## 11 2016  powerlaw              1.63    24   NA        0.05
    ## 12 2017  powerlaw              1.61    26   NA        0.16
    ## 13 2015  powerlaw_exogcut      2.30    14   NA        0.23
    ## 14 2016  powerlaw_exogcut      2.33    14   NA        0.22
    ## 15 2017  powerlaw_exogcut      2.21    14   NA        0.38

    % latex table generated in R 4.1.2 by xtable 1.8-4 package
    % Sun Jan 23 13:00:05 2022
    \begin{table}[ht]
    \centering
    \begin{tabular}{rllrrrr}
      \hline
     & year & distribution & parameter & xmin & stderror & p\_value \\ 
      \hline
    1 & 2015 & lm & 2.46 & 14.00 & 0.23 &  \\ 
      2 & 2016 & lm & 2.57 & 14.00 & 0.24 &  \\ 
      3 & 2017 & lm & 2.42 & 14.00 & 0.26 &  \\ 
      4 & 2015 & lm\_GIcorrect & 2.90 & 14.00 & 1.10 &  \\ 
      5 & 2016 & lm\_GIcorrect & 3.03 & 14.00 & 1.15 &  \\ 
      6 & 2017 & lm\_GIcorrect & 2.83 & 14.00 & 1.07 &  \\ 
      7 & 2015 & ksr & 2.69 & 14.00 &  &  \\ 
      8 & 2016 & ksr & 2.74 & 14.00 &  &  \\ 
      9 & 2017 & ksr & 2.50 & 14.00 &  &  \\ 
      10 & 2015 & powerlaw & 1.61 & 27.00 &  & 0.08 \\ 
      11 & 2016 & powerlaw & 1.63 & 24.00 &  & 0.05 \\ 
      12 & 2017 & powerlaw & 1.61 & 26.00 &  & 0.16 \\ 
      13 & 2015 & powerlaw\_exogcut & 2.30 & 14.00 &  & 0.23 \\ 
      14 & 2016 & powerlaw\_exogcut & 2.33 & 14.00 &  & 0.22 \\ 
      15 & 2017 & powerlaw\_exogcut & 2.21 & 14.00 &  & 0.38 \\ 
       \hline
    \end{tabular}
    \end{table}

 

### Comparing GoF of alternative distributions to power law

P-values of the one sided likelihood ratio tests. The powerlaw of 2
different tail lengths is compared to the lognormal, weibull and
exponential distribution (each estimated with the tail length
corresponding to the powerlaw they are compared to). The p-value is the
probability of having the test statistic tending to one distribution if
actually the other distribution is a better fit. Thus the high p-values
for lognormal and weibull indicates that the tendency of the test
statistic to one distribution is not significant.

For the German data also the p-value for the exponential distribution is
very high, therefore it can not be concluded that the powerlaw is a
significantly better fit.

    ## # A tibble: 2 × 4
    ##   distribution     lognormal weibull exponential
    ##   <chr>                <dbl>   <dbl>       <dbl>
    ## 1 powerlaw             0.785   0.822       0.875
    ## 2 powerlaw_exogcut     0.688   0.759       0.899

    ## [1] -1.150136

    ## [1] -1.273712

    ## % latex table generated in R 4.1.2 by xtable 1.8-4 package
    ## % Sun Jan 23 13:00:05 2022
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rlrrr}
    ##   \hline
    ##  & distribution & lognormal & weibull & exponential \\ 
    ##   \hline
    ## 1 & powerlaw & 0.79 & 0.82 & 0.87 \\ 
    ##   2 & powerlaw\_exogcut & 0.69 & 0.76 & 0.90 \\ 
    ##    \hline
    ## \end{tabular}
    ## \end{table}

P-values of two sided likelihood ratio tests. The p-values are the
probability of getting a positive or negative test statistic with this
value if actually no distribution fits better. The p-values for
lognormal and weibull are large, therefore the test can not distinguish
whether the power law or these distributions fit better.

The p-value for the exponential distribution is quite high here, thus it
can not be concluded that one of the two distributions fits better.

    ## # A tibble: 2 × 4
    ##   distribution     lognormal weibull exponential
    ##   <chr>                <dbl>   <dbl>       <dbl>
    ## 1 powerlaw             0.429   0.357       0.250
    ## 2 powerlaw_exogcut     0.625   0.481       0.203

    ## % latex table generated in R 4.1.2 by xtable 1.8-4 package
    ## % Sun Jan 23 13:00:06 2022
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{rlrrr}
    ##   \hline
    ##  & distribution & lognormal & weibull & exponential \\ 
    ##   \hline
    ## 1 & powerlaw & 0.43 & 0.36 & 0.25 \\ 
    ##   2 & powerlaw\_exogcut & 0.62 & 0.48 & 0.20 \\ 
    ##    \hline
    ## \end{tabular}
    ## \end{table}

 

### Empirical CCDF 2015 first order outdegrees

![](GermanFirstOrderOutdegrees_results_files/figure-gfm/CCDF_fo_2015-1.png)<!-- -->

 

### Plots of fitted distributions versus fitted power law

#### Linear Regression (left) and linear regression with GI correction (right) versus power law (20% tail)

<img src="GermanFirstOrderOutdegrees_results_files/figure-gfm/LMandGIvsPL_exogtail-1.png" width="50%" /><img src="GermanFirstOrderOutdegrees_results_files/figure-gfm/LMandGIvsPL_exogtail-2.png" width="50%" />

 

 

#### Kernel Smoothing Regression and power law (20% tail)

![](GermanFirstOrderOutdegrees_results_files/figure-gfm/KSvsPL_exogtail-1.png)<!-- -->

 

 

#### Exponential distribution and power law (20% tail, left, and optimal tail length, right)

<img src="GermanFirstOrderOutdegrees_results_files/figure-gfm/EXPvsPL-1.png" width="50%" /><img src="GermanFirstOrderOutdegrees_results_files/figure-gfm/EXPvsPL-2.png" width="50%" />

 

 

#### Lognormal and weibull distribution versus power law (optimal tail length)

<img src="GermanFirstOrderOutdegrees_results_files/figure-gfm/LOGNandWEIBULLvsPL_endogtail-1.png" width="50%" /><img src="GermanFirstOrderOutdegrees_results_files/figure-gfm/LOGNandWEIBULLvsPL_endogtail-2.png" width="50%" />
