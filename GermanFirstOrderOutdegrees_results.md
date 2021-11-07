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
    ## 10 2015  powerlaw              1.61    27   NA        0.06
    ## 11 2016  powerlaw              1.63    24   NA        0.03
    ## 12 2017  powerlaw              1.61    26   NA        0.09
    ## 13 2015  powerlaw_exogcut      2.30    14   NA        0.31
    ## 14 2016  powerlaw_exogcut      2.33    14   NA        0.19
    ## 15 2017  powerlaw_exogcut      2.21    14   NA        0.39

 

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

 

### Empirical CCDF 2002 first order outdegrees

![](GermanFirstOrderOutdegrees_results_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

 

### Plots of fitted distributions versus fitted power law

#### Linear Regression (left) and linear regression with GI correction (right) versus power law (20% tail)

<img src="GermanFirstOrderOutdegrees_results_files/figure-gfm/unnamed-chunk-7-1.png" width="50%" /><img src="GermanFirstOrderOutdegrees_results_files/figure-gfm/unnamed-chunk-7-2.png" width="50%" />

 

 

#### Kernel Smoothing Regression and power law (20% tail)

![](GermanFirstOrderOutdegrees_results_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

 

 

#### Exponential distribution and power law (20% tail, left, and optimal tail length, right)

<img src="GermanFirstOrderOutdegrees_results_files/figure-gfm/unnamed-chunk-9-1.png" width="50%" /><img src="GermanFirstOrderOutdegrees_results_files/figure-gfm/unnamed-chunk-9-2.png" width="50%" />

 

 

#### Lognormal and weibull distribution versus power law (optimal tail length)

<img src="GermanFirstOrderOutdegrees_results_files/figure-gfm/unnamed-chunk-10-1.png" width="50%" /><img src="GermanFirstOrderOutdegrees_results_files/figure-gfm/unnamed-chunk-10-2.png" width="50%" />
