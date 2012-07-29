# Example 1 from Lavaan

This exercise examines the first example shown in 
<http://www.jstatsoft.org/v48/i02/paper>.
It's a three-factor confirmatory factor analysis example with three items per factor.
All three latent factors are permitted to correlate.

* `x1` to `x3` load on a `visual` factor
* `x4` to `x6` load on a `textual` factor
* `x7` to `x9` load on a `speed` factor



```r
library('lavaan')
library('Hmisc')
cases <- HolzingerSwineford1939
```




## Quick examination of data



```r
str(cases)
```

```
## 'data.frame':	301 obs. of  15 variables:
##  $ id    : int  1 2 3 4 5 6 7 8 9 11 ...
##  $ sex   : int  1 2 2 1 2 2 1 2 2 2 ...
##  $ ageyr : int  13 13 13 13 12 14 12 12 13 12 ...
##  $ agemo : int  1 7 1 2 2 1 1 2 0 5 ...
##  $ school: Factor w/ 2 levels "Grant-White",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ grade : int  7 7 7 7 7 7 7 7 7 7 ...
##  $ x1    : num  3.33 5.33 4.5 5.33 4.83 ...
##  $ x2    : num  7.75 5.25 5.25 7.75 4.75 5 6 6.25 5.75 5.25 ...
##  $ x3    : num  0.375 2.125 1.875 3 0.875 ...
##  $ x4    : num  2.33 1.67 1 2.67 2.67 ...
##  $ x5    : num  5.75 3 1.75 4.5 4 3 6 4.25 5.75 5 ...
##  $ x6    : num  1.286 1.286 0.429 2.429 2.571 ...
##  $ x7    : num  3.39 3.78 3.26 3 3.7 ...
##  $ x8    : num  5.75 6.25 3.9 5.3 6.3 6.65 6.2 5.15 4.65 4.55 ...
##  $ x9    : num  6.36 7.92 4.42 4.86 5.92 ...
```

```r
Hmisc::describe(cases)
```

```
## cases 
## 
##  15  Variables      301  Observations
## ---------------------------------------------------------------------------
## id 
##       n missing  unique    Mean     .05     .10     .25     .50     .75 
##     301       0     301   176.6      17      33      82     163     272 
##     .90     .95 
##     318     335 
## 
## lowest :   1   2   3   4   5, highest: 346 347 348 349 351 
## ---------------------------------------------------------------------------
## sex 
##       n missing  unique    Mean 
##     301       0       2   1.515 
## 
## 1 (146, 49%), 2 (155, 51%) 
## ---------------------------------------------------------------------------
## ageyr 
##       n missing  unique    Mean 
##     301       0       6      13 
## 
##           11  12  13 14 15 16
## Frequency  8 101 110 55 20  7
## %          3  34  37 18  7  2
## ---------------------------------------------------------------------------
## agemo 
##       n missing  unique    Mean     .05     .10     .25     .50     .75 
##     301       0      12   5.375       0       1       2       5       8 
##     .90     .95 
##      10      11 
## 
##            0  1  2  3  4  5  6  7  8  9 10 11
## Frequency 22 31 26 26 27 27 21 25 26 23 19 28
## %          7 10  9  9  9  9  7  8  9  8  6  9
## ---------------------------------------------------------------------------
## school 
##       n missing  unique 
##     301       0       2 
## 
## Grant-White (145, 48%), Pasteur (156, 52%) 
## ---------------------------------------------------------------------------
## grade 
##       n missing  unique    Mean 
##     300       1       2   7.477 
## 
## 7 (157, 52%), 8 (143, 48%) 
## ---------------------------------------------------------------------------
## x1 
##       n missing  unique    Mean     .05     .10     .25     .50     .75 
##     301       0      35   4.936   3.000   3.333   4.167   5.000   5.667 
##     .90     .95 
##   6.333   6.667 
## 
## lowest : 0.6667 1.6667 1.8333 2.0000 2.6667
## highest: 7.0000 7.1667 7.3333 7.5000 8.5000 
## ---------------------------------------------------------------------------
## x2 
##       n missing  unique    Mean     .05     .10     .25     .50     .75 
##     301       0      25   6.088    4.50    4.75    5.25    6.00    6.75 
##     .90     .95 
##    7.75    8.50 
## 
## lowest : 2.25 3.50 3.75 4.00 4.25, highest: 8.25 8.50 8.75 9.00 9.25 
## ---------------------------------------------------------------------------
## x3 
##       n missing  unique    Mean     .05     .10     .25     .50     .75 
##     301       0      35    2.25   0.625   0.875   1.375   2.125   3.125 
##     .90     .95 
##   4.000   4.250 
## 
## lowest : 0.250 0.375 0.500 0.625 0.750
## highest: 4.000 4.125 4.250 4.375 4.500 
## ---------------------------------------------------------------------------
## x4 
##       n missing  unique    Mean     .05     .10     .25     .50     .75 
##     301       0      20   3.061   1.333   1.667   2.333   3.000   3.667 
##     .90     .95 
##   4.667   5.000 
## 
## lowest : 0.0000 0.3333 0.6667 1.0000 1.3333
## highest: 5.0000 5.3333 5.6667 6.0000 6.3333 
## ---------------------------------------------------------------------------
## x5 
##       n missing  unique    Mean     .05     .10     .25     .50     .75 
##     301       0      25   4.341    2.00    2.50    3.50    4.50    5.25 
##     .90     .95 
##    6.00    6.25 
## 
## lowest : 1.00 1.25 1.50 1.75 2.00, highest: 6.00 6.25 6.50 6.75 7.00 
## ---------------------------------------------------------------------------
## x6 
##       n missing  unique    Mean     .05     .10     .25     .50     .75 
##     301       0      40   2.186  0.7143  1.0000  1.4286  2.0000  2.7143 
##     .90     .95 
##  3.7143  4.2857 
## 
## lowest : 0.1429 0.2857 0.4286 0.5714 0.7143
## highest: 5.1429 5.4286 5.5714 5.8571 6.1429 
## ---------------------------------------------------------------------------
## x7 
##       n missing  unique    Mean     .05     .10     .25     .50     .75 
##     301       0      97   4.186   2.435   2.826   3.478   4.087   4.913 
##     .90     .95 
##   5.696   5.870 
## 
## lowest : 1.304 1.870 2.000 2.043 2.130
## highest: 6.652 6.826 6.957 7.261 7.435 
## ---------------------------------------------------------------------------
## x8 
##       n missing  unique    Mean     .05     .10     .25     .50     .75 
##     301       0      84   5.527    3.90    4.20    4.85    5.50    6.10 
##     .90     .95 
##    6.80    7.20 
## 
## lowest :  3.05  3.50  3.60  3.65  3.70
## highest:  8.00  8.05  8.30  9.10 10.00 
## ---------------------------------------------------------------------------
## x9 
##       n missing  unique    Mean     .05     .10     .25     .50     .75 
##     301       0     129   5.374   3.750   4.111   4.750   5.417   6.083 
##     .90     .95 
##   6.667   7.000 
## 
## lowest : 2.778 3.111 3.222 3.278 3.306
## highest: 7.528 7.611 7.917 8.611 9.250 
## ---------------------------------------------------------------------------
```




The data set include `301` observations. It includes a few demographic variables (e.g., sex, age in years and months, school, and grade). It includes nine variables  that are the observed test scores used in the subsequent CFA.

## Fit CFA


```r
m1_model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
'

m1_fit <- cfa(m1_model, data=cases)
```




* model syntax is specified as a character variable
* `cfa` is one model fitting function in `lavaan`. The command includes many options. Data can be specified as a data frame, as it is here using the `data` argument. Alternatively covariance matrix, vector of means, and sample size can be specified.
* From what I can tell, `lavaan` is the parent model fitting function that can take a `model.type` argument of `'cfa'`, `'sem'`, or `'growth'`. Thus, the arguments to `model.type` are functions themselves which just call `lavaan` with particular argument values.

## Show parameter table


```r
parTable(m1_fit)
```

```
##    id     lhs op     rhs user group free ustart exo label eq.id unco
## 1   1  visual =~      x1    1     1    0      1   0           0    0
## 2   2  visual =~      x2    1     1    1     NA   0           0    1
## 3   3  visual =~      x3    1     1    2     NA   0           0    2
## 4   4 textual =~      x4    1     1    0      1   0           0    0
## 5   5 textual =~      x5    1     1    3     NA   0           0    3
## 6   6 textual =~      x6    1     1    4     NA   0           0    4
## 7   7   speed =~      x7    1     1    0      1   0           0    0
## 8   8   speed =~      x8    1     1    5     NA   0           0    5
## 9   9   speed =~      x9    1     1    6     NA   0           0    6
## 10 10      x1 ~~      x1    0     1    7     NA   0           0    7
## 11 11      x2 ~~      x2    0     1    8     NA   0           0    8
## 12 12      x3 ~~      x3    0     1    9     NA   0           0    9
## 13 13      x4 ~~      x4    0     1   10     NA   0           0   10
## 14 14      x5 ~~      x5    0     1   11     NA   0           0   11
## 15 15      x6 ~~      x6    0     1   12     NA   0           0   12
## 16 16      x7 ~~      x7    0     1   13     NA   0           0   13
## 17 17      x8 ~~      x8    0     1   14     NA   0           0   14
## 18 18      x9 ~~      x9    0     1   15     NA   0           0   15
## 19 19  visual ~~  visual    0     1   16     NA   0           0   16
## 20 20 textual ~~ textual    0     1   17     NA   0           0   17
## 21 21   speed ~~   speed    0     1   18     NA   0           0   18
## 22 22  visual ~~ textual    0     1   19     NA   0           0   19
## 23 23  visual ~~   speed    0     1   20     NA   0           0   20
## 24 24 textual ~~   speed    0     1   21     NA   0           0   21
```




* What do the columns mean?
    * `id`: numeric identifier for the parameter
    * `lhs`: left hand side variable name
    * `op`: operator (see page 7 of http://www.jstatsoft.org/v48/i02/paper); `=~` 
    is manifested by; `~~` is correlated with.
    * `rhs`: right hand side variable name
    * `user`: 1 if parameter was specified by the user, 0 otherwise
    * `group`: presumably used in multiple group analysis
    * `free`: Nonzero elements are free parameters in the model
    * `ustart`: The value specified for fixed parameters
    * `exo`: ???
    * `label`: Probably just an optional label???
    * `eq.id`: ??? 
    * `unco`: ???


* The model syntax used in `lavaan` incorporates a lot of parameters by default to permit a tidy model syntax. The exact nature of these parameters is also determined by options in the `cfa`, `sem` and other model fitting fucntions.
* `parTable` is a method

It shows that the latent factors are allowed to intercorrelate. The `cfa` function has an an argument `orthogonal`. It defaults to FALSE which permits correlated factors.



```r
parTable(cfa(m1_model, data=cases, orthogonal=TRUE))[22:24, ]
```

```
##    id     lhs op     rhs user group free ustart exo label eq.id unco
## 22 22  visual ~~ textual    0     1    0      0   0           0    0
## 23 23  visual ~~   speed    0     1    0      0   0           0    0
## 24 24 textual ~~   speed    0     1    0      0   0           0    0
```



When `orthogonal=TRUE` is specified, the covariance of latent factors is constrained to zero. This is reflected in `free=0` (i.e., it's not free to vary) and `ustart=0` (the constrained value is zero) in the parameter table.

Returning to the original parameter table:

* Variance parameters (`op=~~` where `lhs` is the same as `rhs`) are included for all observed and latent variables.

## Summarise fit


```r
summary(m1_fit)
```

```
## lavaan (0.4-14) converged normally after 41 iterations
## 
##   Number of observations                           301
## 
##   Estimator                                         ML
##   Minimum Function Chi-square                   85.306
##   Degrees of freedom                                24
##   P-value                                        0.000
## 
## Parameter estimates:
## 
##   Information                                 Expected
##   Standard Errors                             Standard
## 
##                    Estimate  Std.err  Z-value  P(>|z|)
## Latent variables:
##   visual =~
##     x1                1.000
##     x2                0.553    0.100    5.554    0.000
##     x3                0.729    0.109    6.685    0.000
##   textual =~
##     x4                1.000
##     x5                1.113    0.065   17.014    0.000
##     x6                0.926    0.055   16.703    0.000
##   speed =~
##     x7                1.000
##     x8                1.180    0.165    7.152    0.000
##     x9                1.082    0.151    7.155    0.000
## 
## Covariances:
##   visual ~~
##     textual           0.408    0.074    5.552    0.000
##     speed             0.262    0.056    4.660    0.000
##   textual ~~
##     speed             0.173    0.049    3.518    0.000
## 
## Variances:
##     x1                0.549    0.114
##     x2                1.134    0.102
##     x3                0.844    0.091
##     x4                0.371    0.048
##     x5                0.446    0.058
##     x6                0.356    0.043
##     x7                0.799    0.081
##     x8                0.488    0.074
##     x9                0.566    0.071
##     visual            0.809    0.145
##     textual           0.979    0.112
##     speed             0.384    0.086
## 
```




The default `summary` method shows $\chi^2$, $df$, p-value for the overall model, unstandardised parameter estimates, in some cases with significance tests.


## Getting fit statistics
There are multiple ways of getting fit statistics



```r
fitMeasures(m1_fit)
```

```
##             chisq                df            pvalue    baseline.chisq 
##            85.306            24.000             0.000           918.852 
##       baseline.df   baseline.pvalue               cfi               tli 
##            36.000             0.000             0.931             0.896 
##              logl unrestricted.logl              npar               aic 
##         -3737.745         -3695.092            21.000          7517.490 
##               bic            ntotal              bic2             rmsea 
##          7595.339           301.000          7528.739             0.092 
##    rmsea.ci.lower    rmsea.ci.upper      rmsea.pvalue              srmr 
##             0.071             0.114             0.001             0.065 
```

```r
# equivalent to:
# inspect(m1_fit, 'fit.measures')

fitMeasures(m1_fit)['rmsea']
```

```
##   rmsea 
## 0.09212 
```

```r
fitMeasures(m1_fit, c('rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper'))
```

```
##          rmsea rmsea.ci.lower rmsea.ci.upper 
##          0.092          0.071          0.114 
```

```r


summary(m1_fit, fit.measures=TRUE)
```

```
## lavaan (0.4-14) converged normally after 41 iterations
## 
##   Number of observations                           301
## 
##   Estimator                                         ML
##   Minimum Function Chi-square                   85.306
##   Degrees of freedom                                24
##   P-value                                        0.000
## 
## Chi-square test baseline model:
## 
##   Minimum Function Chi-square                  918.852
##   Degrees of freedom                                36
##   P-value                                        0.000
## 
## Full model versus baseline model:
## 
##   Comparative Fit Index (CFI)                    0.931
##   Tucker-Lewis Index (TLI)                       0.896
## 
## Loglikelihood and Information Criteria:
## 
##   Loglikelihood user model (H0)              -3737.745
##   Loglikelihood unrestricted model (H1)      -3695.092
## 
##   Number of free parameters                         21
##   Akaike (AIC)                                7517.490
##   Bayesian (BIC)                              7595.339
##   Sample-size adjusted Bayesian (BIC)         7528.739
## 
## Root Mean Square Error of Approximation:
## 
##   RMSEA                                          0.092
##   90 Percent Confidence Interval          0.071  0.114
##   P-value RMSEA <= 0.05                          0.001
## 
## Standardized Root Mean Square Residual:
## 
##   SRMR                                           0.065
## 
## Parameter estimates:
## 
##   Information                                 Expected
##   Standard Errors                             Standard
## 
##                    Estimate  Std.err  Z-value  P(>|z|)
## Latent variables:
##   visual =~
##     x1                1.000
##     x2                0.553    0.100    5.554    0.000
##     x3                0.729    0.109    6.685    0.000
##   textual =~
##     x4                1.000
##     x5                1.113    0.065   17.014    0.000
##     x6                0.926    0.055   16.703    0.000
##   speed =~
##     x7                1.000
##     x8                1.180    0.165    7.152    0.000
##     x9                1.082    0.151    7.155    0.000
## 
## Covariances:
##   visual ~~
##     textual           0.408    0.074    5.552    0.000
##     speed             0.262    0.056    4.660    0.000
##   textual ~~
##     speed             0.173    0.049    3.518    0.000
## 
## Variances:
##     x1                0.549    0.114
##     x2                1.134    0.102
##     x3                0.844    0.091
##     x4                0.371    0.048
##     x5                0.446    0.058
##     x6                0.356    0.043
##     x7                0.799    0.081
##     x8                0.488    0.074
##     x9                0.566    0.071
##     visual            0.809    0.145
##     textual           0.979    0.112
##     speed             0.384    0.086
## 
```





* I assume that lavaan uses S4 classes which makes extracting elements a little different to S3 classes.
* The above code shows how to extract fit measures.
* While it is not clear hear, it appears that `rmsea.ci.lower` and `rmsea.ci.upper` refer to 90% lower and upper confidence intervals.
* Adding `fit.measures=TRUE` provides a way of displaying 


## Modification indices


```r
m1_mod <- modificationIndices(m1_fit)
head(m1_mod[order(m1_mod$mi, decreasing=TRUE), ], 10)
```

```
##        lhs op rhs     mi    epc sepc.lv sepc.all sepc.nox
## 1   visual =~  x9 36.411  0.577   0.519    0.515    0.515
## 2       x7 ~~  x8 34.145  0.536   0.536    0.488    0.488
## 3   visual =~  x7 18.631 -0.422  -0.380   -0.349   -0.349
## 4       x8 ~~  x9 14.946 -0.423  -0.423   -0.415   -0.415
## 5  textual =~  x3  9.151 -0.272  -0.269   -0.238   -0.238
## 6       x2 ~~  x7  8.918 -0.183  -0.183   -0.143   -0.143
## 7  textual =~  x1  8.903  0.350   0.347    0.297    0.297
## 8       x2 ~~  x3  8.532  0.218   0.218    0.164    0.164
## 9       x3 ~~  x5  7.858 -0.130  -0.130   -0.089   -0.089
## 10  visual =~  x5  7.441 -0.210  -0.189   -0.147   -0.147
```




* The `modificationIndices` function returns modification indices and expected parameter changes (EPCs). 
* The second line above sorts the rows of the modification indices table in decreasing order and shows those parameters with the 10 largest values.



```r
m2_model <- ' visual  =~ x1 + x2 + x3 + x9
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
'

m2_fit <- cfa(m2_model, data=cases)
anova(m1_fit, m2_fit)
```

```
## Chi Square Difference Test
## 
##        Df  AIC  BIC Chisq Chisq diff Df diff Pr(>Chisq)    
## m2_fit 23 7487 7568  52.4                                  
## m1_fit 24 7517 7595  85.3       32.9       1    9.6e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
```




* Note that more than one empty line at the end of the model definition seems to cause an error.
* TODO: Work out why the change in $\chi^2$ 
`32.9234`
is different to the value of the modification index
`36.411`.

## Standardised parameters


```r
summary(m1_fit)
```

```
## lavaan (0.4-14) converged normally after 41 iterations
## 
##   Number of observations                           301
## 
##   Estimator                                         ML
##   Minimum Function Chi-square                   85.306
##   Degrees of freedom                                24
##   P-value                                        0.000
## 
## Parameter estimates:
## 
##   Information                                 Expected
##   Standard Errors                             Standard
## 
##                    Estimate  Std.err  Z-value  P(>|z|)
## Latent variables:
##   visual =~
##     x1                1.000
##     x2                0.553    0.100    5.554    0.000
##     x3                0.729    0.109    6.685    0.000
##   textual =~
##     x4                1.000
##     x5                1.113    0.065   17.014    0.000
##     x6                0.926    0.055   16.703    0.000
##   speed =~
##     x7                1.000
##     x8                1.180    0.165    7.152    0.000
##     x9                1.082    0.151    7.155    0.000
## 
## Covariances:
##   visual ~~
##     textual           0.408    0.074    5.552    0.000
##     speed             0.262    0.056    4.660    0.000
##   textual ~~
##     speed             0.173    0.049    3.518    0.000
## 
## Variances:
##     x1                0.549    0.114
##     x2                1.134    0.102
##     x3                0.844    0.091
##     x4                0.371    0.048
##     x5                0.446    0.058
##     x6                0.356    0.043
##     x7                0.799    0.081
##     x8                0.488    0.074
##     x9                0.566    0.071
##     visual            0.809    0.145
##     textual           0.979    0.112
##     speed             0.384    0.086
## 
```

```r
standardizedSolution(m1_fit)
```

```
##        lhs op     rhs est.std se  z pvalue
## 1   visual =~      x1   0.772 NA NA     NA
## 2   visual =~      x2   0.424 NA NA     NA
## 3   visual =~      x3   0.581 NA NA     NA
## 4  textual =~      x4   0.852 NA NA     NA
## 5  textual =~      x5   0.855 NA NA     NA
## 6  textual =~      x6   0.838 NA NA     NA
## 7    speed =~      x7   0.570 NA NA     NA
## 8    speed =~      x8   0.723 NA NA     NA
## 9    speed =~      x9   0.665 NA NA     NA
## 10      x1 ~~      x1   0.404 NA NA     NA
## 11      x2 ~~      x2   0.821 NA NA     NA
## 12      x3 ~~      x3   0.662 NA NA     NA
## 13      x4 ~~      x4   0.275 NA NA     NA
## 14      x5 ~~      x5   0.269 NA NA     NA
## 15      x6 ~~      x6   0.298 NA NA     NA
## 16      x7 ~~      x7   0.676 NA NA     NA
## 17      x8 ~~      x8   0.477 NA NA     NA
## 18      x9 ~~      x9   0.558 NA NA     NA
## 19  visual ~~  visual   1.000 NA NA     NA
## 20 textual ~~ textual   1.000 NA NA     NA
## 21   speed ~~   speed   1.000 NA NA     NA
## 22  visual ~~ textual   0.459 NA NA     NA
## 23  visual ~~   speed   0.471 NA NA     NA
## 24 textual ~~   speed   0.283 NA NA     NA
```






