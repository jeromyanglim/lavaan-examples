



```r
library(psych)
library(lavaan)
Data <- bfi
item_names <- names(Data)[1:25]
```




# Check data



```r
sapply(Data[, item_names], function(X) sum(is.na(X)))
```

```
## A1 A2 A3 A4 A5 C1 C2 C3 C4 C5 E1 E2 E3 E4 E5 N1 N2 N3 N4 N5 O1 O2 O3 O4 O5 
## 16 27 26 19 16 21 24 20 26 16 23 16 25  9 21 22 21 11 36 29 22  0 28 14 20 
```

```r

Data$item_na <- apply(Data[, item_names], 1, function(X) sum(is.na(X)) > 
    0)

table(Data$item_na)
```

```
## 
## FALSE  TRUE 
##  2436   364 
```

```r
Data <- Data[!Data$item_na, ]
```




* I decided to remove data with missing data to simplify subsequent exploration of the features of the lavaan software.


# Basic CFA


```r
m1_model <- ' N =~ N1 + N2 + N3 + N4 + N5
              E =~ E1 + E2 + E3 + E4 + E5
              O =~ O1 + O2 + O3 + O4 + O5
              A =~ A1 + A2 + A3 + A4 + A5
              C =~ C1 + C2 + C3 + C4 + C5
'

m1_fit <- cfa(m1_model, data=Data[, item_names])
summary(m1_fit, standardized=TRUE)
```

```
## lavaan (0.4-14) converged normally after 63 iterations
## 
##   Number of observations                          2436
## 
##   Estimator                                         ML
##   Minimum Function Chi-square                 4165.467
##   Degrees of freedom                               265
##   P-value                                        0.000
## 
## Parameter estimates:
## 
##   Information                                 Expected
##   Standard Errors                             Standard
## 
##                    Estimate  Std.err  Z-value  P(>|z|)   Std.lv  Std.all
## Latent variables:
##   N =~
##     N1                1.000                               1.300    0.825
##     N2                0.947    0.024   39.899    0.000    1.230    0.803
##     N3                0.884    0.025   35.919    0.000    1.149    0.721
##     N4                0.692    0.025   27.753    0.000    0.899    0.573
##     N5                0.628    0.026   24.027    0.000    0.816    0.503
##   E =~
##     E1                1.000                               0.920    0.564
##     E2                1.226    0.051   23.899    0.000    1.128    0.699
##     E3               -0.921    0.041  -22.431    0.000   -0.847   -0.627
##     E4               -1.121    0.047  -23.977    0.000   -1.031   -0.703
##     E5               -0.808    0.039  -20.648    0.000   -0.743   -0.553
##   O =~
##     O1                1.000                               0.635    0.564
##     O2               -1.020    0.068  -14.962    0.000   -0.648   -0.418
##     O3                1.373    0.072   18.942    0.000    0.872    0.724
##     O4                0.437    0.048    9.160    0.000    0.277    0.233
##     O5               -0.960    0.060  -16.056    0.000   -0.610   -0.461
##   A =~
##     A1                1.000                               0.484    0.344
##     A2               -1.579    0.108  -14.650    0.000   -0.764   -0.648
##     A3               -2.030    0.134  -15.093    0.000   -0.983   -0.749
##     A4               -1.564    0.115  -13.616    0.000   -0.757   -0.510
##     A5               -1.804    0.121  -14.852    0.000   -0.873   -0.687
##   C =~
##     C1                1.000                               0.680    0.551
##     C2                1.148    0.057   20.152    0.000    0.781    0.592
##     C3                1.036    0.054   19.172    0.000    0.705    0.546
##     C4               -1.421    0.065  -21.924    0.000   -0.967   -0.702
##     C5               -1.489    0.072  -20.694    0.000   -1.013   -0.620
## 
## Covariances:
##   N ~~
##     E                 0.292    0.032    9.131    0.000    0.244    0.244
##     O                -0.093    0.022   -4.138    0.000   -0.112   -0.112
##     A                 0.141    0.018    7.713    0.000    0.223    0.223
##     C                -0.250    0.025  -10.118    0.000   -0.283   -0.283
##   E ~~
##     O                -0.265    0.021  -12.347    0.000   -0.453   -0.453
##     A                 0.304    0.025   12.293    0.000    0.683    0.683
##     C                -0.224    0.020  -11.121    0.000   -0.357   -0.357
##   O ~~
##     A                -0.093    0.011   -8.446    0.000   -0.303   -0.303
##     C                 0.130    0.014    9.190    0.000    0.301    0.301
##   A ~~
##     C                -0.110    0.012   -9.254    0.000   -0.334   -0.334
## 
## Variances:
##     N1                0.793    0.037                      0.793    0.320
##     N2                0.836    0.036                      0.836    0.356
##     N3                1.222    0.043                      1.222    0.481
##     N4                1.654    0.052                      1.654    0.672
##     N5                1.969    0.060                      1.969    0.747
##     E1                1.814    0.058                      1.814    0.682
##     E2                1.332    0.049                      1.332    0.512
##     E3                1.108    0.038                      1.108    0.607
##     E4                1.088    0.041                      1.088    0.506
##     E5                1.251    0.040                      1.251    0.694
##     O1                0.865    0.032                      0.865    0.682
##     O2                1.990    0.063                      1.990    0.826
##     O3                0.691    0.039                      0.691    0.476
##     O4                1.346    0.040                      1.346    0.946
##     O5                1.380    0.045                      1.380    0.788
##     A1                1.745    0.052                      1.745    0.882
##     A2                0.807    0.028                      0.807    0.580
##     A3                0.754    0.032                      0.754    0.438
##     A4                1.632    0.051                      1.632    0.740
##     A5                0.852    0.032                      0.852    0.528
##     C1                1.063    0.035                      1.063    0.697
##     C2                1.130    0.039                      1.130    0.650
##     C3                1.170    0.039                      1.170    0.702
##     C4                0.960    0.040                      0.960    0.507
##     C5                1.640    0.059                      1.640    0.615
##     N                 1.689    0.073                      1.000    1.000
##     E                 0.846    0.062                      1.000    1.000
##     O                 0.404    0.033                      1.000    1.000
##     A                 0.234    0.030                      1.000    1.000
##     C                 0.463    0.036                      1.000    1.000
## 
```




* **`Std.lv`**: Only latent variables have been standardized
* **`Std.all`**: Observed and latent variables have been standardized. 
* **Factor loadings**: Under the `latent variables` section, the `Std.all` column provides standardised factor loadings. 
* **Factor correlations**: Under the `Covariances`  section, the `Std.all` column provides standardised factor loadings.
* **`Variances`**: Latent factor variances can be constrained for identifiability purposes to be 1, but in this case, one of the loadings was constrained to be one. Variances for items represent the variance not explained by the latent factor.





```r
variances <- c(unique = subset(inspect(m1_fit, "standardizedsolution"), 
    lhs == "N1" & rhs == "N1")[, "est.std"], common = subset(inspect(m1_fit, 
    "standardizedsolution"), lhs == "N" & rhs == "N1")[, "est.std"]^2)
(variances <- c(variances, total = sum(variances)))
```

```
## unique common  total 
## 0.3195 0.6805 1.0000 
```




* The output above illustrates the point about variances. Variance for each item is explained by either the common factor or by error variance. As there is just one latent factor loading on the item, the squared standardised coefficient is the variance explained by the common factor. The sum of the unique and common standardised variances is one, which naturally corresponds to the variance of a standardised variable.
* The code also demonstrates ideas about how to extract specific information from the lavaan model fit object. Specifically, the `inspect` method provides access to a wide range of specific information. See help for further details.
* I used the `subset` method to provide an easy one-liner for extracting elements from the data frame returned by the `inspect` method.



```r
variances <- c(N1_N1 = subset(parameterestimates(m1_fit), lhs == 
    "N1" & rhs == "N1")[, "est"], N_N = subset(parameterestimates(m1_fit), lhs == 
    "N" & rhs == "N")[, "est"], N_N1 = subset(parameterestimates(m1_fit), lhs == 
    "N" & rhs == "N1")[, "est"])

cbind(parameters = c(variances, total = variances["N_N1"] * variances["N_N"] + 
    variances["N1_N1"], raw_divide_by_n_minus_1 = var(Data[, "N1"]), raw_divide_by_n = mean((Data[, 
    "N1"] - mean(Data[, "N1"]))^2)))
```

```
##                         parameters
## N1_N1                       0.7932
## N_N                         1.6893
## N_N1                        1.0000
## total.N_N1                  2.4825
## raw_divide_by_n_minus_1     2.4835
## raw_divide_by_n             2.4825
```




* The output above shows the unstandardised parameters related to the item `N1`.
* `N1_N1` corresponds to the unstandardised unique variance for the item.
* `N_N` times `N_N1` represents the unstandardised common variance.
* Thus, the sum of the unique and common variance represents the total variance.
* When I calculated this on the raw data using the standard $n-1$ denominator, the value was slightly larger, but when I used $n$ as the denominator, the estimate was very close. 



# Compare with a single factor model


```r
m2_model <- ' G =~ N1 + N2 + N3 + N4 + N5
              + E1 + E2 + E3 + E4 + E5
              + O1 + O2 + O3 + O4 + O5
              + A1 + A2 + A3 + A4 + A5
              + C1 + C2 + C3 + C4 + C5
'

m2_fit <- cfa(m2_model, data=Data[, item_names])
summary(m2_fit, standardized=TRUE)
```

```
## lavaan (0.4-14) converged normally after 55 iterations
## 
##   Number of observations                          2436
## 
##   Estimator                                         ML
##   Minimum Function Chi-square                10673.239
##   Degrees of freedom                               275
##   P-value                                        0.000
## 
## Parameter estimates:
## 
##   Information                                 Expected
##   Standard Errors                             Standard
## 
##                    Estimate  Std.err  Z-value  P(>|z|)   Std.lv  Std.all
## Latent variables:
##   G =~
##     N1                1.000                               0.547    0.347
##     N2                0.959    0.081   11.809    0.000    0.524    0.342
##     N3                0.960    0.083   11.547    0.000    0.525    0.329
##     N4                1.375    0.099   13.919    0.000    0.752    0.479
##     N5                0.884    0.081   10.860    0.000    0.484    0.298
##     E1                1.332    0.099   13.509    0.000    0.728    0.447
##     E2                1.868    0.122   15.297    0.000    1.022    0.633
##     E3               -1.382    0.094  -14.730    0.000   -0.756   -0.559
##     E4               -1.702    0.111  -15.307    0.000   -0.931   -0.635
##     E5               -1.292    0.090  -14.425    0.000   -0.707   -0.526
##     O1               -0.656    0.058  -11.321    0.000   -0.359   -0.318
##     O2                0.444    0.067    6.641    0.000    0.243    0.156
##     O3               -0.877    0.068  -12.801    0.000   -0.479   -0.398
##     O4                0.142    0.048    2.930    0.003    0.078    0.065
##     O5                0.416    0.058    7.196    0.000    0.228    0.172
##     A1                0.568    0.065    8.797    0.000    0.311    0.221
##     A2               -1.032    0.074  -13.913    0.000   -0.565   -0.479
##     A3               -1.322    0.090  -14.663    0.000   -0.723   -0.552
##     A4               -1.172    0.088  -13.307    0.000   -0.641   -0.432
##     A5               -1.413    0.093  -15.123    0.000   -0.773   -0.608
##     C1               -0.705    0.063  -11.188    0.000   -0.386   -0.312
##     C2               -0.725    0.066  -10.923    0.000   -0.396   -0.301
##     C3               -0.682    0.064  -10.645    0.000   -0.373   -0.289
##     C4                1.009    0.079   12.852    0.000    0.552    0.401
##     C5                1.332    0.099   13.505    0.000    0.728    0.446
## 
## Variances:
##     N1                2.183    0.064                      2.183    0.880
##     N2                2.075    0.061                      2.075    0.883
##     N3                2.267    0.066                      2.267    0.892
##     N4                1.897    0.057                      1.897    0.770
##     N5                2.401    0.070                      2.401    0.911
##     E1                2.130    0.064                      2.130    0.801
##     E2                1.560    0.050                      1.560    0.599
##     E3                1.255    0.039                      1.255    0.687
##     E4                1.284    0.042                      1.284    0.597
##     E5                1.304    0.040                      1.304    0.723
##     O1                1.140    0.033                      1.140    0.899
##     O2                2.351    0.068                      2.351    0.976
##     O3                1.222    0.036                      1.222    0.842
##     O4                1.417    0.041                      1.417    0.996
##     O5                1.701    0.049                      1.701    0.970
##     A1                1.883    0.054                      1.883    0.951
##     A2                1.072    0.032                      1.072    0.771
##     A3                1.196    0.037                      1.196    0.696
##     A4                1.794    0.053                      1.794    0.814
##     A5                1.017    0.032                      1.017    0.630
##     C1                1.376    0.040                      1.376    0.902
##     C2                1.582    0.046                      1.582    0.910
##     C3                1.528    0.044                      1.528    0.917
##     C4                1.590    0.047                      1.590    0.839
##     C5                2.134    0.064                      2.134    0.801
##     G                 0.299    0.037                      1.000    1.000
## 
```






```r
round(cbind(m1 = inspect(m1_fit, "fit.measures"), m2 = inspect(m2_fit, 
    "fit.measures")), 3)
```

```
##                           m1         m2
## chisq               4165.467  1.067e+04
## df                   265.000  2.750e+02
## pvalue                 0.000  0.000e+00
## baseline.chisq     18222.116  1.822e+04
## baseline.df          300.000  3.000e+02
## baseline.pvalue        0.000  0.000e+00
## cfi                    0.782  4.200e-01
## tli                    0.754  3.670e-01
## logl              -99840.238 -1.031e+05
## unrestricted.logl -97757.504 -9.776e+04
## npar                  60.000  5.000e+01
## aic               199800.476  2.063e+05
## bic               200148.363  2.066e+05
## ntotal              2436.000  2.436e+03
## bic2              199957.729  2.064e+05
## rmsea                  0.078  1.250e-01
## rmsea.ci.lower         0.076  1.230e-01
## rmsea.ci.upper         0.080  1.270e-01
## rmsea.pvalue           0.000  0.000e+00
## srmr                   0.075  1.160e-01
```

```r
anova(m1_fit, m2_fit)
```

```
## Chi Square Difference Test
## 
##         Df    AIC    BIC Chisq Chisq diff Df diff Pr(>Chisq)    
## m1_fit 265 199800 200148  4165                                  
## m2_fit 275 206288 206578 10673       6508      10     <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
```




* The output compares the model fit statistics for the two models.
* It also performs a chi-square difference test which shows that a one-factor model has significantly worse fit than the two-factor model.


# Modification indices


```r
m1_mod <- modificationindices(m1_fit)
m1_mod_summary <- subset(m1_mod, mi > 100)
m1_mod_summary[order(m1_mod_summary$mi, decreasing = TRUE), ]
```

```
##    lhs op rhs    mi    epc sepc.lv sepc.all sepc.nox
## 1   N1 ~~  N2 418.8  0.841   0.841    0.348    0.348
## 2    E =~  N4 200.8  0.487   0.448    0.285    0.285
## 3    O =~  E3 153.7  0.672   0.427    0.316    0.316
## 4   N3 ~~  N4 134.1  0.403   0.403    0.161    0.161
## 5    O =~  E4 122.6 -0.636  -0.404   -0.276   -0.276
## 6    C =~  E5 121.5  0.504   0.343    0.255    0.255
## 7    E =~  O3 114.2 -0.429  -0.395   -0.328   -0.328
## 8    E =~  O4 113.9  0.372   0.343    0.287    0.287
## 9    N =~  C5 108.8  0.271   0.352    0.216    0.216
## 10   E =~  A5 108.6 -0.488  -0.449   -0.354   -0.354
## 11   N =~  C2 107.0  0.219   0.285    0.216    0.216
## 12  C1 ~~  C2 107.0  0.288   0.288    0.177    0.177
## 13  E2 ~~  O4 104.7  0.310   0.310    0.161    0.161
## 14  A1 ~~  A2 101.4 -0.276  -0.276   -0.166   -0.166
```




* `modificationindices` suggests several ad hoc modifications that could be made to improve the fit of the model.
* The largest index suggests that items `N1` and `N2` share common variance. If we look at the help file on the bfi dataset `?bfi`, we see tha the text for `N1` ("Get angry easily") and `N2` ("Get irritated easily") are very similar. 



```r
(N_cors <- round(cor(Data[, paste0("N", 1:5)]), 2))
```

```
##      N1   N2   N3   N4   N5
## N1 1.00 0.72 0.57 0.41 0.38
## N2 0.72 1.00 0.55 0.39 0.35
## N3 0.57 0.55 1.00 0.52 0.43
## N4 0.41 0.39 0.52 1.00 0.40
## N5 0.38 0.35 0.43 0.40 1.00
```

```r
N1_N2_corr <- N_cors["N1", "N2"]
other_N_corrs <- round(mean(abs(N_cors[lower.tri(N_cors)][-1])), 
    2)
```




* The correlation matrix also shows that the correlation N1 and N2 ($r = 0.72$) is much larger than it is for the other variables ($\text{mean}(|r|) = 0.44$).

# Various matrices
## Observed, fitted, and residual covariance matrices
The following analysis extracts observed, fitted, and residual covariances and checks that they are consistent with expectations. I only perform this for five items rather than the full 25 item set in order to make the point about demonstrating their meaning clearer.



```r
N_names <- paste0("N", 1:5)
N_matrices <- list(observed = inspect(m1_fit, "sampstat")$cov[N_names, 
    N_names], fitted = fitted(m1_fit)$cov[N_names, N_names], residual = resid(m1_fit)$cov[N_names, 
    N_names])

N_matrices$check <- N_matrices$observed - (N_matrices$fitted + N_matrices$residual)
lapply(N_matrices, function(X) round(X, 3))
```

```
## $observed
##       N1    N2    N3    N4    N5
## N1 2.482 1.735 1.425 1.013 0.973
## N2 1.735 2.350 1.344 0.950 0.873
## N3 1.425 1.344 2.542 1.309 1.114
## N4 1.013 0.950 1.309 2.463 1.026
## N5 0.973 0.873 1.114 1.026 2.635
## 
## $fitted
##       N1    N2    N3    N4    N5
## N1 2.482 1.599 1.493 1.169 1.061
## N2 1.599 2.350 1.414 1.106 1.004
## N3 1.493 1.414 2.542 1.033 0.937
## N4 1.169 1.106 1.033 2.463 0.734
## N5 1.061 1.004 0.937 0.734 2.635
## 
## $residual
##        N1     N2     N3     N4     N5
## N1  0.000  0.135 -0.068 -0.155 -0.087
## N2  0.135  0.000 -0.069 -0.157 -0.131
## N3 -0.068 -0.069  0.000  0.276  0.177
## N4 -0.155 -0.157  0.276  0.000  0.293
## N5 -0.087 -0.131  0.177  0.293  0.000
## 
## $check
##    N1 N2 N3 N4 N5
## N1  0  0  0  0  0
## N2  0  0  0  0  0
## N3  0  0  0  0  0
## N4  0  0  0  0  0
## N5  0  0  0  0  0
## 
```




* The overved covariance matrix was extracted using the `cov` function on the sample data.
* The fitted covariance matrix can be extracted using the `fitted` method on the model fit object and then extracting the cov
* Many symmetric matrices in lavaan are of class `lavaan.matrix.symmetric`. This hides the upper triangle of the matrix and formats the matrix to `nd` decimal places.
Run `getAnywhere(print.lavaan.matrix.symmetric)` to see more details.
* The `sampstat` option in the `inspect` method can be used to extract the sample covariance matrix. This is similar, but not exactly the same as running `cov` on the sample data.
* The `resid` method can be used to extract the residual covariance matrix
* I then create a `check` that `observed = fitted - residual`, which it does.

## Observed, fitted, and residual correlation matrices
I often find it more meaningful to examine observed, fitted, and residual correlation matrices.  Standardisation often makes it easier to understand the real magnitude of any residual.



```r
N_names <- paste0("N", 1:5)
N_cov <- list(observed = inspect(m1_fit, "sampstat")$cov[N_names, 
    N_names], fitted = fitted(m1_fit)$cov[N_names, N_names])

N_cor <- list(observed = cov2cor(N_cov$observed), fitted = cov2cor(N_cov$fitted))

N_cor$residual <- N_cor$observed - N_cor$fitted

lapply(N_cor, function(X) round(X, 2))
```

```
## $observed
##      N1   N2   N3   N4   N5
## N1 1.00 0.72 0.57 0.41 0.38
## N2 0.72 1.00 0.55 0.39 0.35
## N3 0.57 0.55 1.00 0.52 0.43
## N4 0.41 0.39 0.52 1.00 0.40
## N5 0.38 0.35 0.43 0.40 1.00
## 
## $fitted
##      N1   N2   N3   N4   N5
## N1 1.00 0.66 0.59 0.47 0.41
## N2 0.66 1.00 0.58 0.46 0.40
## N3 0.59 0.58 1.00 0.41 0.36
## N4 0.47 0.46 0.41 1.00 0.29
## N5 0.41 0.40 0.36 0.29 1.00
## 
## $residual
##       N1    N2    N3    N4    N5
## N1  0.00  0.06 -0.03 -0.06 -0.03
## N2  0.06  0.00 -0.03 -0.07 -0.05
## N3 -0.03 -0.03  0.00  0.11  0.07
## N4 -0.06 -0.07  0.11  0.00  0.11
## N5 -0.03 -0.05  0.07  0.11  0.00
## 
```




* `cov2cor` is a `base` R function that scales a covariance matrix into a correlation matrix.
*  Fitted and observed correlation matrices can be obtained by running `cov2cor` on the corresponding covariance matrices.
* The residual correlation matrix can be obtained by subtracting the fitted correlation matrix from the observed correlation matrix.
* In this case we can see that the certain pairs of items correlate more or less than other pairs. In particular `N1-N2`, `N3-N4`, `N4-N5` have positive correlation residuals. An examination of the items below may suggest some added degree of similarity between these pairs of items. For example, N1 and N2 both concern anger and irritation, whereas N3 and N4 both concern mood and affect. 


> N1: Get angry easily. (q_952)
> N2: Get irritated easily. (q_974)
> N3: Have frequent mood swings. (q_1099
> N4: Often feel blue. (q_1479)
> N5: Panic easily. (q_1505)

# Uncorrelated factors
## All Uncorrelated factors
The following examines a mdoel with uncorrelated factors.



```r
m3_model <- ' N =~ N1 + N2 + N3 + N4 + N5
              E =~ E1 + E2 + E3 + E4 + E5
              O =~ O1 + O2 + O3 + O4 + O5
              A =~ A1 + A2 + A3 + A4 + A5
              C =~ C1 + C2 + C3 + C4 + C5
'

m3_fit <- cfa(m3_model, data=Data[, item_names], orthogonal=TRUE)

round(cbind(m1=inspect(m1_fit, 'fit.measures'),
      m3=inspect(m3_fit, 'fit.measures')), 3)
```

```
##                           m1         m3
## chisq               4165.467  5.640e+03
## df                   265.000  2.750e+02
## pvalue                 0.000  0.000e+00
## baseline.chisq     18222.116  1.822e+04
## baseline.df          300.000  3.000e+02
## baseline.pvalue        0.000  0.000e+00
## cfi                    0.782  7.010e-01
## tli                    0.754  6.730e-01
## logl              -99840.238 -1.006e+05
## unrestricted.logl -97757.504 -9.776e+04
## npar                  60.000  5.000e+01
## aic               199800.476  2.013e+05
## bic               200148.363  2.015e+05
## ntotal              2436.000  2.436e+03
## bic2              199957.729  2.014e+05
## rmsea                  0.078  8.900e-02
## rmsea.ci.lower         0.076  8.700e-02
## rmsea.ci.upper         0.080  9.200e-02
## rmsea.pvalue           0.000  0.000e+00
## srmr                   0.075  1.380e-01
```

```r
anova(m1_fit, m3_fit)
```

```
## Chi Square Difference Test
## 
##         Df    AIC    BIC Chisq Chisq diff Df diff Pr(>Chisq)    
## m1_fit 265 199800 200148  4165                                  
## m3_fit 275 201255 201545  5640       1474      10     <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
```

```r

rmsea_m1 <-  round(inspect(m1_fit, 'fit.measures')['rmsea'], 3)
rmsea_m3 <-  round(inspect(m3_fit, 'fit.measures')['rmsea'], 3)
```




* To convert a `cfa` model from one that permits fators to be correlated to one that constrains factors to be uncorrelated, just specify `orthogonal=TRUE`.
* In this case constraining the factor covariances to all be zero led to a significant reduction in fit. This poorer fit can also be seen in measures like RMSEA (m1=
`0.078`; m3 = `0.089` ).


## Correlations and covariances between factors
It is useful to be able to extract correlations and covaraiances between factors.



```r
inspect(m1_fit, "coefficients")$psi
```

```
##   N      E      O      A      C     
## N  1.689                            
## E  0.292  0.846                     
## O -0.093 -0.265  0.404              
## A  0.141  0.304 -0.093  0.234       
## C -0.250 -0.224  0.130 -0.110  0.463
```

```r
cov2cor(inspect(m1_fit, "coefficients")$psi)
```

```
##   N      E      O      A      C     
## N  1.000                            
## E  0.244  1.000                     
## O -0.112 -0.453  1.000              
## A  0.223  0.683 -0.303  1.000       
## C -0.283 -0.357  0.301 -0.334  1.000
```

```r
A_E_r <- cov2cor(inspect(m1_fit, "coefficients")$psi)["A", "E"]
```




* This code first extracts the factor variances and covariances.
* I assume that naming the element `psi` (i.e., $\psi$) is a reference to LISREL Matrix notation (see this discussion from [USP 655 SEM](http://www.upa.pdx.edu/IOA/newsom/semclass/ho_lisrel%20notation.pdf)).
* Once again `cov2cor` is used to convert the covariance matrix to a correlation matrix.
* An inspection of the values shows that there are some substantive correlations that helps to explain why constraining them to zero in an orthogonal model would have substantially damaged fit. For example, the correlation between extraversion (`E`) and agreeableness (`A`) was quite high ($r = 0.68$).




```r
# c('O', 'C', 'E', 'A', 'N') # set of factor names lhs != rhs # excludes
# factor variances
subset(inspect(m1_fit, "standardized"), rhs %in% c("O", "C", "E", 
    "A", "N") & lhs != rhs)
```

```
##    lhs op rhs est.std se  z pvalue
## 1    N ~~   E   0.244 NA NA     NA
## 2    N ~~   O  -0.112 NA NA     NA
## 3    N ~~   A   0.223 NA NA     NA
## 4    N ~~   C  -0.283 NA NA     NA
## 5    E ~~   O  -0.453 NA NA     NA
## 6    E ~~   A   0.683 NA NA     NA
## 7    E ~~   C  -0.357 NA NA     NA
## 8    O ~~   A  -0.303 NA NA     NA
## 9    O ~~   C   0.301 NA NA     NA
## 10   A ~~   C  -0.334 NA NA     NA
```




* The same values can be extracted from the `standardized` coefficients table using the `inspect` method.

We can also confirm that for the orthogonal model (`m3`) the correlations are zero.



```r
cov2cor(inspect(m3_fit, "coefficients")$psi)
```

```
##   N E O A C
## N 1        
## E 0 1      
## O 0 0 1    
## A 0 0 0 1  
## C 0 0 0 0 1
```





# Constrain factor correlations to be equal
## Change constraints so that factor variances are one



```r
m4_model <- ' N =~ N1 + N2 + N3 + N4 + N5
              E =~ E1 + E2 + E3 + E4 + E5
              O =~ O1 + O2 + O3 + O4 + O5
              A =~ A1 + A2 + A3 + A4 + A5
              C =~ C1 + C2 + C3 + C4 + C5
'

m4_fit <- cfa(m4_model, data=Data[, item_names], std.lv=TRUE)

inspect(m4_fit, 'coefficients')$psi
```

```
##   N      E      O      A      C     
## N  1.000                            
## E -0.244  1.000                     
## O -0.112  0.453  1.000              
## A -0.223  0.683  0.303  1.000       
## C -0.283  0.357  0.301  0.334  1.000
```

```r
inspect(m4_fit, 'coefficients')$psi
```

```
##   N      E      O      A      C     
## N  1.000                            
## E -0.244  1.000                     
## O -0.112  0.453  1.000              
## A -0.223  0.683  0.303  1.000       
## C -0.283  0.357  0.301  0.334  1.000
```




* `std.lv` is an argument that when `TRUE` standardises latent variables by fixing their variance to 1.0. The default is `FALSE` which instead constrains the first factor loading to 1.0.
* This makes the covariance and the correlation matrix of the factors the same.

We can see the differences in the loadings by comparing the loadings for the neuroticism factor: 



```r
head(parameterestimates(m4_fit), 5)
```

```
##   lhs op rhs   est    se     z pvalue ci.lower ci.upper
## 1   N =~  N1 1.300 0.028 46.07      0    1.244    1.355
## 2   N =~  N2 1.230 0.028 44.38      0    1.176    1.285
## 3   N =~  N3 1.149 0.030 38.41      0    1.090    1.207
## 4   N =~  N4 0.899 0.031 28.75      0    0.838    0.960
## 5   N =~  N5 0.816 0.033 24.65      0    0.751    0.881
```

```r
head(parameterestimates(m1_fit), 5)
```

```
##   lhs op rhs   est    se     z pvalue ci.lower ci.upper
## 1   N =~  N1 1.000 0.000    NA     NA    1.000    1.000
## 2   N =~  N2 0.947 0.024 39.90      0    0.900    0.993
## 3   N =~  N3 0.884 0.025 35.92      0    0.836    0.932
## 4   N =~  N4 0.692 0.025 27.75      0    0.643    0.741
## 5   N =~  N5 0.628 0.026 24.03      0    0.577    0.679
```

```r

# shows how ratio of loadings has not changed
head(parameterestimates(m4_fit), 5)$est/head(parameterestimates(m4_fit), 
    5)$est[1]
```

```
## [1] 1.0000 0.9467 0.8839 0.6918 0.6278
```






## Add equality constraints


```r
m5_model <- ' N =~ N1 + N2 + N3 + N4 + N5
              E =~ E1 + E2 + E3 + E4 + E5
              O =~ O1 + O2 + O3 + O4 + O5
              A =~ A1 + A2 + A3 + A4 + A5
              C =~ C1 + C2 + C3 + C4 + C5
    N ~~ R*E + R*O + R*A + R*C
    E ~~ R*O + R*A + R*C
    O ~~ R*A + R*C
    A ~~ R*C
'

Data_reversed <- Data
Data_reversed[, paste0('N', 1:5)] <- 7 - Data[, paste0('N', 1:5)]

m5_fit <- cfa(m5_model, data=Data_reversed[, item_names], std.lv=TRUE)
```




* Equality constraints were added by labelling all the covariance parameters with a common label (i.e., `R`). 
* `~~` stands for covariance.
* `R*E` labels the parameter with the `E` variable with the label 
* I reversed the neuroticism items and hence the factor to ensure that all the inter-item correlations were positive.

The following output shows that the correlation/covariance is the same for all factor inter-correlations.



```r
inspect(m5_fit, "coefficients")$psi
```

```
##   N     E     O     A     C    
## N 1.000                        
## E 0.323 1.000                  
## O 0.323 0.323 1.000            
## A 0.323 0.323 0.323 1.000      
## C 0.323 0.323 0.323 0.323 1.000
```




The following analysis compare the fit of the unconstrained with the equal-covariance model.



```r
round(cbind(m1 = inspect(m1_fit, "fit.measures"), m5 = inspect(m5_fit, 
    "fit.measures")), 3)
```

```
##                           m1         m5
## chisq               4165.467  4.576e+03
## df                   265.000  2.740e+02
## pvalue                 0.000  0.000e+00
## baseline.chisq     18222.116  1.822e+04
## baseline.df          300.000  3.000e+02
## baseline.pvalue        0.000  0.000e+00
## cfi                    0.782  7.600e-01
## tli                    0.754  7.370e-01
## logl              -99840.238 -1.000e+05
## unrestricted.logl -97757.504 -9.776e+04
## npar                  60.000  5.100e+01
## aic               199800.476  2.002e+05
## bic               200148.363  2.005e+05
## ntotal              2436.000  2.436e+03
## bic2              199957.729  2.003e+05
## rmsea                  0.078  8.000e-02
## rmsea.ci.lower         0.076  7.800e-02
## rmsea.ci.upper         0.080  8.200e-02
## rmsea.pvalue           0.000  0.000e+00
## srmr                   0.075  8.900e-02
```

```r
anova(m1_fit, m5_fit)
```

```
## Chi Square Difference Test
## 
##         Df   AIC   BIC Chisq Chisq diff Df diff Pr(>Chisq)    
## m1_fit 265 2e+05 2e+05  4165                                  
## m5_fit 274 2e+05 2e+05  4576        411       9     <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
```




* The unconstrained model provides a better fit both in terms of the chi-square difference test and when comparing various parisomony adjusted fit indices such as RMSEA. 
* The difference is relatively small.

The following summarises the correlations between variables (correlations with Neuroticism reversed).



```r
rs <- abs(inspect(m4_fit, "coefficients")$psi)
summary(rs[lower.tri(rs)])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.112   0.254   0.302   0.329   0.352   0.683 
```

```r
hist(rs[lower.tri(rs)])
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19.png) 

```r

round(rs, 2)
```

```
##   N    E    O    A    C   
## N 1.00                    
## E 0.24 1.00               
## O 0.11 0.45 1.00          
## A 0.22 0.68 0.30 1.00     
## C 0.28 0.36 0.30 0.33 1.00
```




* Given the very large sample size, even small variations in sample correlations likely reflect true variation.
* However, in particular, the correlation between E and A is much larger than the average correlation, and the correlation between O and N is much smaller than the average correlation.

## Add equality constraints with some post hoc modifications


```r
m6_model <- ' N =~ N1 + N2 + N3 + N4 + N5
              E =~ E1 + E2 + E3 + E4 + E5
              O =~ O1 + O2 + O3 + O4 + O5
              A =~ A1 + A2 + A3 + A4 + A5
              C =~ C1 + C2 + C3 + C4 + C5
    N ~~ R*E + R*A + R*C
    E ~~ R*O + R*C
    O ~~ R*A + R*C
    A ~~ R*C
'

Data_reversed <- Data
Data_reversed[, paste0('N', 1:5)] <- 7 - Data[, paste0('N', 1:5)]

m6_fit <- cfa(m6_model, data=Data_reversed[, item_names], std.lv=TRUE)
```




The above model frees to up the correlation between E and A, and between O and N.



```r
round(cbind(m1 = inspect(m1_fit, "fit.measures"), m5 = inspect(m1_fit, 
    "fit.measures"), m6 = inspect(m6_fit, "fit.measures")), 3)
```

```
##                           m1         m5         m6
## chisq               4165.467   4165.467   4223.250
## df                   265.000    265.000    272.000
## pvalue                 0.000      0.000      0.000
## baseline.chisq     18222.116  18222.116  18222.116
## baseline.df          300.000    300.000    300.000
## baseline.pvalue        0.000      0.000      0.000
## cfi                    0.782      0.782      0.780
## tli                    0.754      0.754      0.757
## logl              -99840.238 -99840.238 -99869.130
## unrestricted.logl -97757.504 -97757.504 -97757.504
## npar                  60.000     60.000     53.000
## aic               199800.476 199800.476 199844.259
## bic               200148.363 200148.363 200151.559
## ntotal              2436.000   2436.000   2436.000
## bic2              199957.729 199957.729 199983.166
## rmsea                  0.078      0.078      0.077
## rmsea.ci.lower         0.076      0.076      0.075
## rmsea.ci.upper         0.080      0.080      0.079
## rmsea.pvalue           0.000      0.000      0.000
## srmr                   0.075      0.075      0.077
```

```r
anova(m1_fit, m6_fit)
```

```
## Chi Square Difference Test
## 
##         Df   AIC   BIC Chisq Chisq diff Df diff Pr(>Chisq)    
## m1_fit 265 2e+05 2e+05  4165                                  
## m6_fit 272 2e+05 2e+05  4223       57.8       7    4.2e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
```

```r
anova(m5_fit, m6_fit)
```

```
## Chi Square Difference Test
## 
##         Df   AIC   BIC Chisq Chisq diff Df diff Pr(>Chisq)    
## m6_fit 272 2e+05 2e+05  4223                                  
## m5_fit 274 2e+05 2e+05  4576        353       2     <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
```




* Freeing up these two correlations improved the model relative to the equality model. By most fit statistics, this model still provided a worse fit than the unconstrained model. However, interestingly, the RMSEA was slightly lower (i.e., better).
