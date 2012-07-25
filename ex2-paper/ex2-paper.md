
# Example 2 from Rossel's Paper on lavaan


```r
library(lavaan)
Data <- PoliticalDemocracy
```




This example is an elaboration on Example 2 from Yves Rossel's Journal of Statistical Software Article (see [here](http://www.jstatsoft.org/v48/i02/paper)).

# Basic SEM


```r
m0_model <- '
# measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
'

m0_fit <- cfa(m0_model, data=Data)
```




* `m0` defines a basic measurement model that permits correlated factors.  Note that it does not have correlations between corresponding democracy indicator measures over time.

**Questions:**

* Is it a good model?



```r
fitmeasures(m0_fit)
```

```
##             chisq                df            pvalue    baseline.chisq 
##            72.462            41.000             0.002           730.654 
##       baseline.df   baseline.pvalue               cfi               tli 
##            55.000             0.000             0.953             0.938 
##              logl unrestricted.logl              npar               aic 
##         -1564.959         -1528.728            25.000          3179.918 
##               bic            ntotal              bic2             rmsea 
##          3237.855            75.000          3159.062             0.101 
##    rmsea.ci.lower    rmsea.ci.upper      rmsea.pvalue              srmr 
##             0.061             0.139             0.021             0.055 
```




* cfi suggests a reasonable model, but RMSEA is quite large.



```r
inspect(m0_fit, 'standardized')
```

```
##      lhs op   rhs est.std se  z pvalue
## 1  ind60 =~    x1   0.920 NA NA     NA
## 2  ind60 =~    x2   0.973 NA NA     NA
## 3  ind60 =~    x3   0.872 NA NA     NA
## 4  dem60 =~    y1   0.845 NA NA     NA
## 5  dem60 =~    y2   0.760 NA NA     NA
## 6  dem60 =~    y3   0.705 NA NA     NA
## 7  dem60 =~    y4   0.860 NA NA     NA
## 8  dem65 =~    y5   0.803 NA NA     NA
## 9  dem65 =~    y6   0.783 NA NA     NA
## 10 dem65 =~    y7   0.819 NA NA     NA
## 11 dem65 =~    y8   0.847 NA NA     NA
## 12    x1 ~~    x1   0.154 NA NA     NA
## 13    x2 ~~    x2   0.053 NA NA     NA
## 14    x3 ~~    x3   0.240 NA NA     NA
## 15    y1 ~~    y1   0.286 NA NA     NA
## 16    y2 ~~    y2   0.422 NA NA     NA
## 17    y3 ~~    y3   0.503 NA NA     NA
## 18    y4 ~~    y4   0.261 NA NA     NA
## 19    y5 ~~    y5   0.355 NA NA     NA
## 20    y6 ~~    y6   0.387 NA NA     NA
## 21    y7 ~~    y7   0.329 NA NA     NA
## 22    y8 ~~    y8   0.283 NA NA     NA
## 23 ind60 ~~ ind60   1.000 NA NA     NA
## 24 dem60 ~~ dem60   1.000 NA NA     NA
## 25 dem65 ~~ dem65   1.000 NA NA     NA
## 26 ind60 ~~ dem60   0.448 NA NA     NA
## 27 ind60 ~~ dem65   0.555 NA NA     NA
## 28 dem60 ~~ dem65   0.978 NA NA     NA
```




* The table of standardised loadings show all factor loadings to be large.



```r
m0_mod <- modificationindices(m0_fit)
head(m0_mod[order(m0_mod$mi, decreasing=TRUE), ], 12)
```

```
##      lhs op rhs    mi    epc sepc.lv sepc.all sepc.nox
## 1     y2 ~~  y6 9.279  2.129   2.129    0.162    0.162
## 2     y6 ~~  y8 8.668  1.513   1.513    0.140    0.140
## 3     y1 ~~  y5 8.183  0.884   0.884    0.131    0.131
## 4     y3 ~~  y6 6.574 -1.590  -1.590   -0.146   -0.146
## 5     y1 ~~  y3 5.204  1.024   1.024    0.121    0.121
## 6     y2 ~~  y4 4.911  1.432   1.432    0.110    0.110
## 7     y3 ~~  y7 4.088  1.152   1.152    0.108    0.108
## 8  ind60 =~  y5 4.007  0.762   0.510    0.197    0.197
## 9     x1 ~~  y2 3.785 -0.192  -0.192   -0.067   -0.067
## 10 ind60 =~  y4 3.568  0.811   0.543    0.163    0.163
## 11    y2 ~~  y3 3.215 -1.365  -1.365   -0.107   -0.107
## 12    y5 ~~  y6 3.116 -0.774  -0.774   -0.089   -0.089
```




* The table of largest modification indices suggest a range of ways that the model could be improved. Because the sample size is small, particular caution needs to be taken with these.
* Several of these modifications concern the expected requirement to permit indicator variables at different time points to correlate (e.g., `y2` with `y6`, `y3` with `y7`).
* It may also be that some pairs of items are correlated more than others. For example, the following correlation matrix shows how `y6` and `y8` have a particularly large correlation.



```r
round(cor(Data[,c('y5', 'y6', 'y7', 'y8')]), 2)
```

```
##      y5   y6   y7   y8
## y5 1.00 0.56 0.68 0.63
## y6 0.56 1.00 0.61 0.75
## y7 0.68 0.61 1.00 0.71
## y8 0.63 0.75 0.71 1.00
```





* What are the correlations between the factors?



```r
cov2cor(inspect(m0_fit, "coefficients")$psi)
```

```
##       ind60 dem60 dem65
## ind60 1.000            
## dem60 0.448 1.000      
## dem65 0.555 0.978 1.000
```







```r
m1_model <- '
# measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8

# regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60
'

m1_fit <- sem(m1_model, data=Data)
```







```r
#summary(m1_fit)#inspect(m1_fit, 'standardized')#inspect(m1_fit, 'r2')[c('dem60', 'dem65')]
```





