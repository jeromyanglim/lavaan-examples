# Assumptions
* `fit` is an object of class `lavaan` typically returned from functions `cfa`, `sem`, `growth`, and `lavaan`
* `m1_fit` and `m2_fit` are used for showing model comparison of `lavaan` objects.



# Commands


# Matrices
| Name | Command |
-------| -------------
Factor covariance matrix | `inspect(fit, "coefficients")$psi`
Fitted covariance matrix | `fitted(fit)$cov`
Observed covariance matrix | `inspect(fit, 'sampstat')$cov`
Residual covariance matrix | `resid(fit)$cov`
Factor correlation matrix | `cov2cor(inspect(fit, "coefficients")$psi)` or use covariance command with standardised solution e.g.,  `cfa(..., std.ov=TRUE)` 
# Fit Measures
| Name | Command |
-------| -------------
Fit measures: | `fitMeasures(fit)` 
Specific fit measures e.g.: | `fitMeasures(fit)[c('chisq', 'df', 'pvalue', 'cfi', 'rmsea', 'srmr')]`


# Parameters
| Name | Command |
-------| -------------
Parameter information | `parTable(fit)`
Standardised estimates | `standardizedSolution(fit)` or `summary(fit, standardized=TRUE)`



# Compare models

| Name | Command |
-------| -------------
Compare fit measures | `cbind(m1=inspect(m1_fit, 'fit.measures'), m2=inspect(m2_fit, 'fit.measures'))`
Chi-square difference test | `anova(m1_fit, m2_fit)`

# Model improvement
| Name | Command |
-------| -------------
Modification indices | `mod_ind <- modificationindices(fit)`
10 greatest | `head(mod_ind[order(mod_ind$mi, decreasing=TRUE), ], 10)`
mi > 5 | `subset(mod_ind[order(mod_ind$mi, decreasing=TRUE), ], mi > 5)`

