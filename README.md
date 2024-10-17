
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Survtrans

<!-- badges: start -->

[![R-CMD-check](https://github.com/SignorinoY/survtrans/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SignorinoY/survtrans/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/SignorinoY/survtrans/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SignorinoY/survtrans?branch=main)
<!-- badges: end -->

The goal of survtrans is to analyze multi-source survival data with
heterogeneous effects. This package provides a set of functions that
utilize other data sources to enhance the estimation of the model for
the target source.

## Installation

You can install the development version of survtrans from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SignorinoY/survtrans")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(survtrans)
#> Loading required package: survival
```

### Multi-Sources Survival Analysis

``` r
formula <- Surv(time, status) ~ . - group - id
```

``` r
fit <- coxens(
  formula, sim2, sim2$group,
  lambda1 = 0.03, lambda2 = 0.04, lambda3 = 0.01, penalty = "SCAD"
)
summary(fit)
#> Call:
#> coxens(formula = formula, data = sim2, group = sim2$group, lambda1 = 0.03, 
#>     lambda2 = 0.04, lambda3 = 0.01, penalty = "SCAD")
#> 
#>   n=500, number of events=422
#> 
#>               coef exp(coef) se(coef)      z Pr(>|z|)    
#> X1 (1)     0.34784   1.41600  0.05068  6.863 6.74e-12 ***
#> X1 (2, 4)  0.94793   2.58035  0.08471 11.190  < 2e-16 ***
#> X1 (3, 5) -0.25256   0.77681  0.06816 -3.705 0.000211 ***
#> X2 (1)     0.36088   1.43459  0.05384  6.702 2.05e-11 ***
#> X2 (2, 4)  0.96456   2.62362  0.08445 11.421  < 2e-16 ***
#> X2 (3, 5) -0.24282   0.78441  0.08072 -3.008 0.002627 ** 
#> X3 (ALL)   0.34498   1.41196  0.05740  6.010 1.86e-09 ***
#> X4 (ALL)   0.32757   1.38760  0.05674  5.773 7.79e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>           exp(coef) exp(-coef) lower .95 upper .95
#> X1 (1)    1.4160    0.7062     1.2821    1.5639   
#> X1 (2, 4) 2.5804    0.3875     2.1856    3.0464   
#> X1 (3, 5) 0.7768    1.2873     0.6797    0.8878   
#> X2 (1)    1.4346    0.6971     1.2909    1.5943   
#> X2 (2, 4) 2.6236    0.3812     2.2234    3.0959   
#> X2 (3, 5) 0.7844    1.2748     0.6696    0.9189   
#> X3 (ALL)  1.4120    0.7082     1.2617    1.5801   
#> X4 (ALL)  1.3876    0.7207     1.2415    1.5508
```

## Code of Conduct

Please note that the survtrans project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
