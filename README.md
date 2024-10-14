
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
  lambda1 = 0.03, lambda2 = 0.02, lambda3 = 0.01, penalty = "SCAD"
)
summary(fit)
#> Call:
#> coxens(formula = formula, data = sim2, group = sim2$group, lambda1 = 0.03, 
#>     lambda2 = 0.02, lambda3 = 0.01, penalty = "SCAD")
#> 
#>   n=500, number of events=422
#> 
#>               coef exp(coef) se(coef)      z Pr(>|z|)    
#> X1 (1)     0.34902   1.41768  0.05071  6.882 5.90e-12 ***
#> X1 (2, 4)  0.95264   2.59256  0.08481 11.233  < 2e-16 ***
#> X1 (3, 5) -0.25459   0.77523  0.06815 -3.736 0.000187 ***
#> X2 (1)     0.36174   1.43583  0.05388  6.713 1.90e-11 ***
#> X2 (2, 4)  0.96899   2.63527  0.08451 11.466  < 2e-16 ***
#> X2 (3, 5) -0.24550   0.78231  0.08081 -3.038 0.002381 ** 
#> X3 (ALL)   0.34540   1.41255  0.05741  6.016 1.79e-09 ***
#> X4 (ALL)   0.32844   1.38880  0.05677  5.785 7.25e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>           exp(coef) exp(-coef) lower .95 upper .95
#> X1 (1)    1.4177    0.7054     1.2835    1.5658   
#> X1 (2, 4) 2.5926    0.3857     2.1955    3.0614   
#> X1 (3, 5) 0.7752    1.2899     0.6783    0.8860   
#> X2 (1)    1.4358    0.6965     1.2919    1.5958   
#> X2 (2, 4) 2.6353    0.3795     2.2330    3.1100   
#> X2 (3, 5) 0.7823    1.2783     0.6677    0.9166   
#> X3 (ALL)  1.4126    0.7079     1.2622    1.5808   
#> X4 (ALL)  1.3888    0.7200     1.2425    1.5523
```

## Code of Conduct

Please note that the survtrans project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
