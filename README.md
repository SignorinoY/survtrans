
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Survtrans

<!-- badges: start -->

[![R-CMD-check](https://github.com/SignorinoY/survtrans/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SignorinoY/survtrans/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/SignorinoY/survode/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SignorinoY/survtrans?branch=main)
<!-- badges: end -->

The goal of survtrans is to …

## Installation

You can install the development version of survtrans from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SignorinoY/survtrans")
```

## Example

This is a basic example which shows you how to solve a common problem:

### Multi-Source Survival Analysis

``` r
library(survtrans)
#> Loading required package: progress
#> Loading required package: survode
#> Loading required package: survival
```

``` r

formula <- Surv(time, status) ~ . - group - id
group <- as.factor(sim2$group)

# Target learning
fit <- ncvcox(formula, sim2[group == 10, ], lambda = 0.15, penalty = "SCAD")
fit$coefficients
#>         X1         X2         X3         X4         X5         X6         X7 
#> -0.6712921 -0.5785810  0.3196782  0.8471824  0.0000000  0.0000000  0.0000000 
#>         X8         X9        X10        X11        X12        X13        X14 
#>  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000 
#>        X15        X16        X17        X18        X19        X20 
#>  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
```

``` r

# Group learning
fit <- coxgrp(formula, sim2, group, lambda = 0.02, penalty = "SCAD")
fit$coefficients[, 10]
#>         X1         X2         X3         X4         X5         X6         X7 
#> -0.6895172 -0.6184747  0.4903296  0.9018439  0.0000000  0.0000000  0.0000000 
#>         X8         X9        X10        X11        X12        X13        X14 
#>  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000 
#>        X15        X16        X17        X18        X19        X20 
#>  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
```

``` r

# Sub-group learning
fit <- coxsg(
  formula, sim2, group,
  lambda1 = 0.07, lambda2 = 0.05, penalty = "SCAD"
)
fit$coefficients[, 10]
#>         X1         X2         X3         X4         X5         X6         X7 
#> -0.6600533 -0.6405885  0.5567368  0.5539575  0.0000000  0.0000000  0.0000000 
#>         X8         X9        X10        X11        X12        X13        X14 
#>  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000 
#>        X15        X16        X17        X18        X19        X20 
#>  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
```

``` r

# Transfer learning
fit <- coxtl(
  formula, sim2, group, 10,
  lambda1 = 0.04, lambda2 = 0.11, penalty = "SCAD"
)
fit$beta
#>          X1          X2          X3          X4          X5          X6 
#> -0.67568489 -0.65782685  0.56804805  0.57814325  0.00000000  0.00000000 
#>          X7          X8          X9         X10         X11         X12 
#>  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000 
#>         X13         X14         X15         X16         X17         X18 
#>  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000  0.00000000 
#>         X19         X20 
#>  0.00000000 -0.03663008
```

``` r

# Ensemble learning
fit <- coxens(
  formula, sim2, group,
  lambda1 = 0.02, lambda2 = 0.005, penalty = "SCAD"
)
fit$coefficients[, 10]
#>         X1         X2         X3         X4         X5         X6         X7 
#> -0.6605156 -0.6196182  0.5558083  0.5547476  0.0000000  0.0000000  0.0000000 
#>         X8         X9        X10        X11        X12        X13        X14 
#>  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000 
#>        X15        X16        X17        X18        X19        X20 
#>  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
```

``` r

# Multi-task learning
fit <- coxmtl(
  formula, sim2, group,
  lambda1 = 0.06, lambda2 = 0.06, lambda3 = 0.04, penalty = "SCAD"
)
fit$beta + fit$eta[, 10]
#>         X1         X2         X3         X4         X5         X6         X7 
#> -0.5915504 -0.5672794  0.5561607  0.5477745  0.0000000  0.0000000  0.0000000 
#>         X8         X9        X10        X11        X12        X13        X14 
#>  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000 
#>        X15        X16        X17        X18        X19        X20 
#>  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
```

## Code of Conduct

Please note that the survtrans project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
