
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

``` r
library(survtrans)
#> 载入需要的程辑包：progress
#> 载入需要的程辑包：survode
#> 载入需要的程辑包：survival

formula <- Surv(time, status) ~ . - group - id
group <- as.factor(sim2$group)

## Transfer learning
fit <- coxtl(
  formula, sim2,
  group, target = 10, lambda = 0.15, penalty = "SCAD"
)
fit$beta
#>        X1        X2        X3        X4        X5        X6        X7        X8 
#> 1.1485820 0.9914191 1.0675023 0.9912416 1.0577803 1.1452103 1.0443883 1.1246563 
#>        X9       X10 
#> 1.0124952 1.1112742
fit$eta
#>          [,1]       [,2]      [,3] [,4]      [,5] [,6]      [,7]        [,8]
#> X1   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000  0.00000000
#> X2  -2.193078 -0.1065735 -2.221247    0 -2.081851    0 -1.947312  0.00000000
#> X3   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000  0.00000000
#> X4  -2.146608  0.0000000 -2.029079    0 -1.937611    0 -1.775093  0.00000000
#> X5   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000  0.00000000
#> X6  -2.275227  0.0000000 -2.262729    0 -2.378666    0 -2.262440 -0.10226070
#> X7   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000 -0.01420761
#> X8  -2.313742  0.0000000 -2.203844    0 -1.980434    0 -2.204245  0.00000000
#> X9   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000  0.00000000
#> X10 -1.880442  0.0000000 -2.201266    0 -2.176744    0 -1.891832  0.00000000
#>          [,9]
#> X1   0.000000
#> X2  -2.168296
#> X3   0.000000
#> X4  -2.110524
#> X5   0.000000
#> X6  -2.335676
#> X7   0.000000
#> X8  -2.316369
#> X9   0.000000
#> X10 -2.195129

## Multi-task learning
fit <- coxmtl(
  formula, sim2, group,
  lambda1 = 0.05, lambda2 = 0.05, penalty = "SCAD"
)
fit$beta
#>          X1          X2          X3          X4          X5          X6 
#>  1.12869930 -0.07777005  1.04683081 -0.01530216  1.06869556 -0.02513485 
#>          X7          X8          X9         X10 
#>  1.03494954  0.01074179  1.02224076  0.06640373
fit$eta
#>          [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
#> X1   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X2  -1.038997 0.9483055 -1.038997 0.9483055 -1.038997 0.9483055 -1.038997
#> X3   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X4  -1.005997 1.0059971 -1.005997 1.0059971 -1.005997 1.0059971 -1.005997
#> X5   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X6  -1.146825 1.1545552 -1.146825 1.1545552 -1.146825 1.1545552 -1.146825
#> X7   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X8  -1.094335 1.0943348 -1.094335 1.0943348 -1.094335 1.0943348 -1.094335
#> X9   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X10 -1.039596 1.0395961 -1.039596 1.0395961 -1.039596 1.0395961 -1.039596
#>          [,8]      [,9]    [,10]
#> X1  0.0000000  0.000000 0.000000
#> X2  0.9483055 -1.038997 1.401761
#> X3  0.0000000  0.000000 0.000000
#> X4  1.0059971 -1.005997 1.005997
#> X5  0.0000000  0.000000 0.000000
#> X6  1.1159049 -1.146825 1.154555
#> X7  0.0000000  0.000000 0.000000
#> X8  1.0943348 -1.094335 1.094335
#> X9  0.0000000  0.000000 0.000000
#> X10 1.0395961 -1.039596 1.039596
fit$eta_group
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#>  [1,]    1    1    1    1    1    1    1    1    1     1
#>  [2,]    1    2    1    2    1    2    1    2    1    10
#>  [3,]    1    1    1    1    1    1    1    1    1     1
#>  [4,]    1    2    1    2    1    2    1    2    1     2
#>  [5,]    1    1    1    1    1    1    1    1    1     1
#>  [6,]    1    2    1    2    1    2    1    8    1     2
#>  [7,]    1    1    1    1    1    1    1    1    1     1
#>  [8,]    1    2    1    2    1    2    1    2    1     2
#>  [9,]    1    1    1    1    1    1    1    1    1     1
#> [10,]    1    2    1    2    1    2    1    2    1     2
```

## Code of Conduct

Please note that the survtrans project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
