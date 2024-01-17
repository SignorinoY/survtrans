
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
## basic example code
formula <- Surv(time, status) ~ . - group - id
group <- as.factor(sim2$group)
fit <- coxtrans(
  formula, sim2, group, group_target = 10,
  lambda1 = 0.05, lambda2 = 0.06, penalty = "SCAD"
)
round(fit$beta, 3)
#>    X1    X2    X3    X4    X5    X6    X7    X8    X9   X10 
#> 1.117 1.309 1.047 0.987 1.065 1.155 1.035 1.103 1.018 1.099
round(fit$eta, 3)
#>         [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]   [,8]   [,9]
#>  [1,]  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
#>  [2,] -2.419 -0.437 -2.419 -0.437 -2.419 -0.437 -2.419 -0.437 -2.419
#>  [3,]  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
#>  [4,] -2.003  0.000 -2.003  0.000 -2.003  0.000 -2.003  0.000 -2.003
#>  [5,]  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
#>  [6,] -2.323 -0.022 -2.323 -0.022 -2.323 -0.022 -2.323 -0.029 -2.323
#>  [7,]  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
#>  [8,] -2.181  0.000 -2.181  0.000 -2.181  0.000 -2.181  0.000 -2.181
#>  [9,]  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000
#> [10,] -2.067  0.000 -2.067  0.000 -2.067  0.000 -2.067  0.000 -2.067
```

## Code of Conduct

Please note that the survtrans project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
