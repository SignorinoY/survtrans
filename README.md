
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
#> 1.1485819 0.9914212 1.0675040 0.9912339 1.0577806 1.1452113 1.0443883 1.1246573 
#>        X9       X10 
#> 1.0124941 1.1112788
fit$eta
#>          [,1]       [,2]      [,3] [,4]      [,5] [,6]      [,7]        [,8]
#> X1   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000  0.00000000
#> X2  -2.193076 -0.1065746 -2.221245    0 -2.081839    0 -1.947329  0.00000000
#> X3   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000  0.00000000
#> X4  -2.146633  0.0000000 -2.029066    0 -1.937629    0 -1.775079  0.00000000
#> X5   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000  0.00000000
#> X6  -2.275226  0.0000000 -2.262730    0 -2.378676    0 -2.262433 -0.10226388
#> X7   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000 -0.01420525
#> X8  -2.313740  0.0000000 -2.203845    0 -1.980452    0 -2.204240  0.00000000
#> X9   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000  0.00000000
#> X10 -1.880444  0.0000000 -2.201272    0 -2.176774    0 -1.891838  0.00000000
#>          [,9]
#> X1   0.000000
#> X2  -2.168322
#> X3   0.000000
#> X4  -2.110493
#> X5   0.000000
#> X6  -2.335686
#> X7   0.000000
#> X8  -2.316359
#> X9   0.000000
#> X10 -2.195146

## Multi-task learning
fit <- coxtrans(
  formula, sim2, group,
  lambda1 = 0.05, lambda2 = 0.05, penalty = "SCAD"
)
fit$beta
#>          X1          X2          X3          X4          X5          X6 
#>  1.12863917 -0.07776493  1.04677493 -0.01529523  1.06863495 -0.02513361 
#>          X7          X8          X9         X10 
#>  1.03489198  0.01074090  1.02218081  0.06640528
fit$eta
#>          [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
#> X1   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X2  -1.038925 0.9482405 -1.038925 0.9482405 -1.038925 0.9482405 -1.038925
#> X3   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X4  -1.005923 1.0059231 -1.005923 1.0059231 -1.005923 1.0059231 -1.005923
#> X5   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X6  -1.146744 1.1544717 -1.146744 1.1544717 -1.146744 1.1544717 -1.146744
#> X7   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X8  -1.094265 1.0942653 -1.094265 1.0942653 -1.094265 1.0942653 -1.094265
#> X9   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X10 -1.039526 1.0395256 -1.039526 1.0395256 -1.039526 1.0395256 -1.039526
#>          [,8]      [,9]    [,10]
#> X1  0.0000000  0.000000 0.000000
#> X2  0.9482405 -1.038925 1.401665
#> X3  0.0000000  0.000000 0.000000
#> X4  1.0059231 -1.005923 1.005923
#> X5  0.0000000  0.000000 0.000000
#> X6  1.1158350 -1.146744 1.154472
#> X7  0.0000000  0.000000 0.000000
#> X8  1.0942653 -1.094265 1.094265
#> X9  0.0000000  0.000000 0.000000
#> X10 1.0395256 -1.039526 1.039526
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
