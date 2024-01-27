
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
