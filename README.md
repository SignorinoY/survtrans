
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
  formula, sim2, group,
  lambda1 = 1.5, lambda2 = 2.3, penalty = "SCAD"
)
fit$beta
#>           X1           X2           X3           X4           X5           X6 
#>  1.068926330 -0.070782625  0.999758363 -0.010462933  1.013309017 -0.015555871 
#>           X7           X8           X9          X10 
#>  0.976674175  0.007754945  0.956393854  0.071373260
fit$eta
#>             [,1]      [,2]       [,3]      [,4]       [,5]      [,6]       [,7]
#>  [1,]  0.0000000 0.0000000  0.0000000 0.0000000  0.0000000 0.0000000  0.0000000
#>  [2,] -0.9675488 0.7243923 -0.9675488 0.9298314 -0.9675488 0.9298314 -0.9675488
#>  [3,]  0.0000000 0.0000000  0.0000000 0.0000000  0.0000000 0.0000000  0.0000000
#>  [4,] -0.9222461 0.9222461 -0.9222461 0.9222461 -0.9222461 0.9222461 -0.9222461
#>  [5,]  0.0000000 0.0000000  0.0000000 0.0000000  0.0000000 0.0000000  0.0000000
#>  [6,] -1.0701280 1.1300472 -1.0701280 1.1554052 -1.0701280 1.1125420 -1.0701280
#>  [7,]  0.0000000 0.0000000  0.0000000 0.0000000  0.0000000 0.0000000  0.0000000
#>  [8,] -1.0194940 1.0167989 -1.0194940 1.0167989 -1.0060182 1.0167989 -1.0194940
#>  [9,]  0.0000000 0.0000000  0.0000000 0.0000000  0.0000000 0.0000000  0.0000000
#> [10,] -0.9361251 0.9677459 -0.9875176 0.9677459 -0.9875176 0.9677459 -0.9400515
#>            [,8]       [,9]     [,10]
#>  [1,] 0.0000000  0.0000000 0.0000000
#>  [2,] 0.9298314 -0.9675488 1.3238577
#>  [3,] 0.0000000  0.0000000 0.0000000
#>  [4,] 0.9222461 -0.9222461 0.9222461
#>  [5,] 0.0000000  0.0000000 0.0000000
#>  [6,] 0.7785170 -1.0701280 1.1741286
#>  [7,] 0.0000000  0.0000000 0.0000000
#>  [8,] 1.0167989 -1.0194940 1.0167989
#>  [9,] 0.0000000  0.0000000 0.0000000
#> [10,] 0.9677459 -0.9875176 0.9677459
```

## Code of Conduct

Please note that the survtrans project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
