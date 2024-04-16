
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
#> 1.1486489 0.9915027 1.0675651 0.9913045 1.0578434 1.1453034 1.0444529 1.1247399 
#>        X9       X10 
#> 1.0125562 1.1113500
fit$eta
#>          [,1]       [,2]      [,3] [,4]      [,5] [,6]      [,7]        [,8]
#> X1   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000  0.00000000
#> X2  -2.193221 -0.1066108 -2.221407    0 -2.081978    0 -1.947448  0.00000000
#> X3   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000  0.00000000
#> X4  -2.146755  0.0000000 -2.029180    0 -1.937742    0 -1.775180  0.00000000
#> X5   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000  0.00000000
#> X6  -2.275381  0.0000000 -2.262884    0 -2.378828    0 -2.262594 -0.10228831
#> X7   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000 -0.01422607
#> X8  -2.313881  0.0000000 -2.203983    0 -1.980547    0 -2.204376  0.00000000
#> X9   0.000000  0.0000000  0.000000    0  0.000000    0  0.000000  0.00000000
#> X10 -1.880562  0.0000000 -2.201407    0 -2.176891    0 -1.891947  0.00000000
#>          [,9]
#> X1   0.000000
#> X2  -2.168445
#> X3   0.000000
#> X4  -2.110634
#> X5   0.000000
#> X6  -2.335862
#> X7   0.000000
#> X8  -2.316495
#> X9   0.000000
#> X10 -2.195261

## Multi-task learning
fit <- coxmtl(
  formula, sim2, group,
  lambda1 = 0.05, lambda2 = 0.05, penalty = "SCAD"
)
fit$beta
#>          X1          X2          X3          X4          X5          X6 
#>  1.12863892 -0.07776499  1.04677451 -0.01529526  1.06863484 -0.02513366 
#>          X7          X8          X9         X10 
#>  1.03489194  0.01074095  1.02218088  0.06640524
fit$eta
#>          [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
#> X1   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X2  -1.038925 0.9482406 -1.038925 0.9482406 -1.038925 0.9482406 -1.038925
#> X3   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X4  -1.005925 1.0059247 -1.005925 1.0059247 -1.005925 1.0059247 -1.005925
#> X5   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X6  -1.146744 1.1544706 -1.146744 1.1544706 -1.146744 1.1544706 -1.146744
#> X7   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X8  -1.094264 1.0942642 -1.094264 1.0942642 -1.094264 1.0942642 -1.094264
#> X9   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X10 -1.039525 1.0395246 -1.039525 1.0395246 -1.039525 1.0395246 -1.039525
#>          [,8]      [,9]    [,10]
#> X1  0.0000000  0.000000 0.000000
#> X2  0.9482406 -1.038925 1.401665
#> X3  0.0000000  0.000000 0.000000
#> X4  1.0059247 -1.005925 1.005925
#> X5  0.0000000  0.000000 0.000000
#> X6  1.1158391 -1.146744 1.154471
#> X7  0.0000000  0.000000 0.000000
#> X8  1.0942642 -1.094264 1.094264
#> X9  0.0000000  0.000000 0.000000
#> X10 1.0395246 -1.039525 1.039525
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
