
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
fit <- coxtrans(
  formula, sim2, as.factor(sim2$group),
  lambda1 = 0.05, lambda2 = 0.04, penalty = "SCAD"
)
fit$beta
#>           X1           X2           X3           X4           X5           X6 
#>  1.136072815 -0.078964002  1.064436383 -0.023869177  1.081124608 -0.019824684 
#>           X7           X8           X9          X10 
#>  1.042391144  0.004917786  1.026545052  0.069437296
fit$eta
#>          [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
#> X1   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X2  -1.046848 0.9317925 -1.046848 0.9591964 -1.046848 0.9591964 -1.046848
#> X3   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X4  -1.005511 1.0055114 -1.005511 1.0055114 -1.005511 1.0055114 -1.005511
#> X5   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X6  -1.161986 1.2387476 -1.161986 1.2387476 -1.161986 1.2387476 -1.161986
#> X7   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X8  -1.096769 1.0967686 -1.096769 1.0967686 -1.096769 1.0967686 -1.096769
#> X9   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X10 -1.052338 1.0523384 -1.052338 1.0523384 -1.052338 1.0523384 -1.052338
#>          [,8]      [,9]    [,10]
#> X1  0.0000000  0.000000 0.000000
#> X2  0.9591964 -1.046848 1.424857
#> X3  0.0000000  0.000000 0.000000
#> X4  1.0055114 -1.005511 1.005511
#> X5  0.0000000  0.000000 0.000000
#> X6  0.8549381 -1.161986 1.238748
#> X7  0.0000000  0.000000 0.000000
#> X8  1.0967686 -1.096769 1.096769
#> X9  0.0000000  0.000000 0.000000
#> X10 1.0523384 -1.052338 1.052338
fit$eta_group
#>       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#>  [1,]    1    1    1    1    1    1    1    1    1     1
#>  [2,]    1    2    1    4    1    4    1    4    1    10
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
