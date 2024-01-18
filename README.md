
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
#>  1.136068531 -0.078963659  1.064432409 -0.023868649  1.081120313 -0.019824581 
#>           X7           X8           X9          X10 
#>  1.042387046  0.004917742  1.026540812  0.069437346
fit$eta
#>            [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
#>  [1,]  0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#>  [2,] -1.046848 0.9317925 -1.046848 0.9591964 -1.046848 0.9591964 -1.046848
#>  [3,]  0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#>  [4,] -1.005511 1.0055114 -1.005511 1.0055114 -1.005511 1.0055114 -1.005511
#>  [5,]  0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#>  [6,] -1.161986 1.2387476 -1.161986 1.2387476 -1.161986 1.2387476 -1.161986
#>  [7,]  0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#>  [8,] -1.096769 1.0967686 -1.096769 1.0967686 -1.096769 1.0967686 -1.096769
#>  [9,]  0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> [10,] -1.052338 1.0523384 -1.052338 1.0523384 -1.052338 1.0523384 -1.052338
#>            [,8]      [,9]    [,10]
#>  [1,] 0.0000000  0.000000 0.000000
#>  [2,] 0.9591964 -1.046848 1.424857
#>  [3,] 0.0000000  0.000000 0.000000
#>  [4,] 1.0055114 -1.005511 1.005511
#>  [5,] 0.0000000  0.000000 0.000000
#>  [6,] 0.8549381 -1.161986 1.238748
#>  [7,] 0.0000000  0.000000 0.000000
#>  [8,] 1.0967686 -1.096769 1.096769
#>  [9,] 0.0000000  0.000000 0.000000
#> [10,] 1.0523384 -1.052338 1.052338
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
