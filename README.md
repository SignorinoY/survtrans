
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
#>  1.136309066 -0.078983198  1.064655571 -0.023896843  1.081362168 -0.019828737 
#>           X7           X8           X9          X10 
#>  1.042616197  0.004921702  1.026778180  0.069432737
fit$eta
#>          [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
#> X1   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X2  -1.047124 0.9320227 -1.047124 0.9594568 -1.047124 0.9594568 -1.047124
#> X3   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X4  -1.005795 1.0057954 -1.005795 1.0057954 -1.005795 1.0057954 -1.005795
#> X5   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X6  -1.162305 1.2390800 -1.162305 1.2390800 -1.162305 1.2390800 -1.162305
#> X7   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X8  -1.097043 1.0970427 -1.097043 1.0970427 -1.097043 1.0970427 -1.097043
#> X9   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X10 -1.052618 1.0526185 -1.052618 1.0526185 -1.052618 1.0526185 -1.052618
#>          [,8]      [,9]    [,10]
#> X1  0.0000000  0.000000 0.000000
#> X2  0.9594568 -1.047124 1.425227
#> X3  0.0000000  0.000000 0.000000
#> X4  1.0057954 -1.005795 1.005795
#> X5  0.0000000  0.000000 0.000000
#> X6  0.8552062 -1.162305 1.239080
#> X7  0.0000000  0.000000 0.000000
#> X8  1.0970427 -1.097043 1.097043
#> X9  0.0000000  0.000000 0.000000
#> X10 1.0526185 -1.052618 1.052618
```

## Code of Conduct

Please note that the survtrans project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
