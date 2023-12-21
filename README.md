
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
#> 载入需要的程辑包：survode
#> 载入需要的程辑包：survival
## basic example code
formula <- Surv(time, status) ~ . - group - id
group <- sim1$group
fit <- coxtrans(
  formula, data = sim1, group = group, lambda1 = 0.02, penalty = "SCAD"
)
#> Warning in check_convergence(coef = coef, loss = -log_lik, last_record =
#> record, : No improvement for 10 iterations
fit$coefficients
#>               [,1]        [,2]      [,3]
#>  [1,] -0.238688218 0.238688218 1.2635865
#>  [2,]  0.000000000 0.000000000 1.0005424
#>  [3,]  0.000000000 0.000000000 1.0266637
#>  [4,]  0.000000000 0.000000000 0.9846528
#>  [5,]  0.000000000 0.000000000 1.0506290
#>  [6,] -0.001838417 0.001838417 1.0238844
#>  [7,]  0.000000000 0.000000000 0.9236874
#>  [8,]  0.000000000 0.000000000 1.0204494
#>  [9,]  0.000000000 0.000000000 1.0595770
#> [10,]  0.000000000 0.000000000 1.0035363
```

## Code of Conduct

Please note that the survtrans project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
