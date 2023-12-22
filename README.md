
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
group <- as.factor(sim1$group)
fit <- cv_coxtrans(formula, sim1, group, penalty = "SCAD")
coef(fit)
#>              [,1]        [,2]      [,3]
#>  [1,] -0.10396483  0.10396483 1.0964685
#>  [2,]  0.02007187 -0.02007187 0.8679913
#>  [3,]  0.00000000  0.00000000 0.9126952
#>  [4,]  0.00000000  0.00000000 0.8735655
#>  [5,]  0.01844098 -0.01844098 0.9146741
#>  [6,] -0.04535751  0.04535751 0.9815223
#>  [7,]  0.00000000  0.00000000 0.8255911
#>  [8,]  0.00000000  0.00000000 0.9077969
#>  [9,]  0.00000000  0.00000000 0.9500952
#> [10,]  0.00000000  0.00000000 0.8960722
```

## Code of Conduct

Please note that the survtrans project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
