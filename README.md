
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
  formula, sim2, group, lambda1 = 0.05, lambda2 = 0.05, penalty = "SCAD"
)
sweep(fit$eta, 1, fit$beta, "+")
#>           [,1]      [,2]       [,3]      [,4]       [,5]      [,6]       [,7]
#> X1   1.1054434 1.1054434  1.1054434 1.1054434  1.1054434 1.1054434  1.1054434
#> X2  -1.1539259 0.8995398 -1.1539259 0.8995398 -1.1539259 0.8995398 -1.1539259
#> X3   1.0519998 1.0519998  1.0519998 1.0519998  1.0519998 1.0519998  1.0519998
#> X4  -0.9805338 0.9504827 -0.9805338 0.9504827 -0.9805338 0.9504827 -0.9805338
#> X5   1.0742583 1.0742583  1.0742583 1.0742583  1.0742583 1.0742583  1.0742583
#> X6  -1.1291167 1.0882749 -1.1291167 1.0882749 -1.1291167 1.0882749 -1.1291167
#> X7   1.0460208 1.0460208  1.0460208 1.0460208  1.0460208 1.0460208  1.0460208
#> X8  -1.1278733 1.1502917 -1.1278733 1.1502917 -1.1278733 1.1502917 -1.1278733
#> X9   1.0382267 1.0382267  1.0382267 1.0382267  1.0382267 1.0382267  1.0382267
#> X10 -0.9650904 1.0959670 -0.9650904 1.0959670 -0.9650904 1.0959670 -0.9650904
#>          [,8]       [,9]     [,10]
#> X1  1.1054434  1.1054434 1.1054434
#> X2  0.8995398 -1.1539259 1.3680706
#> X3  1.0519998  1.0519998 1.0519998
#> X4  0.9504827 -0.9805338 0.9504827
#> X5  1.0742583  1.0742583 1.0742583
#> X6  1.0506468 -1.1291167 1.0882749
#> X7  1.0460208  1.0460208 1.0460208
#> X8  1.1502917 -1.1278733 1.1502917
#> X9  1.0382267  1.0382267 1.0382267
#> X10 1.0959670 -0.9650904 1.0959670
```

## Code of Conduct

Please note that the survtrans project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
