
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
  lambda1 = 0.05, lambda2 = 0.04, penalty = "SCAD"
)
fit$beta
#>           X1           X2           X3           X4           X5           X6 
#>  1.132417993 -0.078666859  1.061045884 -0.023441560  1.077448342 -0.019760892 
#>           X7           X8           X9          X10 
#>  1.038908842  0.004856086  1.022937462  0.069508666
fit$eta
#>          [,1]      [,2]      [,3]      [,4]      [,5]      [,6]      [,7]
#> X1   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X2  -1.042569 0.9282354 -1.042569 0.9551599 -1.042569 0.9551599 -1.042569
#> X3   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X4  -1.001112 1.0011124 -1.001112 1.0011124 -1.001112 1.0011124 -1.001112
#> X5   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X6  -1.157034 1.2336007 -1.157034 1.2336007 -1.157034 1.2336007 -1.157034
#> X7   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X8  -1.092529 1.0925289 -1.092529 1.0925289 -1.092529 1.0925289 -1.092529
#> X9   0.000000 0.0000000  0.000000 0.0000000  0.000000 0.0000000  0.000000
#> X10 -1.048001 1.0480012 -1.048001 1.0480012 -1.048001 1.0480012 -1.048001
#>          [,8]      [,9]    [,10]
#> X1  0.0000000  0.000000 0.000000
#> X2  0.9551599 -1.042569 1.419130
#> X3  0.0000000  0.000000 0.000000
#> X4  1.0011124 -1.001112 1.001112
#> X5  0.0000000  0.000000 0.000000
#> X6  0.8507663 -1.157034 1.233601
#> X7  0.0000000  0.000000 0.000000
#> X8  1.0925289 -1.092529 1.092529
#> X9  0.0000000  0.000000 0.000000
#> X10 1.0480012 -1.048001 1.048001
```

## Code of Conduct

Please note that the survtrans project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
