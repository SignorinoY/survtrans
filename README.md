
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
#> 载入需要的程序包：progress
#> 载入需要的程序包：survode
#> 载入需要的程序包：survival
```

### Multi-Source Survival Analysis

Ensemble Learning

``` r
formula <- Surv(time, status) ~ . - group - id
fit <- coxens(
  formula, sim2, sim2$group,
  lambda1 = 2e-2, lambda2 = 0, lambda3 = 3e-3, penalty = "SCAD"
)
summary(fit)
#> Call:
#> NULL
#> 
#>   n=1000, number of events=811
#> 
#>                                     coef exp(coef) se(coef)      z Pr(>|z|)    
#> X1 (1, 3, 5, 7, 9)               0.54067   1.71716  0.06112  8.845  < 2e-16 ***
#> X1 (2, 4, 6, 8, 10)             -0.64495   0.52469  0.06868 -9.390  < 2e-16 ***
#> X2 (1, 3, 5, 9)                  0.55958   1.74994  0.06327  8.844  < 2e-16 ***
#> X2 (2, 4, 6, 8, 10)             -0.62512   0.53520  0.07006 -8.922  < 2e-16 ***
#> X2 (7)                           0.31086   1.36460  0.14220  2.186   0.0288 *  
#> X3 (1, 2, 3, 4, 5, 6, 8, 9, 10)  0.52807   1.69565  0.04322 12.218  < 2e-16 ***
#> X3 (7)                           0.75915   2.13646  0.14091  5.387 7.15e-08 ***
#> X4 (All)                         0.54321   1.72152  0.04551 11.935  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                 exp(coef) exp(-coef) lower .95 upper .95
#> X1 (1, 3, 5, 7, 9)              1.7172    0.5824     1.5233    1.9357   
#> X1 (2, 4, 6, 8, 10)             0.5247    1.9059     0.4586    0.6003   
#> X2 (1, 3, 5, 9)                 1.7499    0.5714     1.5458    1.9810   
#> X2 (2, 4, 6, 8, 10)             0.5352    1.8685     0.4665    0.6140   
#> X2 (7)                          1.3646    0.7328     1.0327    1.8032   
#> X3 (1, 2, 3, 4, 5, 6, 8, 9, 10) 1.6957    0.5897     1.5579    1.8455   
#> X3 (7)                          2.1365    0.4681     1.6209    2.8160   
#> X4 (All)                        1.7215    0.5809     1.5746    1.8822
```

## Code of Conduct

Please note that the survtrans project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
