
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

### Multi-Sources Survival Analysis

``` r
formula <- Surv(time, status) ~ . - group - id
```

#### Target Learning

``` r
fit <- ncvcox(
  formula, sim2[sim2$group == 10, ], lambda = 0.15, penalty = "SCAD"
)
summary(fit)
#> Call:
#> NULL
#> 
#>   n=100, number of events=76
#> 
#>        coef exp(coef) se(coef)      z Pr(>|z|)    
#> X1 -0.67132   0.51104  0.13061 -5.140 2.75e-07 ***
#> X2 -0.57862   0.56067  0.11382 -5.084 3.71e-07 ***
#> X3  0.31976   1.37680  0.10605  3.015  0.00257 ** 
#> X4  0.84723   2.33318  0.07791 10.875  < 2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>    exp(coef) exp(-coef) lower .95 upper .95
#> X1 0.5110    1.9568     0.3956    0.6601   
#> X2 0.5607    1.7836     0.4486    0.7008   
#> X3 1.3768    0.7263     1.1184    1.6949   
#> X4 2.3332    0.4286     2.0028    2.7181
```

#### Ensemble Learning

``` r
fit <- coxens(
  formula, sim2, sim2$group,
  lambda1 = 2e-2, lambda2 = 0, lambda3 = 3e-3, penalty = "SCAD"
)
summary(fit)
#> Call:
#> coxens(formula = formula, data = sim2, group = sim2$group, lambda1 = 0.02, 
#>     lambda2 = 0, lambda3 = 0.003, penalty = "SCAD")
#> 
#>   n=1000, number of events=811
#> 
#>                                     coef exp(coef) se(coef)       z Pr(>|z|)
#> X1 (1, 3, 5, 7, 9)               0.54067   1.71716  0.04802  11.260  < 2e-16
#> X1 (2, 4, 6, 8, 10)             -0.64495   0.52469  0.05096 -12.656  < 2e-16
#> X2 (1, 3, 5, 9)                  0.55958   1.74994  0.05097  10.979  < 2e-16
#> X2 (2, 4, 6, 8, 10)             -0.62512   0.53520  0.05283 -11.833  < 2e-16
#> X2 (7)                           0.31086   1.36460  0.13109   2.371   0.0177
#> X3 (1, 2, 3, 4, 5, 6, 8, 9, 10)  0.52807   1.69565  0.03905  13.523  < 2e-16
#> X3 (7)                           0.75915   2.13646  0.12915   5.878 4.15e-09
#> X4 (ALL)                         0.54321   1.72152  0.03951  13.750  < 2e-16
#>                                    
#> X1 (1, 3, 5, 7, 9)              ***
#> X1 (2, 4, 6, 8, 10)             ***
#> X2 (1, 3, 5, 9)                 ***
#> X2 (2, 4, 6, 8, 10)             ***
#> X2 (7)                          *  
#> X3 (1, 2, 3, 4, 5, 6, 8, 9, 10) ***
#> X3 (7)                          ***
#> X4 (ALL)                        ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                 exp(coef) exp(-coef) lower .95 upper .95
#> X1 (1, 3, 5, 7, 9)              1.7172    0.5824     1.5629    1.8866   
#> X1 (2, 4, 6, 8, 10)             0.5247    1.9059     0.4748    0.5798   
#> X2 (1, 3, 5, 9)                 1.7499    0.5714     1.5836    1.9338   
#> X2 (2, 4, 6, 8, 10)             0.5352    1.8685     0.4826    0.5936   
#> X2 (7)                          1.3646    0.7328     1.0554    1.7644   
#> X3 (1, 2, 3, 4, 5, 6, 8, 9, 10) 1.6957    0.5897     1.5707    1.8305   
#> X3 (7)                          2.1365    0.4681     1.6587    2.7518   
#> X4 (ALL)                        1.7215    0.5809     1.5933    1.8601
```

## Code of Conduct

Please note that the survtrans project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
