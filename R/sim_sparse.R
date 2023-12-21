#' Simulated Survival Data: Sparse Effects
#'
#' A dataset of survival data with sparse effects. The survival time is
#' generated according to the following model:
#' \deqn{\lambda(t)=\lambda_{0}(t)\exp\left(x^\top\beta\right),} where
#' \eqn{\lambda_{0}(t)} represents the baseline hazard, \eqn{\beta} represents
#' the effects, and \eqn{x} represents the covariates. The covariates
#' \eqn{x=(x_{1},\ldots,x_{10})} are generated from a multivariate normal
#' distribution with mean 0 and covariance matrix \eqn{\Sigma=I_{10}}. The
#' baseline hazard \eqn{\lambda_{0}(t)=t^2}, while the effects
#' \eqn{\beta=(1,0.5,0,\ldots,0)}. The maximum censoring time is fixed at 3, and
#' the censoring rate is approximately 20%.
#'
#' @name sim_sparse
#' @docType data
#' @format A data frame with 1000 rows and 13 variables:
#' \describe{
#' \item{id}{Individual identifier, 1-1000.}
#' \item{time}{Survival time.}
#' \item{status}{Status indicator, 0=censored, 1=event.}
#' \item{X1}{covariate 1.}
#' \item{X2}{covariate 2.}
#' \item{X3}{covariate 3.}
#' \item{X4}{covariate 4.}
#' \item{X5}{covariate 5.}
#' \item{X6}{covariate 6.}
#' \item{X7}{covariate 7.}
#' \item{X8}{covariate 8.}
#' \item{X9}{covariate 9.}
#' \item{X10}{covariate 10.}
#' }
#' @source \url{../articles/simulate-data.html}
NULL
