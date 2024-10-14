#' Simulated Group Survival Data: Multiple-source Transfer Learning
#'
#' A dataset containing 5 groups of survival data, where part of the effects
#' and baseline hazards are heterogeneous. Each group contains 100 individuals.
#' The survival time \eqn{T} of group \eqn{i} is generated according to the
#' following models:
#' \deqn{
#'     \lambda^{(i)}(t)=\lambda_{0,i}(t)\exp\left(x^\top(\beta^{(i)})\right),
#' }
#' where \eqn{\lambda_{0,i}(t)} represents the baseline hazard of group i,
#' \eqn{\beta^{(i)}} represents the effects of group i, and \eqn{x} represents
#' the covariates. The covariates \eqn{x=(x_{1},\ldots,x_{20})} are generated
#' from a multivariate normal distribution with mean 0 and covariance matrix
#' \eqn{\Sigma=I_{20}}. The baseline hazard function
#' \deqn{
#'     \lambda_{0,i}(t)=\left\{\begin{array}{ll}
#'         t^{2}, & i=1,3,5 \\
#'         t,     & i=2,4.
#'     \end{array}\right.
#' }
#' while the effects
#' \deqn{
#'     \beta^{(i)}=\left\{\begin{array}{ll}
#'         (0.3,0.3,0.3,0.3,0,\ldots,0),   & i=1, \\
#'         (0.9,0.9,0.3,0.3,0,\ldots,0), & i=2,4, \\
#'         (-0.3,-0.3,0.3,0.3,0,\ldots,0), & i=3,5.
#'     \end{array}\right.
#' }
#' The maximum censoring time is fixed at 2, and the censoring rate is
#' approximately 20%.
#'
#' @name sim2
#' @docType data
#' @format A data frame with 500 rows and 24 variables:
#' \describe{
#' \item{id}{Individual identifier, 1-500}
#' \item{group}{Group indicator, 1-5.}
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
#' \item{X11}{covariate 11.}
#' \item{X12}{covariate 12.}
#' \item{X13}{covariate 13.}
#' \item{X14}{covariate 14.}
#' \item{X15}{covariate 15.}
#' \item{X16}{covariate 16.}
#' \item{X17}{covariate 17.}
#' \item{X18}{covariate 18.}
#' \item{X19}{covariate 19.}
#' \item{X20}{covariate 20.}
#' }
#' @source \url{../articles/simulate-data.html}
NULL
