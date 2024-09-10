#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import progress
#' @importFrom doSNOW registerDoSNOW
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics points
#' @importFrom graphics segments
#' @importFrom MASS mvrnorm
#' @importFrom MASS Null
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom Rcpp sourceCpp
#' @importFrom simsurv simsurv
#' @importFrom stats ave
#' @importFrom stats BIC
#' @importFrom stats coef
#' @importFrom stats cov
#' @importFrom stats logLik
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats model.response
#' @importFrom stats optim
#' @importFrom stats pchisq
#' @importFrom stats predict
#' @importFrom stats printCoefmat
#' @importFrom stats qnorm
#' @importFrom stats vcov
#' @importFrom stringr str_extract
#' @importFrom survival Surv
#' @importFrom survode survode
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @useDynLib survtrans, .registration = TRUE
## usethis namespace: end
NULL
