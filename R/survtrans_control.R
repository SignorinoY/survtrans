#' Ancillary arguments for controlling survtrans fitting
#' @param eps the convergence criteria for the proposed algorithm.
#'  Default is 1e-4.
#' @param maxit the maximum number of iterations for the proposed algorithm.
#'  Default is 1000.
#' @param inner.eps the convergence criteria for the inner loop of the CCD
#'  algorithm. Default is 1e-3.
#' @param inner.maxit the maximum number of iterations for the inner loop
#'  of the CCD algorithm. Default is 100.
#' @param verbose a logical value indicating whether to print messages
#'  during the fitting process. Default is \code{FALSE}.
survtrans_control <- function(
    eps = 1e-4, maxit = 1000, inner.eps = 1e-3, inner.maxit = 100,
    verbose = FALSE) {
  if (eps <= 0) stop("Invalid convergence criteria")
  if (maxit < 0) stop("Invalid value for iterations")
  if (inner.eps <= 0) stop("Invalid convergence criteria for inner loop")
  if (inner.maxit < 0) stop("Invalid value for iterations for inner loop")
  if (!is.logical(verbose)) stop("Invalid value for verbose")
  list(
    eps = eps, maxit = as.integer(maxit), inner.eps = inner.eps,
    inner.maxit = as.integer(inner.maxit), verbose = verbose
  )
}
