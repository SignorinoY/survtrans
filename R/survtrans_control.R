#' Ancillary arguments for controlling survtrans fitting
#' @param eps the convergence criteria for the CCD algorithm. Default is 1e-4.
#' @param maxit the maximum number of iterations for the CCD algorithm.
#'   Default is 1000.
#' @param patience the number of iterations without improvement before
#'  stopping the CCD algorithm. Default is 5.
#' @param verbose a logical value indicating whether to print messages
#'  during the fitting process. Default is \code{FALSE}.
survtrans_control <- function(
    eps = 1e-4, maxit = 1000, patience = 5, verbose = FALSE) {
  if (eps <= 0) stop("Invalid convergence criteria")
  if (maxit < 0) stop("Invalid value for iterations")
  if (patience < 0) stop("Invalid value for patience")
  if (!is.logical(verbose)) stop("Invalid value for verbose")
  list(
    eps = eps, maxit = as.integer(maxit), patience = as.integer(patience),
    verbose = verbose
  )
}
