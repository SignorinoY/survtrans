#' Ancillary arguments for controlling survtrans fitting
#' @param eps the convergence criteria for the CCD algorithm. Default is 1e-6.
#' @param maxit the maximum number of iterations for the CCD algorithm.
#'   Default is 1000.
#' @param patience the number of iterations without improvement before
#'  stopping the CCD algorithm. Default is 3.
survtrans_control <- function(eps = 1e-6, maxit = 1000, patience = 3) {
  if (eps <= 0) stop("Invalid convergence criteria")
  if (maxit < 0) stop("Invalid value for iterations")
  if (patience < 0) stop("Invalid value for patience")
  list(eps = eps, maxit = as.integer(maxit), patience = as.integer(patience))
}
