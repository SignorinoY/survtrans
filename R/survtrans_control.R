#' Ancillary arguments for controlling survtrans fitting
#' @param eps the convergence criteria for the CCD algorithm. Default is 1e-6.
#' @param maxit the maximum number of iterations for the CCD algorithm.
#'   Default is 1000.
#' @export
survtrans_control <- function(eps = 1e-6, maxit = 1000) {
  if (eps <= 0) stop("Invalid convergence criteria")
  if (maxit < 0) stop("Invalid value for iterations")
  list(eps = eps, maxit = as.integer(maxit))
}
