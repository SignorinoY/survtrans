#' @title Variance-covariance method for a \code{ncvcox} model
#' @param object the result of a \code{ncvcox} fit.
#' @param ... Unused.
#' @return A matrix representing the variance-covariance matrix of
#' the coefficients.
#' @export
vcov.ncvcox <- function(object, ...) {
  # Properties of the coxens object
  time <- object$time
  status <- object$status
  group <- object$group
  x <- object$x
  n_samples <- nrow(x)
  group_levels <- levels(group)
  n_groups <- length(group_levels)
  group_idxs <- lapply(group_levels, function(g) which(group == g))

  coefficients <- object$coefficients
  coef_nonzero <- coefficients[coefficients != 0]
  x_nonzero <- x[, coefficients != 0]
  n_parameters <- sum(coefficients != 0)
  lp <- x_nonzero %*% coef_nonzero
  ghs_list <- lapply(seq_len(n_groups), function(k) {
    idx <- group_idxs[[k]]
    calc_grad_hess(lp[idx], x_nonzero[idx, ], time[idx], status[idx])
  })

  gradients <- do.call(rbind, lapply(ghs_list, function(ghs) ghs$grad))
  hessians <- do.call(rbind, lapply(ghs_list, function(ghs) ghs$hess))
  hess <- matrix(
    colSums(hessians),
    nrow = n_parameters, ncol = n_parameters
  )
  sigma <- penalty_grad(
    coef_nonzero, object$penalty, object$lambda, object$gamma
  ) / abs(coef_nonzero) * n_samples
  hess_inv <- solve(hess + diag(sigma))
  cov_grad <- cov(gradients) * n_samples
  vcov <- hess_inv %*% cov_grad %*% hess_inv
  return(vcov)
}
