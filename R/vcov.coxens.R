#' @title Variance-covariance method for a \code{coxens} model
#' @param object the result of a \code{coxens} fit.
#' @param ... Unused.
#' @return A matrix representing the variance-covariance matrix of
#' the coefficients.
#' @export
vcov.coxens <- function(object, ...) {
  # Properties of the coxens object
  time <- object$time
  status <- object$status
  group <- object$group
  x <- object$x
  n_samples <- nrow(x)
  n_features <- ncol(x)
  group_levels <- levels(group)
  n_groups <- length(group_levels)
  group_idxs <- lapply(group_levels, function(g) which(group == g))

  coefficients <- object$coefficients
  eta <- coefficients[, 1:n_groups]
  beta <- coefficients[, (n_groups + 1)]

  # Reassign the coefficients' group and track expanded coefficients
  eta_expanded <- numeric()
  coef_processed <- numeric()
  eta_idx <- matrix(0, nrow = n_features, ncol = n_groups)
  n_total_groups <- 0

  for (j in seq_len(n_features)) {
    feature_groups <- as.factor(eta[j, ])
    feature_levels <- unique(as.character(feature_groups))
    for (k in seq_along(feature_levels)) {
      if (feature_levels[k] == "0") next
      idx <- feature_groups == feature_levels[k]
      eta_idx[j, idx] <- k + n_total_groups
    }
    eta_expanded <- c(eta_expanded, as.numeric(feature_levels))
    coef_processed <- c(coef_processed, as.numeric(feature_levels) + beta[j])
    n_total_groups <- n_total_groups + length(feature_levels)
  }
  coef_expanded <- c(eta_expanded, beta)
  n_expanded_nonzero <- sum(coef_expanded != 0)
  n_processed_nonzero <- sum(coef_processed != 0)
  if (n_expanded_nonzero == 0 || n_processed_nonzero == 0) {
    stop("No non-zero coefficients to compute the variance-covariance matrix")
  }

  beta_idx <- cumsum(beta != 0) + sum(eta_expanded != 0)
  beta_idx[beta == 0] <- 0

  # Calculate the variance-covariance matrix for non-sparse coefficients
  z <- do.call(rbind, lapply(seq_len(n_groups), function(k) {
    idx <- group_idxs[[k]]
    feature_map <- matrix(0, nrow = n_features, ncol = n_total_groups)
    for (j in seq_len(n_features)) {
      feature_map[j, eta_idx[j, k]] <- 1
    }
    x[idx, ] %*% feature_map
  }))
  z <- cbind(z, x)[, coef_expanded != 0]
  coef_expanded <- coef_expanded[coef_expanded != 0]
  lp <- z %*% coef_expanded
  ghs_list <- lapply(seq_len(n_groups), function(k) {
    idx <- group_idxs[[k]]
    calc_grad_hess(lp[idx], z[idx, ], time[idx], status[idx])
  })
  gradients <- do.call(rbind, lapply(ghs_list, function(ghs) ghs$grad))
  hessians <- do.call(rbind, lapply(ghs_list, function(ghs) ghs$hess))
  hess <- matrix(
    colSums(hessians),
    nrow = n_expanded_nonzero, ncol = n_expanded_nonzero
  )
  hess_inv <- solve(hess)
  cov_grad <- cov(gradients) * n_samples
  vcov_expanded <- hess_inv %*% cov_grad %*% hess_inv
  vcov_expanded_inv <- solve(vcov_expanded)

  # Calculate the Null space of the constraints
  n_constr <- sum(rowSums(eta_idx != 0) > 0)
  constr_idx <- which(rowSums(eta_idx != 0) > 0)
  constr <- matrix(0, nrow = n_constr, ncol = sum(coef_expanded != 0))
  for (i in seq_len(n_constr)) {
    idx <- constr_idx[i]
    group_levels <- unique(eta_idx[idx, ])
    group_levels <- group_levels[group_levels != 0]
    constr[i, group_levels] <- sapply(
      group_levels, function(level) sum(eta_idx[idx, ] == level)
    )
  }
  null_constr <- MASS::Null(t(constr))

  # Calculate the variance-covariance matrix for the constrained coefficients
  vcov_constr <- solve(t(null_constr) %*% vcov_expanded_inv %*% null_constr)
  vcov_constr <- null_constr %*% vcov_constr %*% t(null_constr)

  # Reconstruct the variance-covariance matrix to the original coefficients
  prox_processed <- matrix(0, n_expanded_nonzero, n_processed_nonzero)
  j <- 1
  for (i in seq_len(n_features)) {
    idx1 <- unique(eta_idx[i, ])
    idx2 <- beta_idx[i]
    if (any(idx1 != 0) || idx2 != 0) {
      for (idx in idx1) {
        prox_processed[idx, j] <- 1
        prox_processed[idx2, j] <- 1
        j <- j + 1
      }
    }
  }
  vcov_processed <- t(prox_processed) %*% vcov_constr %*% prox_processed

  return(vcov_processed)
}
