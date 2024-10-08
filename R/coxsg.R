#' Subgroup Analysis for Cox proportional hazards model
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param lambda1 a non-negative value specifying the sparse penalty
#' parameter. The default is 0.
#' @param lambda2 a non-negative value specifying the group penalty
#' parameter. The default is 0.
#' @param penalty a character string specifying the penalty function.
#' The default is "lasso". Other options are "MCP" and "SCAD".
#' @param gamma a non-negative value specifying the penalty parameter. The
#' default is 3.7 for SCAD and 3.0 for MCP.
#' @param rho a value in (2, 10) specifying the expansion factor of the
#' augmented Lagrangian's penalty parameter. The default is 2.0.
#' @param init a numeric vector specifying the initial value of the
#' coefficients. The default is a zero vector.
#' @param control Object of class \link{survtrans_control} containing
#' control parameters for the fitting algorithm. Default is
#' \code{survtrans_control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans_control}}.
#' @return a coxsg object.
#' @export
#' @examples
#' library(survtrans)
#' formula <- Surv(time, status) ~ . - group - id
#' group <- as.factor(sim2$group)
#' fit <- coxsg(
#'   formula, sim2, group,
#'   lambda1 = 0.07, lambda2 = 0.05, penalty = "SCAD"
#' )
#' fit$coefficients
coxsg <- function(
    formula, data, group, lambda1 = 0, lambda2 = 0,
    penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), rho = 2.0, init, control, ...) {
  set.seed(42)

  # Load the data
  data_ <- data
  group_ <- group
  data <- preprocess_data(formula, data_, group = group_)
  x <- data$x
  x_scale <- attr(x, "scale")
  time <- data$time
  status <- data$status
  group <- data$group

  # Properties of the data
  n_samples <- nrow(x)
  n_features <- ncol(x)
  n_groups <- length(unique(group))
  group_levels <- levels(group)
  group_idxs <- lapply(group_levels, function(x) which(group == x))

  # Calculate the null deviance
  risk_set_size <- ave(rep(1, n_samples), group, FUN = cumsum)
  for (k in 1:n_groups) {
    idx <- group_idxs[[k]]
    risk_set_size[idx] <- ave(risk_set_size[idx], time[idx], FUN = max)
  }
  null_deviance <- -sum(status * log(risk_set_size))

  # Check the penalty argument
  penalty <- match.arg(penalty)

  # Check the init argument
  if (!missing(init) && length(init) > 0) {
    if (length(init) != n_features * n_groups) {
      stop("Wrong length for inital values")
    }
  } else {
    init <- rep(0, n_features * n_groups)
  }

  # Check the control argument
  if (missing(control)) control <- survtrans_control(...)

  # Extract the coefficients from init vector
  init <- matrix(init, nrow = n_features)
  init <- sweep(init, 1, x_scale, `*`)
  beta <- init[, 1:n_groups, drop = FALSE]
  xi <- matrix(0, nrow = n_features, ncol = n_groups * (n_groups - 1) / 2)
  for (i in 1:n_groups) {
    for (j in 1:n_groups) {
      if (i >= j) next
      pos <- get_position(i, j, n_groups)
      xi[, pos] <- beta[, i] - beta[, j]
    }
  }
  mu <- matrix(0, nrow = n_features, ncol = n_groups * (n_groups - 1) / 2)

  # Initialize the training process
  record <- list(
    convergence = FALSE, n_iterations = 0, null_deviance = null_deviance,
    maxit = control$maxit, eps = control$eps
  )
  w <- numeric(n_samples)
  r <- numeric(n_samples)
  theta <- numeric(n_samples)
  alpha <- 1

  # Pre-calculate the quantities
  x2 <- x^2

  repeat {
    # Calculate the theta
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      theta[idx] <- x[idx, ] %*% beta[, k]
    }

    # Calculate the loss
    hazard <- exp(theta)
    risk_set <- ave(hazard, group, FUN = cumsum)
    loss <- -sum(status * (theta - log(risk_set)))

    # Check the convergence
    record <- check_convergence(beta, loss, record)
    if (record$convergence) break

    # Calculate the weights and residuals
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      wls <- approx_likelihood(
        offset = theta[idx], time = time[idx], status = status[idx]
      )
      w[idx] <- wls$weights
      r[idx] <- wls$residuals
    }

    # Update the coefficients of constraints on eta
    xi_sum <- matrix(0, nrow = n_features, ncol = n_groups)
    mu_sum <- matrix(0, nrow = n_features, ncol = n_groups)
    beta_sum <- matrix(0, nrow = n_features, ncol = n_groups)
    for (i in 1:n_groups) {
      for (j in 1:n_groups) {
        if (i >= j) next
        pos <- get_position(i, j, n_groups)
        xi_sum[, i] <- xi_sum[, i] + xi[, pos]
        xi_sum[, j] <- xi_sum[, j] - xi[, pos]
        mu_sum[, i] <- mu_sum[, i] + mu[, pos]
        mu_sum[, j] <- mu_sum[, j] - mu[, pos]
      }
      beta_sum[, i] <- rowSums(beta[, -i, drop = FALSE])
    }

    # Update eta by cyclic coordinate descent
    r <- r + theta
    sub_convergence <- TRUE
    for (k in 1:n_groups) {
      idx <- group == group_levels[k]
      x_ <- x[idx, , drop = FALSE]
      r_ <- r[idx] - x_ %*% beta[, k]
      xw_ <- x_ * w[idx]
      xwx_ <- colMeans(w[idx] * x2[idx, , drop = FALSE])
      for (iter in 1:control$inner.maxit) {
        beta_old <- beta[, k]
        features_idx <- sample(seq_len(n_features), n_features, FALSE)
        for (j in features_idx) {
          phi <- mean(xw_[, j] * r_) + xwx_[j] * beta[j, k] -
            mu_sum[j, k] + alpha * (xi_sum[j, k] + beta_sum[j, k])
          psi <- xwx_[j] + alpha * (n_groups - 1)
          beta[j, k] <- close_update(phi, psi, penalty, lambda1, gamma)
          r_ <- r_ - x_[, j] * (beta[j, k] - beta_old[j])
        }
        if (max(abs(beta_old - beta[, k])) <= control$inner.eps) break
      }
      if (iter == control$inner.maxit) sub_convergence <- FALSE
    }

    # Update xi
    for (i in 1:n_groups) {
      for (j in 1:n_groups) {
        if (i >= j) next
        pos <- get_position(i, j, n_groups)
        zeta <- beta[, i] - beta[, j] + mu[, pos] / alpha
        for (k in 1:n_features) {
          xi[k, pos] <- close_update(
            zeta[k], 1, penalty, lambda2 / alpha, gamma
          )
        }
      }
    }

    # Update dual lagrange multipliers mu
    for (i in 1:n_groups) {
      for (j in 1:n_groups) {
        if (i >= j) next
        pos <- get_position(i, j, n_groups)
        mu[, pos] <- mu[, pos] + alpha * (beta[, i] - beta[, j] - xi[, pos])
      }
    }

    # Update alpha
    if (!sub_convergence) {
      alpha <- min(alpha * rho, n_samples)
    }
  }

  # Recognize the group assignment
  coefs_proc <- matrix(0, nrow = n_features, ncol = n_groups)
  coefs_group <- matrix(0, nrow = n_features, ncol = n_groups)
  for (i in 1:n_features) {
    is_processed <- rep(FALSE, n_groups)
    for (j in 1:n_groups) {
      if (is_processed[j]) next
      is_processed[j] <- TRUE
      coefs_group[i, j] <- j
      for (k in 1:n_groups) {
        if (is_processed[k]) next
        pos <- get_position(j, k, n_groups)
        if (abs(xi[i, pos]) < control$eps) {
          coefs_group[i, k] <- j
          is_processed[k] <- TRUE
        }
      }
    }
    for (j in unique(coefs_group[i, ])) {
      idx <- which(coefs_group[i, ] == j)
      coefs_proc[i, idx] <- mean(beta[i, idx])
    }
  }
  coefs <- coefs_proc
  coefs[abs(coefs) < control$eps] <- 0

  # Unscale the coefficients
  coefs <- sweep(coefs, 1, x_scale, `/`)

  colnames(coefs) <- group_levels
  rownames(coefs) <- colnames(x)

  # Return the fit
  fit <- list(
    coefficients = coefs, coefficients_group = coefs_group,
    xi = xi, mu = mu, alpha = alpha, group_levels = group_levels,
    logLik = -loss, iter = record$n_iterations, message = record$message,
    penalty = penalty, lambda1 = lambda1, gamma = gamma,
    formula = formula, call = match.call()
  )
  class(fit) <- "coxsg"
  return(fit)
}
