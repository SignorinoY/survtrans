#' Transfer Learning of Multi-source for Cox proportional hazards model
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
#' @return a coxtrans object.
#' @export
#' @examples
#' library(survtrans)
#' formula <- Surv(time, status) ~ . - group - id
#' fit <- coxtrans(
#'   formula, sim2, as.factor(sim2$group),
#'   lambda1 = 0.05, lambda2 = 0.04, penalty = "SCAD"
#' )
#' fit$eta
coxtrans <- function( # nolint: cyclocomp_linter.
    formula, data, group, lambda1 = 0, lambda2 = 0,
    penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), rho = 2.0, init, control, ...) {
  set.seed(0)

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
    if (length(init) != n_features * (n_groups + 1)) {
      stop("Wrong length for inital values")
    }
  } else {
    init <- rep(0, n_features * (n_groups + 1))
  }

  # Check the control argument
  if (missing(control)) control <- survtrans_control(...)

  # Extract the coefficients from init vector
  init <- matrix(init, nrow = n_features)
  init <- sweep(init, 1, x_scale, `*`)
  eta <- init[, 1:n_groups, drop = FALSE]
  beta <- init[, n_groups + 1]
  xi <- matrix(0, nrow = n_features, ncol = n_groups * (n_groups - 1) / 2)
  for (i in 1:n_groups) {
    for (j in 1:n_groups) {
      if (i >= j) next
      pos <- get_position(i, j, n_groups)
      xi[, pos] <- eta[, i] - eta[, j]
    }
  }
  mu <- matrix(0, nrow = n_features, ncol = n_groups)
  nu <- matrix(0, nrow = n_features, ncol = n_groups * (n_groups - 1) / 2)

  # Initialize the training process
  record <- list(
    convergence = FALSE, n_iterations = 0, null_deviance = null_deviance,
    maxit = control$maxit, eps = control$eps
  )
  w <- numeric(n_samples)
  r <- numeric(n_samples)
  offset <- numeric(n_samples)
  alpha <- 1

  # Pre-calculate the quantities
  x2 <- x^2

  repeat {
    # Calculate the offset
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      offset[idx] <- x[idx, ] %*% (eta[, k] + beta)
    }

    # Calculate the loss
    hazard <- exp(offset)
    risk_set <- ave(hazard, group, FUN = cumsum)
    loss <- -sum(status * (offset - log(risk_set)))

    # Check the convergence
    record <- check_convergence(cbind(eta, beta), loss, record)
    if (record$convergence) break

    # Calculate the weights and residuals
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      wls <- calc_weights_residuals(
        offset = offset[idx], time = time[idx], status = status[idx]
      )
      w[idx] <- wls$weights
      r[idx] <- wls$residuals
    }

    # Update beta by weighted least squares
    xw <- x * w
    beta <- beta + solve(t(xw) %*% x, t(xw) %*% r)

    # Update the coefficients of constraints on eta
    xi_sum <- matrix(0, nrow = n_features, ncol = n_groups)
    nu_sum <- matrix(0, nrow = n_features, ncol = n_groups)
    for (i in 1:n_groups) {
      for (j in 1:n_groups) {
        if (i >= j) next
        pos <- get_position(i, j, n_groups)
        xi_sum[, i] <- xi_sum[, i] + xi[, pos]
        xi_sum[, j] <- xi_sum[, j] - xi[, pos]
        nu_sum[, i] <- nu_sum[, i] + nu[, pos]
        nu_sum[, j] <- nu_sum[, j] - nu[, pos]
      }
    }

    # Update eta by cyclic coordinate descent
    r <- r + offset - x %*% beta
    sub_convergence <- TRUE
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      x_ <- x[idx, , drop = FALSE]
      r_ <- r[idx] - x_ %*% eta[, k]
      xw_ <- x_ * w[idx]
      xwx_ <- colMeans(w[idx] * x2[idx, , drop = FALSE])
      psi <- xwx_ + alpha * n_groups
      for (iter in 1:control$maxit) {
        eta_old <- eta[, k]
        features_idx <- sample(seq_len(n_features), n_features, FALSE)
        for (j in features_idx) {
          phi <- mean(xw_[, j] * r_) + xwx_[j] * eta[j, k] -
            mu[j] - nu_sum[j, k] + alpha * xi_sum[j, k]
          eta[j, k] <- penalty_solution(phi, psi[j], penalty, lambda1, gamma)
          r_ <- r_ - x_[, j] * (eta[j, k] - eta_old[j])
        }
        if (max(abs(eta_old - eta[, k])) <= control$eps) break
      }
      if (iter == control$maxit) sub_convergence <- FALSE
    }

    # Update xi
    for (i in 1:n_groups) {
      for (j in 1:n_groups) {
        if (i >= j) next
        pos <- get_position(i, j, n_groups)
        zeta <- eta[, i] - eta[, j] + nu[, pos] / alpha
        for (k in 1:n_features) {
          xi[k, pos] <- penalty_solution(
            zeta[k], 1, penalty, lambda2 / alpha, gamma
          )
        }
      }
    }

    # Update dual lagrange multipliers mu and nu
    mu <- mu + alpha * rowSums(eta)
    for (i in 1:n_groups) {
      for (j in 1:n_groups) {
        if (i >= j) next
        pos <- get_position(i, j, n_groups)
        nu[, pos] <- nu[, pos] + alpha * (eta[, i] - eta[, j] - xi[, pos])
      }
    }

    # Update alpha
    if (!sub_convergence) {
      alpha <- min(alpha * rho, n_samples)
    }
  }

  # Recognize the group assignment
  eta_processed <- matrix(0, nrow = n_features, ncol = n_groups)
  eta_group <- matrix(0, nrow = n_features, ncol = n_groups)
  for (i in 1:n_features) {
    is_processed <- rep(FALSE, n_groups)
    for (j in 1:n_groups) {
      if (is_processed[j]) next
      is_processed[j] <- TRUE
      eta_group[i, j] <- j
      for (k in 1:n_groups) {
        if (is_processed[k]) next
        pos <- get_position(j, k, n_groups)
        if (abs(xi[i, pos]) < control$eps) {
          eta_group[i, k] <- j
          is_processed[k] <- TRUE
        }
      }
    }
    for (j in unique(eta_group[i, ])) {
      idx <- which(eta_group[i, ] == j)
      eta_processed[i, idx] <- mean(eta[i, idx])
    }
  }

  # Centerlize the eta
  eta_bar <- rowMeans(eta_processed)
  eta <- sweep(eta_processed, 1, eta_bar, "-")
  eta[abs(eta) < control$eps] <- 0
  beta <- beta + eta_bar

  # Refit the beta with the final eta
  for (k in 1:n_groups) {
    idx <- group_idxs[[k]]
    offset[idx] <- x[idx, ] %*% (eta[, k] + beta)
  }
  for (k in 1:n_groups) {
    idx <- group_idxs[[k]]
    wls <- calc_weights_residuals(
      offset = offset[idx], time = time[idx], status = status[idx]
    )
    w[idx] <- wls$weights
    r[idx] <- wls$residuals
  }
  xw <- x * w
  beta <- beta + solve(t(xw) %*% x, t(xw) %*% r)

  # Unscale the coefficients
  coefficients <- cbind(eta, beta)
  coefficients <- sweep(coefficients, 1, x_scale, `/`)
  eta <- coefficients[, 1:n_groups]
  beta <- coefficients[, n_groups + 1]
  coefficients <- as.vector(coefficients)

  # Return the fit
  fit <- list(
    coefficients = coefficients, beta = beta, eta = eta, eta_group = eta_group,
    xi = xi, mu = mu, nu = nu, alpha = alpha, group_levels = group_levels,
    logLik = -loss, iter = record$n_iterations, message = record$message,
    penalty = penalty, lambda1 = lambda1, lambda2 = lambda2, gamma = gamma,
    rho = rho, formula = formula, call = match.call()
  )
  class(fit) <- "coxtrans"
  return(fit)
}
