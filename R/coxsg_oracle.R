#' Personalized Multi-task Learning of Cox proportional hazards model (Oracle)
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param sparse_idx a logical matrix specifying the sparse parameters, where
#' each row corresponds to a feature and each column corresponds to a group.
#' @param group_idx a logical matrix specifying the group parameters, where
#' each row corresponds to a feature and each column corresponds to a group.
#' @param init a numeric vector specifying the initial value of the
#' coefficients. The default is a zero vector.
#' @param control Object of class \link{survtrans_control} containing
#' control parameters for the fitting algorithm. Default is
#' \code{survtrans_control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans_control}}.
#' @return a coxmtl object.
coxsg_oracle <- function(
    formula, data, group, sparse_idx, group_idx, init,
    control, ...) {
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

  # Check the sparse_idx argument
  if (missing(sparse_idx)) {
    sparse_idx <- matrix(TRUE, nrow = n_features, ncol = n_groups)
  }
  if (missing(group_idx)) {
    group_idx <- matrix(rep(1:n_groups, each = n_features), ncol = n_groups)
  }

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

  fn <- function(param) {
    # Extract the parameters
    eta <- matrix(param, nrow = n_features)

    # Calculate the log-likelihood
    theta <- rep(0, n_samples)
    for (k in 1:n_groups) {
      idx <- group == group_levels[k]
      theta[idx] <- theta[idx] + x[idx, ] %*% eta[, k]
    }
    theta_max <- max(theta)
    hazard <- exp(theta - theta_max)
    risk_set <- ave(hazard, group, FUN = cumsum)
    for (k in 1:n_groups) {
      ind <- group == group_levels[k]
      risk_set[ind] <- ave(risk_set[ind], time[ind], FUN = max)
    }
    log_risk <- log(risk_set) + theta_max
    log_lik <- sum(status * (theta - log_risk))
    -log_lik
  }

  gr <- function(param) {
    # Extract the parameters
    eta <- matrix(param, nrow = n_features)

    # Calculate the log-likelihood
    theta <- rep(0, n_samples)
    for (k in 1:n_groups) {
      idx <- group == group_levels[k]
      theta[idx] <- theta[idx] + x[idx, ] %*% eta[, k]
    }
    theta_max <- max(theta)
    hazard <- exp(theta - theta_max)
    risk_set <- ave(hazard, group, FUN = cumsum)
    for (k in 1:n_groups) {
      idx <- group == group_levels[k]
      risk_set[idx] <- ave(risk_set[idx], time[idx], FUN = max)
    }
    hazard_x <- sweep(x, 1, hazard, "*")
    risk_set_x <- matrix(0, nrow = n_samples, ncol = n_features)
    for (k in 1:n_groups) {
      ind <- group == group_levels[k]
      risk_set_x[ind, ] <- apply(hazard_x[ind, ], 2, cumsum)
      # TODO: Missing the max operations for ties
    }
    risk_set_ratio <- sweep(risk_set_x, 1, risk_set, "/")
    grads <- sweep(x - risk_set_ratio, 1, status, "*")

    grad <- matrix(0, nrow = n_features, ncol = n_groups)
    for (k in 1:n_groups) {
      idx <- group == group_levels[k]
      grad[, k] <- -colSums(grads[idx, ])
    }
    # Mask the sparse parameters
    grad[sparse_idx == 0] <- 0
    # Mask the group parameters
    for (k in 1:n_features) {
      values <- unique(group_idx[k, ])
      for (j in values) {
        idx <- group_idx[k, ] == j
        grad[k, idx] <- sum(grad[k, idx])
      }
    }

    grad <- as.vector(grad)
    return(grad)
  }

  res <- optim(init, fn, gr, method = "BFGS")
  eta <- matrix(res$par, nrow = n_features)

  # Unscale the coefficients
  eta <- sweep(eta, 1, x_scale, "/")
  coef <- as.vector(eta)

  # Return the fit
  fit <- list(
    coefficients = coef, logLik = -res$value, iter = res$iter,
    eta = eta, formula = formula, call = match.call()
  )
  class(fit) <- "coxsg"
  return(fit)
}
