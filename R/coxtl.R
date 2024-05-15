#' Transfer Learning for Cox proportional hazards model
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param target a factor specifying the target group.
#' @param lambda1 a non-negative value specifying the sparse penalty
#' parameter. The default is 0.
#' @param lambda2 a non-negative value specifying the transfer penalty
#' parameter. The default is 0.
#' @param penalty a character string specifying the penalty function.
#' The default is "lasso". Other options are "MCP" and "SCAD".
#' @param gamma a non-negative value specifying the penalty parameter. The
#' default is 3.7 for SCAD and 3.0 for MCP.
#' @param init a numeric vector specifying the initial value of the
#' coefficients. The default is a zero vector.
#' @param control Object of class \link{survtrans_control} containing
#' control parameters for the fitting algorithm. Default is
#' \code{survtrans_control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans_control}}.
#' @return a coxtl object.
#' @export
#' @examples
#' library(survtrans)
#' formula <- Surv(time, status) ~ . - group - id
#' group <- as.factor(sim2$group)
#' fit <- coxtl(
#'   formula, sim2, group, 10,
#'   lambda1 = 0.04, lambda2 = 0.11, penalty = "SCAD"
#' )
#' fit$beta
#' fit$eta
coxtl <- function(
    formula, data, group, target, lambda1 = 0, lambda2 = 0,
    penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), init, control, ...) {
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
  group_levels_drop <- group_levels[group_levels != target]

  # Calculate the null deviance
  risk_set_size <- ave(rep(1, n_samples), group, FUN = cumsum)
  for (k in 1:n_groups) {
    idx <- group == group_levels[k]
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
  eta <- init[, 1:(n_groups - 1), drop = FALSE]
  beta <- init[, n_groups]

  # Initialize the training process
  record <- list(
    convergence = FALSE, n_iterations = 0, null_deviance = null_deviance,
    maxit = control$maxit, eps = control$eps
  )
  w <- numeric(n_samples)
  r <- numeric(n_samples)

  # Pre-calculate the quantities
  x2 <- x^2

  repeat {
    # Calculate the theta
    theta <- x %*% beta
    for (k in 1:(n_groups - 1)) {
      idx <- group == group_levels_drop[k]
      theta[idx] <- theta[idx] + x[idx, ] %*% eta[, k]
    }

    # Calculate the loss
    hazard <- exp(theta - max(theta))
    risk_set <- ave(hazard, group, FUN = cumsum)
    for (k in 1:n_groups) {
      idx <- group == group_levels[k]
      risk_set[idx] <- ave(risk_set[idx], time[idx], FUN = max)
    }
    loss <- -sum(status * (theta - log(risk_set) - max(theta)))

    # Check the convergence
    record <- check_convergence(cbind(eta, beta), loss, record)
    if (record$convergence) break

    # Calculate the weights and residuals
    for (k in 1:n_groups) {
      idx <- group == group_levels[k]
      wls <- calc_weights_residuals(
        offset = theta[idx], time = time[idx], status = status[idx]
      )
      w[idx] <- wls$weights
      r[idx] <- wls$residuals
    }

    # Update beta by cyclic coordinate descent
    xw <- x * w
    xwx <- colMeans(w * x2)
    for (iter in 1:control$inner.maxit) {
      beta_old <- beta
      features_idx <- sample(seq_len(n_features), n_features, FALSE)
      for (j in features_idx) {
        z <- mean(xw[, j] * r) + xwx[j] * beta[j]
        beta[j] <- penalty_solution(z, xwx[j], penalty, lambda1, gamma)
        r <- r - x[, j] * (beta[j] - beta_old[j])
      }
      if (max(abs(beta_old - beta)) <= control$inner.eps) break
    }

    # Update eta by cyclic coordinate descent
    r <- r + theta - x %*% beta
    for (k in 1:(n_groups - 1)) {
      idx <- group == group_levels_drop[k]
      xk <- x[idx, , drop = FALSE]
      rk <- r[idx] - xk %*% eta[, k]
      xwk <- xk * w[idx]
      xwxk <- colMeans(w[idx] * x2[idx, , drop = FALSE])
      for (iter in 1:control$inner.maxit) {
        eta_old <- eta[, k]
        features_idx <- sample(seq_len(n_features), n_features, FALSE)
        for (j in features_idx) {
          phi <- mean(xwk[, j] * rk) + xwxk[j] * eta[j, k]
          eta[j, k] <- penalty_solution(phi, xwxk[j], penalty, lambda2, gamma)
          rk <- rk - xk[, j] * (eta[j, k] - eta_old[j])
        }
        if (max(abs(eta_old - eta[, k])) <= control$inner.eps) break
      }
    }
  }

  # Unscale the coefficients
  coefficients <- cbind(eta, beta)
  coefficients <- sweep(coefficients, 1, x_scale, `/`)

  colnames(coefficients) <- c(group_levels_drop, target)
  rownames(coefficients) <- colnames(x)

  eta <- coefficients[, 1:(n_groups - 1)]
  beta <- coefficients[, n_groups]
  coefficients <- as.vector(coefficients)

  # Return the fit
  fit <- list(
    coefficients = coefficients, beta = beta, eta = eta,
    group_levels = group_levels, logLik = -loss, iter = record$n_iterations,
    message = record$message, target = target, penalty = penalty,
    lambda1 = lambda1, lambda2 = lambda2, gamma = gamma, formula = formula,
    call = match.call()
  )
  class(fit) <- "coxtl"
  return(fit)
}
