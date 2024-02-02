#' Transfer Learning for Cox proportional hazards model
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param target a factor specifying the target group.
#' @param lambda a non-negative value specifying the sparse penalty
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
#' fit <- coxtl(
#'   formula, sim2, as.factor(sim2$group), 10,
#'   lambda = 0.04, penalty = "SCAD"
#' )
#' fit$eta
coxtl <- function( # nolint: cyclocomp_linter.
    formula, data, group, target, lambda = 0,
    penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), init, control, ...) {
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
  group_levels_source <- group_levels[group_levels != target]

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
      idx <- group == group_levels_source[k]
      theta[idx] <- theta[idx] + x[idx, ] %*% eta[, k]
    }

    # Calculate the loss
    hazard <- exp(theta)
    risk_set <- ave(hazard, group, FUN = cumsum)
    loss <- -sum(status * (theta - log(risk_set)))

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

    # Update beta by weighted least squares
    xw <- x * w
    beta <- beta + solve(t(xw) %*% x, t(xw) %*% r)

    # Update eta by cyclic coordinate descent
    r <- r + theta - x %*% beta
    for (k in 1:(n_groups - 1)) {
      idx <- group == group_levels_source[k]
      x_ <- x[idx, , drop = FALSE]
      r_ <- r[idx] - x_ %*% eta[, k]
      xw_ <- x_ * w[idx]
      xwx_ <- colMeans(w[idx] * x2[idx, , drop = FALSE])
      for (iter in 1:control$maxit) {
        eta_old <- eta[, k]
        features_idx <- sample(seq_len(n_features), n_features, FALSE)
        for (j in features_idx) {
          phi <- mean(xw_[, j] * r_) + xwx_[j] * eta[j, k]
          eta[j, k] <- penalty_solution(phi, xwx_[j], penalty, lambda, gamma)
          r_ <- r_ - x_[, j] * (eta[j, k] - eta_old[j])
        }
        if (max(abs(eta_old - eta[, k])) <= control$eps) break
      }
    }
  }

  # Unscale the coefficients
  coefficients <- cbind(eta, beta)
  coefficients <- sweep(coefficients, 1, x_scale, `/`)
  eta <- coefficients[, 1:(n_groups - 1)]
  beta <- coefficients[, n_groups]
  coefficients <- as.vector(coefficients)

  # Return the fit
  fit <- list(
    coefficients = coefficients, beta = beta, eta = eta,
    logLik = -loss, iter = record$n_iterations, message = record$message,
    group_levels = group_levels, target = target, penalty = penalty,
    lambda = lambda, gamma = gamma, formula = formula, call = match.call()
  )
  class(fit) <- "coxtl"
  return(fit)
}
