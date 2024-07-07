#' Multiple-sources Cox proportional hazards model with group sparsity
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param lambda a non-negative value specifying the sparse penalty
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
#' fit <- coxgrp(formula, sim2, group, lambda = 0.07, penalty = "SCAD")
#' fit$coefficients
coxgrp <- function(
    formula, data, group, lambda = 0,
    penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), rho = 2.0, init, control, ...) {
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

  risk_set_size <- ave(rep(1, n_samples), group, FUN = cumsum)
  for (k in 1:n_groups) {
    idx <- group_idxs[[k]]
    risk_set_size[idx] <- ave(risk_set_size[idx], time[idx], FUN = max)
  }
  null_deviance <- -sum(status * log(risk_set_size))

  ## Check the penalty argument
  penalty <- match.arg(penalty)
  lambda <- lambda * sqrt(n_samples)

  ## Check the init argument
  if (!missing(init) && length(init) > 0) {
    if (length(init) != n_features * n_groups) {
      stop("Wrong length for inital values")
    }
  } else {
    init <- rep(0, n_features * n_groups)
  }
  ## Check the control argument
  if (missing(control)) control <- survtrans_control(...)

  # Extract the coefficients from init vector
  init <- matrix(init, nrow = n_features)
  init <- sweep(init, 1, x_scale, `*`)
  beta <- init[, 1:n_groups, drop = FALSE]

  # Initialize the training process
  n_iterations <- 0
  message <- ""
  convergence <- FALSE

  offset <- numeric(n_samples)
  w <- numeric(n_samples)
  z <- numeric(n_samples)

  alpha <- matrix(0, nrow = n_features, ncol = n_groups)
  mu <- matrix(0, nrow = n_features, ncol = n_groups)
  vartheta <- 1

  repeat {
    n_iterations <- n_iterations + 1

    # Calculate the theta
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      offset[idx] <- x[idx, ] %*% beta[, k]
    }

    # Calculate the weights and residuals
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      wls <- calc_weights_residuals(
        offset = offset[idx], time = time[idx], status = status[idx]
      )
      w[idx] <- wls$weights
      z[idx] <- wls$residuals + offset[idx]
    }

    # Update the coefficients
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      xw <- x[idx, ] * w[idx] / n_samples
      xwx <- t(xw) %*% x[idx, ]
      xwz <- t(xw) %*% z[idx]
      beta[, k] <- solve(xwx + vartheta * diag(n_features)) %*%
        (xwz + vartheta * alpha[, k] - mu[, k])
    }

    # Update the auxiliary variables
    alpha_old <- alpha
    for (j in 1:n_features) {
      alphaj <- beta[j, ] + mu[j, ] / vartheta
      alphaj_norm <- norm(matrix(alphaj), type = "e")
      alpha[j, ] <- threshold(alphaj_norm, vartheta, penalty, lambda, gamma) *
        alphaj / alphaj_norm
    }
    mu <- mu + vartheta * (beta - alpha)

    # Update the penalty parameter
    r <- norm(beta - alpha, type = "e")
    s <- norm(alpha - alpha_old, type = "e")
    if (r > 10 * s) vartheta <- vartheta * rho
    if (r < 10 * s) vartheta <- vartheta / rho
    vartheta <- max(1, vartheta)

    # Check the convergence
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      offset[idx] <- x[idx, ] %*% beta[, k]
    }
    hazard <- exp(offset)
    risk_set <- ave(hazard, group, FUN = cumsum)
    loss <- -sum(status * (offset - log(risk_set)))

    if (n_iterations >= control$maxit) {
      convergence <- TRUE
      message <- paste(
        "Maximum number of iterations reached (", control$maxit, ")."
      )
    }
    if (r < control$eps && s < control$eps) {
      convergence <- TRUE
      message <- paste(
        "Convergence reached at iteration", n_iterations, "."
      )
    }
    if (-loss / null_deviance < 0.01) {
      convergence <- TRUE
      message <- paste(
        "The log-likelihood is too small (", -loss / null_deviance,
        "). Stopping the algorithm."
      )
    }
    if (convergence) break
  }

  # Unscale the coefficients
  coefficients <- sweep(alpha, 1, x_scale, `/`)
  colnames(coefficients) <- group_levels
  rownames(coefficients) <- colnames(x)

  # Return the fitted model
  fit <- list(
    coefficients = coefficients, group_levels = group_levels, logLik = -loss,
    iter = n_iterations, message = message, penalty = penalty,
    lambda = lambda, gamma = gamma, formula = formula, call = match.call()
  )
  class(fit) <- "coxgrp"
  return(fit)
}
