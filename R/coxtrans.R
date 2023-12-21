#' Transfer Learning of Multi-source for Cox proportional hazards model
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param lambda1 a non-negative value specifying the sparisity penalty
#' parameter. The default is 0.
#' @param lambda2 a non-negative value specifying the group penalty
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
coxtrans <- function(
    formula, data, group, lambda1 = 0, lambda2 = 0,
    penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ),
    init, control, ...) {
  penalty <- match.arg(penalty)

  data_ <- preprocess_data(formula, data, group = group)
  x <- data_$x
  x_scale <- attr(x, "scale")
  time <- data_$time
  status <- data_$status
  group <- data_$group

  # Properties of the data
  n_samples <- nrow(x)
  n_features <- ncol(x)
  n_groups <- length(unique(group))
  group_levels <- levels(group)

  # Check the init argument
  if (!missing(init) && length(init) > 0) {
    if (length(init) != n_features * (n_groups + 1)) {
      stop("Wrong length for inital values")
    }
  } else {
    init <- matrix(
      rep(0, n_features * (n_groups + 1)),
      nrow = n_features
    )
  }

  # Check the control argument
  if (missing(control)) control <- survtrans_control(...)

  # Initialize the coefficients
  record <- list(
    convergence = FALSE, n_iterations = 0, n_iterations_no_improvement = 0,
    best_loss = Inf, best_coef = init, coef = init
  )

  coef <- init
  weights <- rep(0, n_samples)
  residuals <- rep(0, n_samples)
  offset <- rep(0, n_samples)

  repeat {
    # Calculate the weights and residuals
    # Update the coefficients for each group
    for (k in 1:n_groups) {
      ind <- which(group == group_levels[k])
      temp <- calc_weights_residuals(
        coef = rep(0, n_features), x = x[ind, ], time = time[ind],
        status = status[ind], offset = offset[ind]
      )
      weights[ind] <- temp$weights
      residuals[ind] <- temp$residuals

      w <- weights[ind]
      r <- residuals[ind]
      coef_ <- coef[, k]
      for (j in 1:n_features) {
        v <- sum(w * x[ind, j]**2) / n_samples
        z <- sum(x[ind, j] * w * r) / n_samples + coef[j, k] * v
        coef[j, k] <- soft_threshold(z, v, penalty, lambda1, gamma)
      }
      offset[ind] <- offset[ind] + x[ind, ] %*% (coef[, k] - coef_)
    }

    # Update the common coefficients
    w <- diag(weights)
    r <- residuals
    coef_increment <- solve(t(x) %*% w %*% x) %*% t(x) %*% w %*% r

    decay_factor <- 0.1**record$n_iterations_no_improvement
    coef_increment <- coef_increment * decay_factor
    coef[, n_groups + 1] <- coef[, n_groups + 1] + coef_increment
    offset <- offset + x %*% coef_increment

    # Centralize the coefficients
    coef_group <- coef[, 1:n_groups]
    coef_center <- rowMeans(coef_group)
    coef[, 1:n_groups] <- sweep(coef_group, MARGIN = 1, coef_center, `-`)
    coef[, n_groups + 1] <- coef[, n_groups + 1] + coef_center

    # Calculate the log-likelihood
    haz <- exp(offset)
    risk_set <- ave(haz, group, FUN = cumsum)
    for (k in 1:n_groups) {
      ind <- which(group == group_levels[k])
      risk_set[ind] <- ave(risk_set[ind], time[ind], FUN = max)
    }
    log_lik <- sum(status * (offset - log(risk_set)))

    # Check the convergence
    record <- check_convergence(
      coef = coef, loss = -log_lik, last_record = record, control = control
    )
    if (record$convergence) break
  }

  # Unstandardize the coefficients
  coef_final <- sweep(record$best_coef, MARGIN = 1, x_scale, `/`)

  # Return the fit
  fit <- list(
    coef = coef_final, logLik = -record$best_loss,
    penalty = penalty, lambda1 = lambda1, gamma = gamma,
    iter = record$n_iterations, formula = formula, call = match.call()
  )
  class(fit) <- "coxtrans"
  return(fit)
}
