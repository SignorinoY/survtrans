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
#'   lambda1 = 1.5, lambda2 = 2.3, penalty = "SCAD"
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
  n_parameters <- n_groups^2 + 2

  risk_set_size <- ave(rep(1, n_samples), group, FUN = cumsum)
  for (k in 1:n_groups) {
    idx <- which(group == group_levels[k])
    risk_set_size[idx] <- ave(risk_set_size[idx], time[idx], FUN = max)
  }
  null_deviance <- -sum(status * log(risk_set_size))

  penalty <- match.arg(penalty)

  # Check the init argument
  if (!missing(init) && length(init) > 0) {
    if (length(init) != n_features * n_parameters) {
      stop("Wrong length for inital values")
    }
  } else {
    init <- rep(0, n_features * n_parameters)
  }

  # Check the control argument
  if (missing(control)) control <- survtrans_control(...)

  # Initialize the return fit object
  fit <- list(
    penalty = penalty, lambda1 = lambda1, lambda2 = lambda2, gamma = gamma,
    rho = rho, formula = formula, call = match.call(),
    group_levels = group_levels
  )
  class(fit) <- "coxtrans"

  # Extract the coefficients from init vector
  coefficients <- matrix(init, nrow = n_features)
  coefficients <- sweep(coefficients, 1, x_scale, `*`)
  eta <- coefficients[, 1:n_groups, drop = FALSE]
  beta <- coefficients[, n_groups + 1]
  xi <- coefficients[
    , (n_groups + 2):(n_groups * (n_groups + 1) / 2 + 1),
    drop = FALSE
  ]
  mu <- coefficients[, n_groups * (n_groups + 1) / 2 + 2, drop = FALSE]
  nu <- coefficients[
    , (n_groups * (n_groups + 1) / 2 + 3):n_parameters,
    drop = FALSE
  ]
  alpha <- 1

  # Initialize the coefficients
  coef_ <- cbind(eta, beta, xi)
  record <- list(
    convergence = FALSE, n_iterations = 0, n_iterations_no_improvement = 0,
    best_loss = Inf, coef = coef_, null_deviance = null_deviance
  )
  weights <- rep(0, n_samples)
  residuals <- rep(0, n_samples)

  repeat {
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

    # Calculate the weights and residuals
    offset <- x %*% beta
    for (i in 1:n_groups) {
      idx <- which(group == group_levels[i])
      offset[idx] <- offset[idx] + x[idx, ] %*% eta[, i]
    }
    for (k in 1:n_groups) {
      idx <- which(group == group_levels[k])
      wls <- calc_weights_residuals(
        offset = offset[idx], time = time[idx], status = status[idx]
      )
      weights[idx] <- wls$weights
      residuals[idx] <- wls$residuals
    }

    # Update beta by weighted least squares
    r <- residuals + x %*% beta
    w <- diag(weights)
    beta <- solve(t(x) %*% w %*% x) %*% t(x) %*% w %*% r

    # Update eta by cyclic coordinate descent
    r <- residuals + offset - x %*% beta
    w <- weights
    for (i in 1:n_groups) {
      idx <- which(group == group_levels[i])
      for (iter in 1:control$maxit) {
        eta_old <- eta[, i]
        features_idx <- sample(seq_len(n_features), n_features, replace = FALSE)
        for (k in features_idx) {
          xk <- as.vector(x[idx, k])
          phi <- sum(w[idx] * xk * (r[idx] - x[idx, -k] %*% eta[-k, i])) -
            mu[k] - nu_sum[k, i] + alpha * xi_sum[k, i]
          psi <- sum(w[idx] * xk^2) + alpha * n_groups
          eta[k, i] <- soft_threshold(phi, psi, penalty, lambda1, gamma)
        }
        if (max(abs(eta_old - eta[, i])) <= control$eps) break
      }
    }

    # Update xi
    for (i in 1:n_groups) {
      for (j in 1:n_groups) {
        if (i >= j) next
        pos <- get_position(i, j, n_groups)
        zeta <- eta[, i] - eta[, j] + nu[, pos] / alpha
        for (k in 1:n_features) {
          xi[k, pos] <- soft_threshold(
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

    if (record$n_iterations_no_improvement || iter == control$maxit) {
      alpha <- min(alpha * rho, n_samples)
    }

    # Check the convergence
    fit$eta <- sweep(eta, 1, x_scale, `/`)
    fit$beta <- beta / x_scale
    loss <- -logLik(fit, data_, group_)

    coef_ <- cbind(eta, beta, xi)
    coefficients <- coef_
    record <- check_convergence(
      coef = coef_, loss = loss, last_record = record, control = control
    )
    if (record$convergence) break
  }

  # Unstandardize the coefficients
  coef_final <- sweep(coef_, 1, x_scale, `/`)
  eta <- coef_final[, 1:n_groups]
  beta <- coef_final[, n_groups + 1]
  xi <- coef_final[, (n_groups + 2):(n_groups * (n_groups + 1) / 2 + 1)]

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

  eta_bar <- rowMeans(eta_processed)
  eta_processed <- sweep(eta_processed, 1, eta_bar, "-")
  eta_processed[abs(eta_processed) < control$eps] <- 0
  beta <- beta + eta_bar

  coef <- cbind(eta, beta, xi, mu, nu)
  coef <- sweep(coef, 1, x_scale, `/`)
  coef <- as.vector(coef)
  # Return the fit
  fit <- list(
    coefficients = coef, logLik = -loss,
    beta = beta, eta = eta_processed, eta_group = eta_group, xi = xi,
    mu = mu, nu = nu, alpha = alpha, group_levels = group_levels,
    penalty = penalty, lambda1 = lambda1, lambda2 = lambda2, gamma = gamma,
    rho = rho, formula = formula, call = match.call(),
    iter = record$n_iterations, message = record$message
  )
  class(fit) <- "coxtrans"
  return(fit)
}
