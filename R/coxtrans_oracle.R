#' Transfer Learning of Multi-source for Cox proportional hazards model (Oracle)
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param sparse_idx a logical matrix specifying the sparse parameters, where
#' each row corresponds to a feature and each column corresponds to a group.
#' @param group_idx a logical matrix specifying the group parameters, where
#' each row corresponds to a feature and each column corresponds to a group.
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
#' formula <- Surv(time, status) ~ . - group - id
#' sparse_idx <- matrix(
#'   rep(rep(c(FALSE, TRUE), each = 10), 5),
#'   ncol = 10, byrow = TRUE
#' )
#' group_idx <- matrix(
#'   rep(c(rep(1, 10), rep(c(1, 2), 5)), 5),
#'   ncol = 10, byrow = TRUE
#' )
#' fit <- coxtrans_oracle(
#'   formula, sim2, as.factor(sim2$group), sparse_idx, group_idx
#' )
#' fit$eta
coxtrans_oracle <- function( # nolint: cyclocomp_linter.
    formula, data, group, sparse_idx, group_idx, rho = 2.0, init,
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
  n_parameters <- n_groups + 1

  # Check the sparse_idx argument
  if (missing(sparse_idx)) {
    sparse_idx <- matrix(TRUE, nrow = n_features, ncol = n_groups)
  }
  if (missing(group_idx)) {
    group_idx <- matrix(rep(1:n_groups, each = n_features), ncol = n_groups)
  }

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

  fn <- function(param) {
    # Extract the parameters
    coefs <- matrix(param, nrow = n_features)
    eta <- coefs[, 1:n_groups]
    beta <- coefs[, n_parameters]

    # Calculate the log-likelihood
    theta <- x %*% beta
    for (k in 1:n_groups) {
      idx <- group == group_levels[k]
      theta[idx] <- theta[idx] + x[idx, ] %*% eta[, k]
    }
    theta_max <- max(theta)
    hazard <- exp(theta - theta_max)
    hazard <- exp(theta - theta_max)
    risk_set <- ave(hazard, group, FUN = cumsum)
    for (k in 1:n_groups) {
      ind <- group == group_levels[k]
      risk_set[ind] <- ave(risk_set[ind], time[ind], FUN = max)
    }
    log_risk <- log(risk_set) + theta_max
    log_lik <- sum(status * (theta - log_risk))

    # Calculate the augmented term
    constr <- rowSums(eta)
    aug <- sum(constr * mu) + sum(rowSums(eta)^2) * alpha / 2

    -log_lik + aug
  }

  gr <- function(param) {
    # Extract the parameters
    coefs <- matrix(param, nrow = n_features)
    eta <- coefs[, 1:n_groups]
    beta <- coefs[, n_parameters]


    theta <- x %*% beta
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

    grad_beta <- -colSums(grads)
    grad_eta <- matrix(0, nrow = n_features, ncol = n_groups)
    for (k in 1:n_groups) {
      idx <- group == group_levels[k]
      grad_eta[, k] <- -colSums(grads[idx, ])
    }
    grad_eta <- grad_eta + mu + rowSums(eta) * alpha
    # Mask the sparse parameters
    grad_eta[sparse_idx == 0] <- 0
    # Mask the group parameters
    for (k in 1:n_features) {
      values <- unique(group_idx[k, ])
      for (j in values) {
        idx <- group_idx[k, ] == j
        grad_eta[k, idx] <- sum(grad_eta[k, idx])
      }
    }

    grad <- cbind(grad_eta, grad_beta)
    grad <- as.vector(grad)
    return(grad)
  }


  alpha <- 1
  mu <- rep(0, n_features)
  param_old <- init
  fn_best <- fn(param_old)

  n_iterations_no_improvement <- 0
  for (iter in 1:control$maxit) {
    res <- optim(param_old, fn, gr, method = "L-BFGS-B")
    if (max(abs(res$par - param_old)) < control$eps) break
    param_old <- res$par
    coefs <- matrix(res$par, nrow = n_features)
    mu <- mu + alpha * rowSums(coefs[, 1:n_groups])
    if (fn_best < res$value) {
      fn_best <- res$value
    } else {
      alpha <- min(alpha * rho, n_samples)
      n_iterations_no_improvement <- n_iterations_no_improvement + 1
    }
    if (n_iterations_no_improvement >= control$patience) {
      break
    }
  }
  eta <- coefs[, 1:n_groups]
  beta <- coefs[, n_parameters]
  eta_bar <- rowMeans(eta)
  eta <- sweep(eta, 1, eta_bar, "-")
  eta[abs(eta) < control$eps] <- 0
  beta <- beta + eta_bar

  # Unscale the coefficients
  coef <- cbind(eta, beta, mu)
  coef <- sweep(coef, 1, x_scale, "/")
  eta <- coef[, 1:n_groups]
  beta <- coef[, n_parameters]
  coef <- as.vector(coef)

  # Return the fit
  fit <- list(
    coefficients = coef, logLik = -res$value, iter = iter,
    beta = beta, eta = eta, mu = mu,
    rho = rho, formula = formula, call = match.call()
  )
  class(fit) <- "coxtrans"
  return(fit)
}
