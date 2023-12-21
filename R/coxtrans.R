#' @importFrom survival Surv
#' @importFrom stats model.frame model.matrix model.response ave
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

  # Load X, y from formula and data
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  x <- model.matrix(formula, data)
  x <- x[, -1] # Remove the intercept column

  # Standardize the covariates
  x <- scale(x)
  x_scale <- attr(x, "scaled:scale")

  # Check the group argument
  if (!is.factor(group)) group <- factor(group)

  # Properties of the data
  n_samples <- nrow(x)
  n_features <- ncol(x)
  n_groups <- length(unique(group))
  group_levels <- levels(group)

  # Sort the data by time
  time <- y[, 1]
  status <- y[, 2]
  sorted <- order(group, time, decreasing = c(FALSE, TRUE))
  time <- time[sorted]
  status <- status[sorted]
  x <- x[sorted, , drop = FALSE]
  group <- group[sorted]

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
  coef <- init
  n_iterations <- 0
  best_log_lik <- -Inf
  n_iterations_no_improvement <- 0
  weights <- rep(0, n_samples)
  residuals <- rep(0, n_samples)
  offset <- rep(0, n_samples)
  repeat {
    n_iterations <- n_iterations + 1

    coef_old <- coef

    # Calculate the weights and residuals
    for (k in 1:n_groups) {
      ind <- which(group == group_levels[k])
      temp <- calc_weights_residuals(
        coef = rep(0, n_features), x = x[ind, ], time = time[ind],
        status = status[ind], offset = offset[ind]
      )
      weights[ind] <- temp$weights
      residuals[ind] <- temp$residuals
    }

    # Update the common coefficients
    w <- diag(weights)
    r <- residuals
    coef_imporve <- solve(t(x) %*% w %*% x) %*% t(x) %*% w %*% r
    if (n_iterations_no_improvement > 0) {
      coef_imporve <- coef_imporve * 0.5
      print(coef_imporve)
    }
    coef[, n_groups + 1] <- coef[, n_groups + 1] + coef_imporve
    offset <- offset + x %*% coef_imporve

    # Update the coefficients for each group
    for (k in 1:n_groups) {
      ind <- which(group == group_levels[k])

      w <- weights[ind]
      r <- residuals[ind]

      coef_ <- coef[, k]
      for (j in 1:n_features) {
        v <- mean(w * x[ind, j]**2)
        z <- mean(x[ind, j] * w * r) + coef[j, k] * v
        coef[j, k] <- soft_threshold(z, v, penalty, lambda1, gamma)
      }
      offset[ind] <- offset[ind] + x[ind, ] %*% (coef[, k] - coef_)
    }

    # Centralize the coefficients
    coef_group <- coef[, 1:n_groups]
    coef_center <- rowMeans()
    coef[, 1:n_groups] <- sweep(coef_group, MARGIN = 1, coef_center, `-`)
    coef[, n_groups + 1] <- coef[, n_groups + 1] + coef_center

    # Calculate the log-likelihood
    haz <- exp(offset)
    risk_set <- ave(haz, group, FUN = cumsum)
    risk_set <- ave(risk_set, group, time, FUN = max)
    log_lik <- sum(status * (offset - log(risk_set)))

    # Check the convergence
    if (n_iterations >= control$maxit) {
      warning("Maximum number of iterations reached")
      break
    }
    if (max(abs(coef_old - coef)) <= control$eps) break

    if (log_lik <= best_log_lik) {
      n_iterations_no_improvement <- n_iterations_no_improvement + 1
    }
    if (log_lik > best_log_lik) {
      best_log_lik <- log_lik
      best_coef <- coef
      n_iterations_no_improvement <- 0
    }

  }

  # Unstandardize the coefficients
  best_coef <- sweep(best_coef, MARGIN = 1, x_scale, `/`)

  # Return the fit
  fit <- list(
    coef = best_coef, logLik = best_log_lik,
    penalty = penalty, lambda1 = lambda1, gamma = gamma,
    iter = n_iterations, formula = formula, call = match.call()
  )
  class(fit) <- "coxtrans"
  return(fit)
}
