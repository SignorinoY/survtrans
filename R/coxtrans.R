#' @importFrom survival Surv
#' @importFrom stats model.frame model.matrix model.response ave
coxtrans <- function(
    formula, data, group, lambda1 = 0, lambda2 = 0,
    penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), # styler: ignore
    init, control, ...) {
  penalty <- match.arg(penalty)
  if (missing(control)) control <- survtrans_control(...)
  if (missing(formula)) stop("a formula argument is required")

  # Load response and design matrix from the data
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  x <- model.matrix(formula, data)
  x <- x[, -1] # Remove the intercept column

  # Standardize the covariates
  x <- scale(x)
  x_scale <- attr(x, "scaled:scale")

  group <- factor(group)

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
  group <- group[sorted]
  x <- x[sorted, , drop = FALSE]

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


  # Initialize the coefficients
  n_iterations <- 0
  coef <- init
  repeat {
    n_iterations <- n_iterations + 1

    offset <- x %*% coef[, n_groups + 1]

    weights <- rep(0.0, n_samples)
    residuals <- rep(0.0, n_samples)
    for (k in 1:n_groups) {
      ind <- which(group == group_levels[k])
      x_ <- x[ind, , drop = FALSE]
      time_ <- time[ind]
      status_ <- status[ind]
      offset_ <- offset[ind]

      temp <- calc_weights_residuals(
        coef[, k], x_, time_, status_, offset_
      )
      weights_ <- temp$weights
      residuals_ <- temp$residuals

      for (j in 1:n_features) {
        v <- sum(weights_ * x_[, j]**2)
        z <- sum(x_[, j] * weights_ * residuals_) + coef[j, k] * v
        coef[j, k] <- soft_threshold(z, v, penalty, lambda1, gamma)
      }

      weights[ind] <- weights_
      residuals[ind] <- residuals_
    }

    # Update the offset
    w <- diag(weights)
    coef_ <- solve(t(x) %*% w %*% x) %*% t(x) %*% w %*% residuals
    coef[, n_groups + 1] <- coef[, n_groups + 1] + coef_
    coef <- coef / x_scale
  }
}
