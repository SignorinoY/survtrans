#' Non-convex penalized Cox proportional hazards model
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param offset a numeric vector specifying the offset..
#' @param lambda a non-negative value specifying the penalty parameter. The
#' default is 0.
#' @param penalty a character string specifying the penalty function.
#' The default is "lasso". Other options are "MCP" and "SCAD"
#' @param gamma a non-negative value specifying the penalty parameter. The
#' default is 3.7 for SCAD and 3.0 for MCP.
#' @param init a numeric vector specifying the initial value of the
#' coefficients. The default is a zero vector.
#' @param control Object of class \link{survtrans_control} containing
#'  control parameters for the fitting algorithm. Default is
#' \code{survtrans_control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans_control}}.
#' @return a ncvcox object.
#' @export
ncvcox <- function(
    formula, data, group, offset, lambda = 0,
    penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), init, control, ...) {
  # Load the data
  data <- preprocess_data(formula, data, group, offset)
  x <- data$x
  x_scale <- attr(x, "scale")
  time <- data$time
  status <- data$status
  group <- data$group
  offset <- data$offset

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

  # Check the penalty argument
  penalty <- match.arg(penalty, choices = c("lasso", "MCP", "SCAD"))

  # Check the init argument
  if (!missing(init) && length(init) > 0) {
    if (length(init) != n_features) {
      stop("Wrong length for initial values")
    }
  } else {
    init <- numeric(n_features)
  }

  # Check the control argument
  if (missing(control)) control <- survtrans_control(...)

  # Initialize the training process
  n_iterations <- 0
  message <- ""
  convergence <- FALSE
  coefficients <- init

  lp <- offset
  w <- numeric(n_samples)
  r <- numeric(n_samples)

  x2 <- x**2

  repeat {
    n_iterations <- n_iterations + 1
    last_coefficients <- coefficients

    # Calculate the weights and residuals
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      wls <- approx_likelihood(
        offset = lp[idx], time = time[idx], status = status[idx]
      )
      w[idx] <- wls$weights
      r[idx] <- wls$residuals
    }
    xw <- x * w
    xwx <- colMeans(x2 * w)

    # Update coefficients by cyclic coordinate descent
    active_set <- c()
    features_idx <- sample(seq_len(n_features), n_features, FALSE)
    for (j in features_idx) {
      coef_j <- coefficients[j]
      z <- mean(xw[, j] * r) + coefficients[j] * xwx[j]
      coefficients[j] <- close_update(z, xwx[j], penalty, lambda, gamma)
      delta_coef <- coefficients[j] - coef_j
      if (delta_coef != 0) {
        r <- r - x[, j] * delta_coef
        active_set <- c(active_set, j)
      }
    }
    for (i in 1:control$inner.maxit) {
      max_diff <- 0
      for (j in active_set) {
        coef_j <- coefficients[j]
        z <- mean(xw[, j] * r) + coefficients[j] * xwx[j]
        coefficients[j] <- close_update(z, xwx[j], penalty, lambda, gamma)
        delta_coef <- coefficients[j] - coef_j
        if (delta_coef != 0) {
          r <- r - x[, j] * delta_coef
          max_diff <- max(max_diff, abs(delta_coef))
        }
      }
      if (max_diff <= control$inner.eps) break
    }

    lp <- x %*% coefficients + offset

    # Calculate the log-likelihood
    hazard <- exp(lp)
    risk_set <- ave(hazard, group, FUN = cumsum)
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      risk_set[idx] <- ave(risk_set[idx], time[idx], FUN = max)
    }
    loss <- -sum(status * (lp - log(risk_set)))

    # Check the convergence
    if (is.infinite(loss) || is.nan(loss)) {
      stop("The log-likelihood is not finite. Stopping the algorithm.")
    }
    if (n_iterations >= control$maxit) {
      convergence <- TRUE
      message <- paste0(
        "Maximum number of iterations reached (", control$maxit, ")."
      )
    }
    if (max(abs(coefficients - last_coefficients)) <= control$eps) {
      convergence <- TRUE
      message <- paste0(
        "Convergence reached at iteration ", n_iterations, "."
      )
    }
    if (-loss / null_deviance < 0.01) {
      convergence <- TRUE
      message <- paste0(
        "The log-likelihood is too small (", -loss / null_deviance,
        "). Stopping the algorithm."
      )
    }
    if (convergence) break
  }

  # Unstandardize the coefficients
  coefficients <- coefficients / x_scale
  names(coefficients) <- colnames(x)

  # Return the fit
  fit <- list(
    coefficients = coefficients,
    logLik = -loss,
    iter = n_iterations,
    message = message,
    penalty = penalty,
    lambda = lambda,
    gamma = gamma,
    formula = formula,
    call = match.call(),
    time = time,
    status = status,
    group = group,
    x = sweep(x, 2, x_scale, `/`),
    offset = offset
  )
  class(fit) <- "ncvcox"
  return(fit)
}
