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
#' @examples
#' library(survtrans)
#' formula <- Surv(time, status) ~ . - group - id
#' group <- as.factor(sim1$group)
#' fit.src <- ncvcox(
#'   formula, sim1[group == 1, ],
#'   lambda = 0.1, penalty = "SCAD"
#' )
#' fit.src$coefficients
#' offset.trg <- predict(fit.src, newdata = sim1[group == 2, ], type = "lp")
#' fit.trg <- ncvcox(
#'   formula, sim1[group == 2, ],
#'   offset = offset.trg, lambda = 0.2, penalty = "SCAD"
#' )
#' fit.trg$coefficients
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
      stop("Wrong length for inital values")
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

  offset <- numeric(n_samples)
  w <- numeric(n_samples)
  r <- numeric(n_samples)

  # Pre-calculate the quantities
  x2 <- x**2

  repeat {
    n_iterations <- n_iterations + 1
    last_coefficients <- coefficients

    # Calculate the weights and residuals
    offset <- x %*% coefficients
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      wls <- calc_weights_residuals(
        offset = offset[idx], time = time[idx], status = status[idx]
      )
      w[idx] <- wls$weights
      r[idx] <- wls$residuals
    }

    # Update coefficients by cyclic coordinate descent
    xw <- x * w
    xwx <- colMeans(w * x2)
    for (i in seq_len(control$inner.maxit)) {
      coefficients_old <- coefficients
      delta_max <- 0
      for (j in seq_len(n_features)) {
        xwr <- mean(xw[, j] * r)
        coefficients[j] <- penalty_solution(xwr, xwx[j], penalty, lambda, gamma)
        delta <- coefficients[j] - coefficients_old[j]
        r <- r - x[, j] * delta
        delta_max <- max(delta_max, abs(delta))
      }
      if (delta_max < control$inner.eps) break
    }

    # Check the convergence
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
    offset <- x %*% coefficients
    offset_max <- max(offset)
    hazard <- exp(offset - offset_max)
    risk_set <- ave(hazard, group, FUN = cumsum)
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      risk_set[idx] <- ave(risk_set[idx], time[idx], FUN = max)
    }
    loss <- -sum(status * (offset - log(risk_set) - offset_max))
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
  coefficients[abs(coefficients) < control$eps] <- 0
  coefficients <- coefficients / x_scale
  names(coefficients) <- colnames(x)

  # Return the fit
  fit <- list(
    coefficients = coefficients, group_levels = group_levels,
    logLik = -loss, iter = n_iterations, message = message,
    penalty = penalty, lambda = lambda, gamma = gamma, formula = formula,
    call = match.call()
  )
  class(fit) <- "ncvcox"
  return(fit)
}
