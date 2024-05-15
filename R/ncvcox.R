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

  # Calculate the null deviance
  risk_set_size <- ave(rep(1, n_samples), group, FUN = cumsum)
  for (k in 1:n_groups) {
    idx <- group_idxs[[k]]
    risk_set_size[idx] <- ave(risk_set_size[idx], time[idx], FUN = max)
  }
  null_deviance <- -sum(status * log(risk_set_size))

  # Check the penalty argument
  penalty <- match.arg(penalty)

  # Check the init argument
  n_features <- ncol(x)
  if (!missing(init) && length(init) > 0) {
    if (length(init) != n_features) {
      stop("Wrong length for inital values")
    }
  } else {
    init <- rep(0.0, n_features)
  }

  # Check the control argument
  if (missing(control)) control <- survtrans_control(...)

  # Initialize the training process
  record <- list(
    convergence = FALSE, n_iterations = 0, null_deviance = null_deviance,
    maxit = control$maxit, eps = control$eps
  )
  beta <- init
  theta <- rep(0.0, n_samples)
  w <- rep(1.0, n_samples)
  r <- rep(0.0, n_samples)

  repeat {
    # Calculate the theta
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      theta[idx] <- x[idx, ] %*% beta + offset[idx]
    }

    # Calculate the loss
    hazard <- exp(theta)
    risk_set <- ave(hazard, group, FUN = cumsum)
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      risk_set[idx] <- ave(risk_set[idx], time[idx], FUN = max)
    }
    loss <- -sum(status * (theta - log(risk_set)))

    # Check the convergence
    record <- check_convergence(beta, loss, record)
    if (record$convergence) break

    # Calculate the weights and residuals
    for (k in 1:n_groups) {
      idx <- group_idxs[[k]]
      wls <- calc_weights_residuals(
        offset = theta[idx], time = time[idx], status = status[idx]
      )
      w[idx] <- wls$weights
      r[idx] <- wls$residuals
    }

    # Update beta by cyclic coordinate descent
    features_idx <- sample(seq_len(n_features), n_features, FALSE)
    for (j in features_idx) {
      v <- mean(w * x[, j]**2)
      z <- mean(x[, j] * w * r) + beta[j] * v
      beta[j] <- penalty_solution(z, v, penalty, lambda, gamma)
    }
  }

  # Unstandardize the coefficients
  beta <- beta / x_scale

  # Return the fit
  fit <- list(
    coefficients = beta, group_levels = group_levels, logLik = -loss,
    penalty = penalty, lambda = lambda, gamma = gamma,
    iter = record$n_iterations, formula = formula, call = match.call()
  )
  class(fit) <- "ncvcox"
  return(fit)
}
