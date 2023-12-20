#' Non-convex penalized Cox proportional hazards model
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param offset a numeric vector specifying the offset.
#' @param penalty a character string specifying the penalty function.
#' The default is "lasso". Other options are "MCP" and "SCAD".
#' @param lambda a non-negative value specifying the penalty parameter. The
#' default is 0.
#' @param gamma a non-negative value specifying the penalty parameter. The
#' default is 3.7 for SCAD and 1.5 for MCP.
#' @param init a numeric vector specifying the initial value of the
#' coefficients. The default is a zero vector.
#' @param control Object of class \link{survtrans.control} containing
#'  control parameters for the fitting algorithm. Default is
#' \code{survtrans.control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans.control}}.
#' @importFrom stats model.frame model.matrix model.response ave
#' @export
#' @examples
#' library(survtrans)
#' ncvcox(Surv(time, status) ~ ., data = sim1_src, lambda = 0.1)
ncvcox <- function(
    formula, data, offset, penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), lambda = 0, init, control, ...) {
  penalty <- match.arg(penalty)
  if (missing(formula)) stop("a formula argument is required")

  # Load X, y from formula and data
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  x <- model.matrix(formula, data)
  x <- x[, -1] # Remove the intercept column

  # Standardize the covariates
  x <- scale(x)
  x_center <- attr(x, "scaled:center")
  x_scale <- attr(x, "scaled:scale")

  # Properties of the data
  n_samples <- nrow(x)
  n_features <- ncol(x)

  # Sort the data by time
  time <- y[, 1]
  status <- y[, 2]
  sorted <- order(time, decreasing = TRUE)
  time <- time[sorted]
  status <- status[sorted]
  x <- x[sorted, , drop = FALSE]

  # Check the offset argument
  if (missing(offset)) offset <- rep(0.0, n_samples)
  offset <- offset[sorted]

  # Check the init argument
  if (!missing(init) && length(init) > 0) {
    if (length(init) != n_features) {
      stop("Wrong length for inital values")
    }
  } else {
    init <- rep(0.0, n_features)
  }

  # Check the control argument
  if (missing(control)) control <- survtrans.control(...)

  # Initialize the coefficients
  n_iterations <- 0
  coef <- init
  repeat {
    n_iterations <- n_iterations + 1

    # calculate the weights and residuals
    temp <- calc_weights_residuals(coef, x, time, status, offset)
    weights <- temp$weights
    residuals <- temp$residuals

    # update the coefficients
    coef_old <- coef
    for (j in 1:n_features) {
      v <- mean(weights * x[, j]**2)
      z <- mean(x[, j] * weights * residuals) + coef[j] * v
      coef[j] <- soft_threshold(z, v, penalty, lambda, gamma)
    }

    # check for convergence
    if (n_iterations >= control$maxit) {
      warning("Maximum number of iterations reached")
      break
    }
    if (max(abs(coef - coef_old)) <= control$eps) {
      break
    }
  }

  # Calculate the log-likelihood
  eta <- x %*% coef + offset
  haz <- exp(eta)
  risk_set <- cumsum(haz)
  risk_set <- ave(risk_set, time, FUN = max)
  log_lik <- sum(status * (eta - log(risk_set)))

  # Unstandardize the coefficients
  coef <- coef / x_scale

  # Return the fit
  fit <- list(
    logLik = log_lik, coefficients = coef,
    penalty = penalty, lambda = lambda, gamma = gamma,
    iter = n_iterations, formula = formula, call = match.call()
  )
  class(fit) <- "ncvcox"
  return(fit)
}
