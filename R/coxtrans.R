#' Transfer Learning for Cox Proportional Hazards Model
#' @param data a data frame containing the variables in the model.
#' @param fit_source a coxph object fitted to the source data.
#' @param penalty a character string specifying the penalty function.
#' The default is "lasso". Other options are "MCP" and "SCAD".
#' @param lambda a non-negative value specifying the penalty parameter. The
#' default is 0.
#' @param gamma a non-negative value specifying the penalty parameter. The
#' default is 3.7 for SCAD and 1.5 for MCP.
#' @param init a numeric vector specifying the initial value of the
#' coefficients. The default is a zero vector.
#' @param control Object of class \link{survtrans_control} containing
#'  control parameters for the fitting algorithm. Default is
#' \code{survtrans_control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans_control}}.
#' @export
#' @examples
#' library(survival)
#' library(survtrans)
#' fit_src <- coxph(Surv(time, status) ~ ., data = sim1_src)
#' coxtrans(sim1_trg, fit_src, lambda = 0.1)
coxtrans <- function(
    data, fit_source, penalty = "lasso", lambda = 0, gamma = NULL,
    init, control, ...) {
  if (missing(data)) stop("a data argument is required")
  # if (!inherits(fit_source, "survode")) {
  #   stop("fit_source must be survode object")
  # }

  form_src <- fit_source$formula
  coef_src <- fit_source$coefficients

  mf <- stats::model.frame(form_src, data)
  y <- stats::model.response(mf)
  x <- stats::model.matrix(form_src, data)
  x <- x[, !colnames(x) %in% "(Intercept)"]
  if (!is.matrix(x)) x <- as.matrix(x)
  nobs <- nrow(x)
  nvar <- ncol(x)
  if (missing(init)) init <- rep(0, nvar)
  if (missing(control)) control <- survtrans_control(...)

  if (is.null(gamma)) {
    gamma <- switch(penalty,
      MCP = 1.5,
      SCAD = 3.7,
      1
    )
  }

  time <- y[, 1]
  status <- y[, 2]

  sorted <- order(time, decreasing = TRUE)
  time <- time[sorted]
  status <- status[sorted]
  x <- x[sorted, ]
  offset <- x %*% coef_src

  eta <- init
  iter <- 0
  repeat {
    iter <- iter + 1
    eta_old <- eta
    theta <- x %*% eta
    phi <- theta + offset
    phi <- exp(phi - max(phi))
    risk_phi <- cumsum(phi)
    w <- sapply(1:nobs, function(i) {
      idx <- time < time[i]
      sum((phi[i] * risk_phi[idx] - phi[i]^2) / (risk_phi[idx]^2))
    })
    z <- sapply(1:nobs, function(i) {
      idx <- time < time[i]
      theta[i] + (status[i] - sum(phi[i] / risk_phi[idx])) / w[i]
    })
    z[is.infinite(z)] <- 0
    for (j in 1:nvar) {
      xr <- mean(x[, j] * w * (z - x[, -j] %*% eta[-j]))
      v <- mean(w * x[, j]**2)
      eta[j] <- soft_threshold(xr, v, penalty, lambda, gamma)
    }
    if (max(abs(eta - eta_old)) <= control$eps || iter >= control$maxit) break
  }

  fit <- list()
  coefficients <- list()
  coefficients$beta <- coef_src + eta
  coefficients$eta <- eta

  fit$coefficients <- coefficients
  fit$penalty <- penalty
  fit$lambda <- lambda
  fit$gamma <- gamma
  fit$iter <- iter
  fit$formula <- form_src

  return(fit)
}
