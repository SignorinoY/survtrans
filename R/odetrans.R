#' Transfer Learning for Survival Analysis via Ordinary Differential Equations
#' @param data a data frame containing the variables in the model.
#' @param fit_source a survode object fitted to the source data.
#' @param penalty a character string specifying the penalty function.
#'  The default is "lasso". Other options are "MCP" and "SCAD".
#' @param lambda a non-negative value specifying the penalty parameter. The
#'  default is 0.
#' @param gamma a non-negative value specifying the penalty parameter. The
#'  default is 3.7 for SCAD and 3.0 for MCP.
#' @param cbh_func a function specifying the cumulative baseline hazard. The
#'  default is NULL, which means the cumulative baseline hazard is estimateds
#'  from the survode object fitted to the source data.
#' @param init a numeric vector specifying the initial value of the
#'  coefficients. The default is a zero vector.
#' @param control Object of class \link{survtrans_control} containing
#'  control parameters for the fitting algorithm. Default is
#'  \code{survtrans_control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans_control}}.
#' @export
#' @examples
#' library(survode)
#' library(survtrans)
#' formula <- Surv(time, status) ~ . - group - id
#' fit_src <- survode(formula, data = sim1[sim1$group == 1, ], df = 10)
#' fit <- odetrans(sim1[sim1$group == 2, ], fit_src, lambda = 0.1)
odetrans <- function(
    data, fit_source, penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), lambda = 0, cbh_func = NULL, init, control, ...) {
  penalty <- match.arg(penalty)

  # TODO: check formula, coefficient and data
  form_src <- fit_source$formula
  coef_src <- fit_source$coefficients

  mf <- stats::model.frame(form_src, data)
  y <- stats::model.response(mf)
  x <- stats::model.matrix(form_src, data)
  x <- x[, !colnames(x) %in% "(Intercept)"]
  if (!is.matrix(x)) x <- as.matrix(x)
  nvar <- ncol(x)
  if (missing(init)) init <- rep(0, nvar)
  if (missing(control)) control <- survtrans_control(...)

  time <- y[, 1]
  status <- y[, 2]
  if (is.null(cbh_func)) {
    cbh <- stats::predict(fit_source, type = "hazard", time = time)$cumhaz
  } else {
    if (!is.function(cbh_func)) stop("cbh_func must be a function")
    cbh <- cbh_func(time)
  }
  offset <- x %*% coef_src$beta

  eta <- init
  iter <- 0
  repeat {
    iter <- iter + 1
    eta_old <- eta
    theta <- x %*% eta
    w <- cbh * exp(theta + offset)
    z <- theta + status / w - 1
    for (j in 1:nvar) {
      xr <- mean(x[, j] * w * (z - x[, -j] %*% eta[-j]))
      v <- mean(w * x[, j]**2)
      eta[j] <- penalty_solution(xr, v, penalty, lambda, gamma)
    }
    if (max(abs(eta - eta_old)) <= control$eps || iter >= control$maxit) break
  }

  fit <- list()
  coefficients <- coef_src
  coefficients$beta <- coefficients$beta + eta
  coefficients$eta <- eta

  fit$coefficients <- coefficients
  fit$penalty <- penalty
  fit$lambda <- lambda
  fit$gamma <- gamma
  fit$iter <- iter
  fit$formula <- form_src

  return(fit)
}
