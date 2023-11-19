#' Cross-validation for \code{survtran}
#' @param data a data frame containing the variables in the model.
#' @param fit_source a survode object.
#' @param penalty a character string specifying the type of penalty.
#'  Possible values are \code{"lasso"}, \code{"MCP"} and \code{"SCAD"}.
#' @param gamma a numeric value specifying the tuning parameter for MCP or SCAD.
#' @param nfolds an integer specifying the number of folds.
#' @param nlambdas an integer specifying the number of lambda values.
#' @param lambda_min_ratio a numeric value specifying the minimum lambda value
#' as a fraction of lambda_max.
#' @param seed an integer specifying the random seed.
#' @param control Object of class \link{survtrans_control} containing
#'   control parameters for the fitting algorithm. Default is
#'  \code{survtrans_control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans_control}}.
#' @return a cv.survtran object.
#' @import survode
#' @export
#' @examples
#' library(survode)
#' library(survtrans)
#' fit_src <- survode(Surv(time, status) ~ ., data = sim1_src, df = 10)
#' cv_survtrans(sim1_trg, fit_src)
cv_survtrans <- function(
    data, fit_source, penalty = "lasso", gamma = NULL, nfolds = 10,
    nlambdas = 100, lambda_min_ratio = NULL, seed = 0, control, ...) {
  if (missing(data)) stop("a data argument is required")
  if (!inherits(fit_source, "survode")) {
    stop("fit_source must be survode object")
  }

  # TODO: check formula, coefficient and data
  form_src <- fit_source$formula
  coef_src <- fit_source$coefficients

  mf <- stats::model.frame(form_src, data)
  y <- stats::model.response(mf)
  x <- stats::model.matrix(form_src, data)
  x <- x[, !colnames(x) %in% "(Intercept)"]
  if (!is.matrix(x)) x <- as.matrix(x)
  nobs <- nrow(x)
  nvar <- ncol(x)

  if (is.null(gamma)) {
    gamma <- switch(penalty,
      MCP = 1.5,
      SCAD = 3.7,
      1
    )
  }
  if (is.null(lambda_min_ratio)) {
    lambda_min_ratio <- ifelse(nobs < nvar, 0.01, 1e-04)
  }
  if (missing(control)) control <- survtrans_control(...)

  time <- y[, 1]
  status <- y[, 2]
  cbh <- stats::predict(fit_source, type = "hazard", time = time)$cumhaz

  # TODO: cannot guarantee the coefficients be zero
  zw0 <- status - cbh * exp(x %*% coef_src$beta)
  lambda_max <- max(colMeans(sweep(x, MARGIN = 1, zw0, `*`)))
  lambda_min <- lambda_max * lambda_min_ratio
  lambdas <- exp(seq(log(lambda_max), log(lambda_min), length.out = nlambdas))

  idx <- sample(1:nfolds, nrow(x), replace = TRUE)

  criterions <- matrix(0, nrow = nlambdas, ncol = nfolds)
  etas <- matrix(0, nrow = nlambdas, ncol = nvar)
  for (k in 1:nfolds) {
    eta0 <- rep(0, nvar)
    for (i in seq_along(lambdas)) {
      fit_k <- survtrans(
        data[idx != k, ], fit_source,
        penalty = penalty, lambda = lambdas[i], gamma = gamma,
        init = eta0, control = control
      )
      etas[i, ] <- etas[i, ] + fit_k$coefficients$eta / nfolds
      criterions[i, k] <- loss_mle(data[idx == k, ], cbh[idx == k], fit_k)
    }
  }
  for (i in seq_along(lambdas)) {
    etas[i, ] <- survtrans(
      data, fit_source,
      penalty = penalty, lambda = lambdas[i], gamma = gamma,
      init = etas[i, ], control = control
    )$coefficients$eta
  }

  cvm <- rowMeans(criterions)
  cvsd <- apply(criterions, 1, stats::sd)
  lambda_opt <- lambdas[which.max(cvm)]
  eta_opt <- etas[which.max(cvm), ]

  coefficients <- coef_src
  coefficients$beta <- coefficients$beta + eta_opt
  coefficients$eta <- eta_opt
  coefficients$penalty <- penalty
  coefficients$lambda <- lambda_opt
  coefficients$gamma <- gamma
  coefficients$formula <- form_src

  fit <- list(
    coefficients = coefficients, lambdas = lambdas, etas = etas,
    cvm = cvm, cvsd = cvsd
  )
  class(fit) <- "cv.survtran"
  fit
}
