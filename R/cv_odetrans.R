#' Cross-validation for \code{survtran}
#' @param data a data frame containing the variables in the model.
#' @param fit_source a survode object.
#' @param penalty a character string specifying the type of penalty.
#'  Possible values are \code{"lasso"}, \code{"MCP"} and \code{"SCAD"}.
#' @param gamma a numeric value specifying the tuning parameter for MCP or SCAD.
#' @param cbh_func a function specifying the cumulative baseline hazard. The
#'  default is NULL, which means the cumulative baseline hazard is estimated
#'  from the survode object fitted to the source data.
#' @param nfolds an integer specifying the number of folds.
#' @param nlambdas an integer specifying the number of lambda values.
#' @param lambda_min_ratio a numeric value specifying the minimum lambda value
#'  as a fraction of lambda_max.
#' @param seed an integer specifying the random seed.
#' @param control Object of class \link{survtrans_control} containing
#'  control parameters for the fitting algorithm. Default is
#'  \code{survtrans_control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans_control}}.
#' @return a cv_survtran object.
#' @export
#' @examples
#' formula <- Surv(time, status) ~ . - group - id
#' fit_src <- survode(formula, data = sim1[sim1$group == 1, ], df = 10)
#' cv_fit <- cv_odetrans(sim1[sim1$group == 2, ], fit_src, lambda = 0.1)
cv_odetrans <- function(
    data, fit_source, penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), cbh_func = NULL, nfolds = 10, nlambdas = 100, lambda_min_ratio = NULL,
    seed = 0, control, ...) {
  penalty <- match.arg(penalty)
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

  if (is.null(lambda_min_ratio)) {
    lambda_min_ratio <- ifelse(nobs < nvar, 0.01, 1e-04)
  }
  if (missing(control)) control <- survtrans_control(...)

  time <- y[, 1]
  status <- y[, 2]
  if (is.null(cbh_func)) {
    cbh <- stats::predict(fit_source, type = "hazard", time = time)$cumhaz
  } else {
    if (!is.function(cbh_func)) stop("cbh_func must be a function")
    cbh <- cbh_func(time)
  }

  # TODO: cannot guarantee the coefficients be zero
  zw0 <- status
  lambda_max <- max(colMeans(sweep(x, MARGIN = 1, zw0, `*`)))
  lambda_min <- lambda_max * lambda_min_ratio
  lambdas <- exp(seq(log(lambda_max), log(lambda_min), length.out = nlambdas))

  idx <- sample(1:nfolds, nrow(x), replace = TRUE)

  criterions <- matrix(0, nrow = nlambdas, ncol = nfolds)
  etas <- matrix(0, nrow = nlambdas, ncol = nvar)
  for (k in 1:nfolds) {
    eta0 <- rep(0, nvar)
    for (i in seq_along(lambdas)) {
      fit_k <- odetrans(
        data[idx != k, ], fit_source,
        penalty = penalty, lambda = lambdas[i], gamma = gamma,
        cbh_func = cbh_func, init = eta0, control = control
      )
      etas[i, ] <- etas[i, ] + fit_k$coefficients$eta / nfolds
      criterions[i, k] <- loss_mle(data[idx == k, ], cbh[idx == k], fit_k)
    }
  }
  for (i in seq_along(lambdas)) {
    etas[i, ] <- odetrans(
      data, fit_source,
      penalty = penalty, lambda = lambdas[i], gamma = gamma,
      cbh_func = cbh_func, init = etas[i, ], control = control
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

  fit <- list(
    coefficients = coefficients, lambdas = lambdas, etas = etas,
    cvm = cvm, cvsd = cvsd, formula = form_src
  )
  class(fit) <- "cv_survtran"
  fit
}
