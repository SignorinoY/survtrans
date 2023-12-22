#' Cross-validation for \code{coxtrans}
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param offset a numeric vector specifying the offset.
#' @param penalty a character string specifying the penalty function.
#' The default is "lasso". Other options are "MCP" and "SCAD".
#' @param gamma a non-negative value specifying the penalty parameter. The
#' default is 3.7 for SCAD and 3 for MCP.
#' @param nfolds an integer specifying the number of folds.
#' @param nlambdas an integer specifying the number of lambda values.
#' @param lambda_min_ratio a numeric value specifying the minimum lambda value
#' as a fraction of lambda_max.
#' @param seed an integer specifying the random seed.
#' @param control Object of class \link{survtrans_control} containing
#' control parameters for the fitting algorithm. Default is
#' \code{survtrans_control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans_control}}.
#' @return a cv_ncvcox object.
#' @export
#' @examples
#' library(survtrans)
#' formula <- Surv(time, status) ~ . - id
#' cv_fit <- cv_ncvcox(formula, sim_sparse, penalty = "SCAD")
#' coef(cv_fit, "lambda.min")
cv_ncvcox <- function(
    formula, data, offset, penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), nfolds = 10, nlambdas = 100, lambda_min_ratio = NULL,
    seed = 0, control, ...) {
  set.seed(seed)
  penalty <- match.arg(penalty)

  data_ <- preprocess_data(formula, data, offset)
  x <- data_$x
  time <- data_$time
  status <- data_$status
  offset_ <- data_$offset

  # Properties of the data
  n_samples <- nrow(x)
  n_features <- ncol(x)

  # Check the offset argument
  if (missing(offset)) offset <- rep(0, nrow(x))

  # Check the lambda_min_ratio argument
  if (is.null(lambda_min_ratio)) {
    lambda_min_ratio <- ifelse(n_samples < n_features, 0.01, 1e-04)
  }

  # Check the control argument
  if (missing(control)) control <- survtrans_control(...)

  # Determmine the lambda sequence
  temp <- calc_weights_residuals(
    coef = rep(0, n_features), x = x, time = time, status = status,
    offset = offset_
  )
  weights <- temp$weights
  residuals <- temp$residuals
  zw0 <- (offset_ + residuals) * weights
  lambda_max <- max(abs(colMeans(sweep(x, MARGIN = 1, zw0, `*`))))
  lambda_min <- lambda_max * lambda_min_ratio
  lambdas <- exp(seq(log(lambda_max), log(lambda_min), length.out = nlambdas))

  idx <- sample(1:nfolds, nrow(x), replace = TRUE)

  coef_init <- rep(0, n_features)
  criterions <- matrix(0, nrow = nlambdas, ncol = nfolds)
  coefs <- matrix(0, nrow = nlambdas, ncol = n_features)
  for (i in seq_along(lambdas)) {
    for (k in 1:nfolds) {
      fit <- ncvcox(
        formula = formula, data = data[idx != k, ], offset = offset[idx != k],
        lambda = lambdas[i], penalty = penalty, gamma = gamma, init = coef_init,
        control = control, ...
      )
      coef_init <- fit$coefficients
      coefs[i, ] <- coefs[i, ] + coef_init / nfolds
      criterions[i, k] <- logLik(fit, data, offset) - fit$logLik
    }
  }
  for (i in seq_along(lambdas)) {
    coefs[i, ] <- ncvcox(
      formula = formula, data = data, offset = offset,
      lambda = lambdas[i], penalty = penalty, gamma = gamma,
      init = coefs[i, ], control = control, ...
    )$coefficients
  }

  cvm <- rowMeans(criterions)
  cvsd <- apply(criterions, 1, stats::sd)

  fit <- list(
    lambdas = lambdas, coefs = coefs, cvm = cvm, cvsd = cvsd,
    formula = formula, call = match.call(),
    n_features = n_features
  )
  class(fit) <- "cv_ncvcox"
  return(fit)
}
