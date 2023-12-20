#' Cross-validation for \code{coxtrans}
#' @param formula a formula object, with the response on the left of a ~
#' operator, and the terms, separated by + operators, on the right.
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
#' @param control Object of class \link{survtrans.control} containing
#' control parameters for the fitting algorithm. Default is
#' \code{survtrans.control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans.control}}.
#' @return a cv_ncvcox object.
#' @export
cv_ncvcox <- function(
    formula, data, offset, penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), nfolds = 10, nlambdas = 100, lambda_min_ratio = NULL,
    seed = 0, control, ...) {
  penalty <- match.arg(penalty)
  if (missing(formula)) stop("a formula argument is required")

  # Load X, y from formula and data
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  x <- model.matrix(formula, data)
  x <- x[, -1] # Remove the intercept column

  # Properties of the data
  n_samples <- nrow(x)
  n_features <- ncol(x)

  # Check the offset argument
  if (missing(offset)) offset <- rep(0.0, n_samples)

  # Check the lambda_min_ratio argument
  if (is.null(lambda_min_ratio)) {
    lambda_min_ratio <- ifelse(n_samples < n_features, 0.01, 1e-04)
  }
  if (missing(control)) control <- survtrans.control(...)

  # Determmine the lambda sequence
  coef_init <- rep(0, n_features)
  sorted <- order(y[, 1], decreasing = TRUE)
  time <- y[sorted, 1]
  status <- y[sorted, 2]
  x <- x[sorted, , drop = FALSE]
  offset_ <- offset[sorted]
  temp <- calc_weights_residuals(
    coef = coef_init, x = x, time = time, status = status, offset = offset_
  )
  weights <- temp$weights
  residuals <- temp$residuals
  zw0 <- (offset_ + residuals) * weights
  lambda_max <- max(abs(colMeans(sweep(x, MARGIN = 1, zw0, `*`))))
  lambda_min <- lambda_max * lambda_min_ratio
  lambdas <- exp(seq(log(lambda_max), log(lambda_min), length.out = nlambdas))

  idx <- sample(1:nfolds, nrow(x), replace = TRUE)

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
      criterions[i, k] <- logLik(fit, data, offset) - logLik(fit, data[idx != k, ], offset[idx != k])
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
    lambdas = lambdas, coefs = coefs,
    cvm = cvm, cvsd = cvsd, formula = formula, call = match.call()
  )
  class(fit) <- "cv_ncvcox"
  return(fit)
}
