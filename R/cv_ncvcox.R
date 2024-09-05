#' Cross-validation for \code{ncvcox}
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param offset a numeric vector specifying the offset..
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
#' formula <- Surv(time, status) ~ . - group - id
#' group <- as.factor(sim1$group)
#' cv_fit <- cv_ncvcox(
#'  formula, sim1[sim1$group == 1, ], penalty = "SCAD"
#' )
#' coef(cv_fit)
cv_ncvcox <- function(
    formula, data, group, offset, penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), nfolds = 10, nlambdas = 100, lambda_min_ratio = NULL,
    seed = 0, control, ...) {
  set.seed(seed)
  penalty <- match.arg(penalty)

  # Properties of the data
  x <- model.matrix(formula, data)
  x <- x[, -1] # Remove the intercept column
  n_samples <- nrow(data)
  n_features <- ncol(x)

  # Check the arguments
  if (missing(group)) group <- rep(1, n_samples)
  if (missing(offset)) offset <- rep(0, n_samples)
  if (is.null(lambda_min_ratio)) {
    lambda_min_ratio <- ifelse(n_samples < n_features, 0.01, 1e-04)
  }
  if (missing(control)) control <- survtrans_control(...)

  # Determmine the lambda sequence
  lambda_max <- calc_lambda_max(formula, data, group, offset)
  lambda_min <- lambda_max * lambda_min_ratio
  lambdas <- exp(seq(log(lambda_max), log(lambda_min), length.out = nlambdas))

  coefs <- matrix(0, nrow = nlambdas, ncol = n_features)
  init <- numeric(n_features)
  for (i in seq_along(lambdas)) {
    fit <- ncvcox(
      formula, data, group, offset,
      lambda = lambdas[i], penalty = penalty,
      gamma = gamma, init = init, control = control, ...
    )
    init <- fit$coefficients
    coefs[i, ] <- fit$coefficients
  }
  colnames(coefs) <- colnames(x)

  idx <- sample(1:nfolds, n_samples, replace = TRUE)
  criterions <- matrix(0, nrow = nlambdas, ncol = nfolds)
  for (i in seq_along(lambdas)) {
    for (k in 1:nfolds) {
      fit <- ncvcox(
        formula = formula, data = data[idx != k, ],
        group = group[idx != k], offset = offset[idx != k],
        lambda = lambdas[i], penalty = penalty, gamma = gamma,
        init = coefs[i, ], control = control, ...
      )
      criterions[i, k] <- logLik(fit, data, group, offset) - fit$logLik
    }
  }

  cvm <- rowMeans(criterions)
  cvsd <- apply(criterions, 1, stats::sd)

  fit <- list(
    lambdas = lambdas, coefs = coefs, cvm = cvm, cvsd = cvsd,
    formula = formula, call = match.call()
  )
  class(fit) <- "cv_ncvcox"
  return(fit)
}
