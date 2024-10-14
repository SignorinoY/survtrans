#' @title Cross-validation for \code{ncvcox}
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
#' @param seed an integer specifying the random seed. Default is 0.
#' @param parallel a integer specifying the number of parallel workers.
#' @param control Object of class \link{survtrans_control} containing
#' control parameters for the fitting algorithm. Default is
#' \code{survtrans_control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans_control}}.
#' @return a \code{cv.ncvcox} object.
#' @export
cv.ncvcox <- function(
    formula, data, group, offset, penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), nfolds = 10, nlambdas = 100, lambda_min_ratio = NULL,
    seed = 0, parallel = 0, control, ...) {
  # Properties of the data
  x <- model.matrix(formula, data)
  n_samples <- nrow(data)
  n_features <- ncol(x) - 1

  # Check the arguments
  if (missing(group)) group <- rep(1, n_samples)
  if (missing(offset)) offset <- rep(0, n_samples)
  penalty <- match.arg(penalty, choices = c("lasso", "MCP", "SCAD"))
  if (is.null(lambda_min_ratio)) {
    lambda_min_ratio <- ifelse(n_samples < n_features, 0.01, 1e-04)
  }
  if (missing(control)) control <- survtrans_control(...)

  # Determmine the lambda sequence
  lambda_max <- calc_lambda_max(formula, data, group, offset)
  lambda_min <- lambda_max * lambda_min_ratio
  lambdas <- exp(seq(log(lambda_max), log(lambda_min), length.out = nlambdas))

  coefficients <- matrix(0, nrow = nlambdas, ncol = n_features)
  set.seed(seed)
  idx <- sample(1:nfolds, n_samples, replace = TRUE)
  if (!parallel) {
    criterions <- matrix(0, nrow = nlambdas, ncol = nfolds)
    for (i in seq_along(lambdas)) {
      fit <- ncvcox(
        formula, data, group, offset,
        lambda = lambdas[i], penalty = penalty,
        gamma = gamma, control = control, ...
      )
      coefficients[i, ] <- fit$coefficients
      for (k in 1:nfolds) {
        fit <- ncvcox(
          formula = formula, data = data[idx != k, ],
          group = group[idx != k], offset = offset[idx != k],
          lambda = lambdas[i], penalty = penalty, gamma = gamma,
          control = control, ...
        )
        criterions[i, k] <- logLik(fit, data, group, offset) - fit$logLik
      }
    }
  } else {
    cl <- parallel::makeCluster(parallel)
    doSNOW::registerDoSNOW(cl)
    pb <- txtProgressBar(max = nlambdas, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    records <- foreach(
      i = seq_along(lambdas), .combine = "rbind", .errorhandling = "remove",
      .options.snow = opts, .packages = "survtrans"
    ) %dopar% {
      fit <- ncvcox(
        formula, data, group, offset,
        lambda = lambdas[i], penalty = penalty,
        gamma = gamma, control = control, ...
      )
      coefficients <- fit$coefficients
      criterion <- numeric(nfolds)
      for (k in 1:nfolds) {
        fit <- ncvcox(
          formula = formula, data = data[idx != k, ],
          group = group[idx != k], offset = offset[idx != k],
          lambda = lambdas[i], penalty = penalty, gamma = gamma,
          control = control, ...
        )
        criterion[k] <- logLik(fit, data, group, offset) - fit$logLik
      }
      c(coefficients, criterion)
    }
    coefficients <- records[, 1:n_features]
    criterions <- records[, -seq_len(n_features)]
    parallel::stopCluster(cl)
    close(pb)
  }
  colnames(coefficients) <- colnames(x[, -1])
  colnames(criterions) <- paste0("Fold", 1:nfolds)
  cvm <- rowMeans(criterions)
  cvsd <- apply(criterions, 1, stats::sd)
  lambda.min <- lambdas[which.max(cvm)]
  lambda.1se <- lambdas[which.max(cvm + cvsd)]

  fit <- list(
    lambdas = lambdas, coefficients = coefficients, criterions = criterions,
    name = "Cross-validated log-likelihood", cvm = cvm, cvsd = cvsd,
    lambda.min = lambda.min, lambda.1se = lambda.1se, call = match.call()
  )
  class(fit) <- "cv.ncvcox"
  return(fit)
}
