#' Cross-validation for \code{coxtrans}
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param  penalty a character string specifying the penalty function.
#' The default is "lasso". Other options are "MCP" and "SCAD".
#' @param penalty_weights a numeric vector specifying the weights of the
#' groups. The default is NULL, which means that the weights are set to be
#' proportional to the inverse number of samples in each group.
#' @param gamma a non-negative value specifying the penalty parameter. The
#' default is 3.7 for SCAD and 3.0 for MCP.
#' @param nfolds an integer specifying the number of folds.
#' @param nlambdas an integer specifying the number of lambda values.
#' @param lambda_min_ratio a numeric value specifying the minimum lambda value
#' as a fraction of lambda_max.
#' @param verbose a logical value specifying whether to print the progress.
#' @param seed an integer specifying the random seed.
#' @param control Object of class \link{survtrans_control} containing
#' control parameters for the fitting algorithm. Default is
#' \code{survtrans_control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans_control}}.
#' @return a cv_coxtrans object.
#' @export
#' @examples
#' library(survtrans)
#' formula <- Surv(time, status) ~ . - group - id
#' group <- as.factor(sim1$group)
#' cv_fit <- cv_coxtrans(formula, sim1, group, penalty = "SCAD")
#' coef(cv_fit, "lambda.min")
cv_coxtrans <- function( # nolint: cyclocomp_linter.
    formula, data, group, penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), penalty_weights = NULL, nfolds = 10, nlambdas = 100,
    lambda_min_ratio = NULL, verbose = FALSE, seed = 0, control, ...) {
  # Set the random seed
  set.seed(seed)

  # Load the data
  data_ <- preprocess_data(formula, data, group = group)
  x <- data_$x
  time <- data_$time
  status <- data_$status
  group_ <- data_$group

  # Properties of the data
  n_samples <- nrow(x)
  n_features <- ncol(x)
  n_groups <- length(unique(group_))
  group_levels <- levels(group_)

  penalty <- match.arg(penalty)

  # Check the penalty_weights argument
  if (is.null(penalty_weights)) {
    penalty_weights <- c()
    for (k in 1:n_groups) {
      ind <- which(group == group_levels[k])
      penalty_weights <- c(penalty_weights, length(ind))
    }
  } else {
    if (length(penalty_weights) != n_groups) {
      stop("Wrong length for penalty_weights")
    }
  }
  penalty_weights <- penalty_weights / sum(penalty_weights)

  # Check the lambda_min_ratio argument
  if (is.null(lambda_min_ratio)) {
    lambda_min_ratio <- ifelse(n_samples < n_features, 0.01, 1e-04)
  }

  # Check the control argument
  if (missing(control)) control <- survtrans_control(...)

  # Determmine the lambda sequence
  lambda_max <- 0
  for (k in 1:n_groups) {
    ind <- which(group_ == group_levels[k])
    temp <- calc_weights_residuals(
      coef = rep(0, n_features), x = x[ind, ], time = time[ind],
      status = status[ind], offset = rep(0, length(ind))
    )
    weights <- temp$weights
    residuals <- temp$residuals
    zw0 <- residuals * weights
    lambda_max_ <- max(abs(colMeans(sweep(x[ind, ], MARGIN = 1, zw0, `*`))))
    lambda_max_ <- lambda_max_ / penalty_weights[k]
    if (lambda_max_ > lambda_max) lambda_max <- lambda_max_
  }
  lambda_min <- lambda_max * lambda_min_ratio * min(penalty_weights)
  lambdas <- exp(seq(log(lambda_max), log(lambda_min), length.out = nlambdas))

  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent eta: :eta lambda=:lambda",
      total = nlambdas, clear = FALSE, width = 60
    )
  }

  idx <- sample(1:nfolds, nrow(x), replace = TRUE)
  coefs <- matrix(0, nrow = nlambdas, ncol = n_features * (n_groups + 1))
  criterions <- matrix(0, nrow = nlambdas, ncol = nfolds)
  for (i in seq_along(lambdas)) {
    fit <- coxtrans(
      formula = formula, data = data, group = group,
      lambda1 = lambdas[i], penalty = penalty, gamma = gamma,
      control = control, ...
    )
    coefs[i, ] <- as.vector(fit$coefficients)
    for (k in 1:nfolds) {
      fit <- coxtrans(
        formula = formula, data = data[idx != k, ], group = group[idx != k],
        lambda1 = lambdas[i], penalty = penalty, gamma = gamma,
        init = matrix(coefs[i, ], nrow = n_features), control = control, ...
      )
      log_lik <- logLik(fit, data[idx != k, ], group[idx != k])
      criterions[i, k] <- logLik(fit, data, group) - log_lik
    }
    if (verbose) {
      pb$tick(
        tokens = list(
          lambda = round(lambdas[i], digits = 3),
          loss = round(mean(criterions[i, ]), digits = 3)
        )
      )
    }
  }

  cvm <- rowMeans(criterions)
  cvsd <- apply(criterions, 1, stats::sd)

  fit <- list(
    n_features = n_features,
    lambdas = lambdas, coefs = coefs, cvm = cvm, cvsd = cvsd,
    formula = formula, call = match.call()
  )
  class(fit) <- "cv_coxtrans"
  return(fit)
}
