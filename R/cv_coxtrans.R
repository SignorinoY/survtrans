#' Cross-validation for \code{coxtrans}
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param bgroup a factor specifying the group of each sample for baseline
#' hazard.
#' @param egroup a factor specifying the group of each sample for effect.
#' @param  penalty a character string specifying the penalty function.
#' The default is "lasso". Other options are "MCP" and "SCAD".
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
cv_coxtrans <- function(
    # nolint: cyclocomp_linter.
    formula, data, group, bgroup, egroup,
    penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), nfolds = 10, nlambdas = 100, lambda_min_ratio = NULL,
    verbose = FALSE, seed = 0, control, ...) {
  # Set the random seed
  set.seed(seed)

  # Load the data
  data_ <- preprocess_data(formula, data)
  x <- data_$x
  time <- data_$time
  status <- data_$status

  # Properties of the data
  n_samples <- nrow(x)
  n_features <- ncol(x)

  # Check the group argument
  if (missing(bgroup)) bgroup <- group
  if (missing(egroup)) {
    egroup <- matrix(rep(group, n_features), ncol = n_features)
  }
  bgroup_ <- bgroup[data_$sorted]
  egroup_ <- egroup[data_$sorted, ]

  # Properties of the data
  n_bgroups <- length(unique(bgroup))
  bgroup_levels <- levels(bgroup)
  n_egroups <- c()
  for (k in 1:n_features) {
    n_egroups[k] <- length(unique(egroup[, k]))
  }
  egroup_levels <- list()
  for (k in 1:n_features) {
    egroup_levels[[k]] <- levels(as.factor(egroup[, k]))
  }

  penalty <- match.arg(penalty)

  # Check the lambda_min_ratio argument
  if (is.null(lambda_min_ratio)) {
    lambda_min_ratio <- ifelse(n_samples < n_features, 0.01, 1e-04)
  }

  # Check the control argument
  if (missing(control)) control <- survtrans_control(...)

  # Determmine the lambda sequence
  lambda_max <- 0
  weights <- rep(0, n_samples)
  residuals <- rep(0, n_samples)
  for (k in 1:n_bgroups) {
    ind <- which(bgroup_ == bgroup_levels[k])
    temp <- calc_weights_residuals(
      coef = rep(0, n_features), x = x[ind, ], time = time[ind],
      status = status[ind], offset = rep(0, length(ind))
    )
    weights[ind] <- temp$weights
    residuals[ind] <- temp$residuals
  }
  for (j in 1:n_features) {
    for (k in 1:n_egroups[j]) {
      ind <- which(egroup_[, j] == egroup_levels[[j]][k])
      w <- weights[ind]
      r <- residuals[ind]
      zw0 <- w * r
      lambda_max_ <- abs(mean(x[ind, j] * zw0))
      if (lambda_max_ > lambda_max) lambda_max <- lambda_max_
    }
  }
  lambda_min <- lambda_max * lambda_min_ratio
  lambdas <- exp(seq(log(lambda_max), log(lambda_min), length.out = nlambdas))

  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent eta: :eta lambda=:lambda",
      total = nlambdas, clear = FALSE, width = 60
    )
  }

  idx <- sample(1:nfolds, nrow(x), replace = TRUE)
  coefs <- matrix(0, nrow = nlambdas, ncol = n_features * (max(n_egroups) + 1))
  criterions <- matrix(0, nrow = nlambdas, ncol = nfolds)
  for (i in seq_along(lambdas)) {
    fit <- coxtrans(
      formula = formula, data = data, group = group, bgroup = bgroup,
      egroup = egroup, lambda1 = lambdas[i], penalty = penalty, gamma = gamma,
      control = control, ...
    )
    coefs[i, ] <- as.vector(fit$coefficients)
    for (k in 1:nfolds) {
      fit <- coxtrans(
        formula = formula, data = data[idx != k, ], group = group[idx != k],
        bgroup = bgroup[idx != k], egroup = egroup[idx != k, ],
        lambda1 = lambdas[i], penalty = penalty, gamma = gamma,
        init = matrix(coefs[i, ], nrow = n_features), control = control, ...
      )
      log_lik <- logLik(
        fit, data[idx != k, ], bgroup[idx != k], egroup[idx != k, ]
      )
      criterions[i, k] <- logLik(fit, data, bgroup, egroup) - log_lik
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
