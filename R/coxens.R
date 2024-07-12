#' @title Multiple-sources Cox proportional hazards model with group sparsity
#' and Local-Global transfer
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param lambda1 a non-negative value specifying the sparse penalty
#' parameter. The default is 0.
#' @param lambda2 a non-negative value specifying the transfer penalty
#' parameter. The default is 0.
#' @param alpha a value in (0, 1) specifying the proportion of the penalty
#' applied to the transfer coefficients between global and local models. The
#' default is 0.5.
#' @param penalty a character string specifying the penalty function.
#' The default is "lasso". Other options are "MCP" and "SCAD".
#' @param gamma a non-negative value specifying the penalty parameter. The
#' default is 3.7 for SCAD and 3.0 for MCP.
#' @param rho a value in (2, 10) specifying the expansion factor of the
#' augmented Lagrangian's penalty parameter. The default is 2.0.
#' @param init a numeric vector specifying the initial value of the
#' coefficients. The default is a zero vector.
#' @param control Object of class \link{survtrans_control} containing
#' control parameters for the fitting algorithm. Default is
#' \code{survtrans_control(...)}.
#' @param ... Other arguments passed to \code{\link{survtrans_control}}.
#' @return a coxsg object.
#' @export
#' @examples
#' library(survtrans)
#' formula <- Surv(time, status) ~ . - group - id
#' group <- as.factor(sim2$group)
#' fit <- coxens(
#'   formula, sim2, group,
#'   lambda1 = 0.02, lambda2 = 0.006, penalty = "SCAD"
#' )
#' fit$coefficients
coxens <- function(
    formula, data, group, lambda1 = 0, lambda2 = 0, alpha = 0.5,
    penalty = c("lasso", "MCP", "SCAD"),
    gamma = switch(penalty,
      SCAD = 3.7,
      MCP = 3,
      1
    ), rho = 2.0, init, control, ...) {
  # Load the data
  data_ <- data
  group_ <- group
  data <- preprocess_data(formula, data_, group = group_)
  x <- data$x
  x_scale <- attr(x, "scale")
  time <- data$time
  status <- data$status
  group <- data$group

  # Properties of the data
  n_samples <- nrow(x)
  n_features <- ncol(x)
  n_groups <- length(unique(group))
  group_levels <- levels(group)
  group_idxs <- lapply(group_levels, function(x) which(group == x))

  risk_set_size <- ave(rep(1, n_samples), group, FUN = cumsum)
  risk_set_size <- unlist(lapply(1:n_groups, function(k) {
    idx <- group_idxs[[k]]
    ave(risk_set_size[idx], time[idx], FUN = max)
  }))
  null_deviance <- -sum(status * log(risk_set_size))

  ## Check the penalty argument
  penalty <- match.arg(penalty, choices = c("lasso", "MCP", "SCAD"))

  ## Check the init argument
  if (!missing(init) && length(init) > 0) {
    if (length(init) != n_features * (n_groups + 1)) {
      stop("Wrong length for inital values")
    }
  } else {
    init <- coxgrp(
      formula = formula, data = data_, group = group_, lambda = lambda1,
      penalty = penalty, gamma = gamma,
    )$coefficients
    init <- cbind(init, rowMeans(init))
  }
  ## Check the control argument
  if (missing(control)) control <- survtrans_control(...)

  # Extract the coefficients from init vector
  init <- matrix(init, nrow = n_features)
  init <- sweep(init, 1, x_scale, `*`)
  beta <- init[, 1:(n_groups + 1), drop = FALSE]
  beta <- matrix(beta, nrow = n_features * (n_groups + 1), ncol = 1)
  beta <- Matrix::Matrix(beta, sparse = TRUE)

  # Initialize the training process
  n_iterations <- 0
  message <- ""
  convergence <- FALSE

  offset <- numeric(n_samples)
  w <- numeric(n_samples)
  z <- numeric(n_samples)

  idx <- which(lower.tri(matrix(1, n_groups, n_groups)), arr.ind = TRUE)
  e <- Matrix::Matrix(0, nrow(idx), n_groups, sparse = TRUE)
  e[cbind(seq_len(nrow(idx)), idx[, 1])] <- 1
  e[cbind(seq_len(nrow(idx)), idx[, 2])] <- -1

  contr <- rbind(
    cbind(
      Matrix::Diagonal(n_features * n_groups),
      matrix(0, nrow = n_features * n_groups, ncol = n_features)
    ),
    cbind(
      Matrix::Diagonal(n_features * n_groups),
      -kronecker(matrix(1, n_groups, 1), diag(n_features))
    ),
    cbind(
      kronecker(e, diag(n_features)),
      matrix(0, n_groups * (n_groups - 1) * n_features / 2, n_features)
    )
  )
  n_constraints <- nrow(contr)
  contr2 <- Matrix::crossprod(contr)
  global_indices <- (n_groups * n_features + 1):
    (n_groups * n_features + n_features * n_groups)
  local_indices <- (2 * n_groups * n_features + 1):n_constraints

  x_tilde <- Matrix::bdiag(lapply(group_idxs, function(idx) x[idx, ]))
  x_tilde <- cbind(x_tilde, matrix(0, nrow = n_samples, ncol = n_features))
  time_tilde <- unlist(lapply(group_idxs, function(idx) time[idx]))
  status_tilde <- unlist(lapply(group_idxs, function(idx) status[idx]))

  eta <- Matrix::Matrix(0, n_constraints, 1, sparse = TRUE)
  mu <- Matrix::Matrix(0, n_constraints, 1, sparse = TRUE)
  vartheta <- 1

  repeat {
    n_iterations <- n_iterations + 1

    # Calculate the weights and residuals
    offset <- x_tilde %*% beta
    n_passes <- 0
    for (k in 1:n_groups) {
      idx <- n_passes + seq_len(length(group_idxs[[k]]))
      n_passes <- n_passes + length(group_idxs[[k]])
      wls <- calc_weights_residuals(
        offset = offset[idx], time = time_tilde[idx], status = status_tilde[idx]
      )
      w[idx] <- wls$weights
      z[idx] <- wls$residuals + offset[idx]
    }

    # Update the coefficients
    w_tilde <- Matrix::Matrix(diag(w), sparse = TRUE)
    xwx <- Matrix::crossprod(x_tilde, w_tilde %*% x_tilde) / n_samples
    xwz <- Matrix::crossprod(x_tilde, w_tilde %*% z) / n_samples
    beta <- Matrix::solve(
      xwx + vartheta * contr2,
      xwz + vartheta * Matrix::crossprod(contr, eta - mu / vartheta),
      sparse = TRUE
    )

    # Update the auxiliary variables
    eta_old <- eta
    eta <- contr %*% beta + mu / vartheta

    ## Group Sparsity
    for (j in 1:n_features) {
      idx <- 1:n_groups * n_features - n_features + j
      etaj_norm <- norm(matrix(eta[idx]), type = "e")
      eta[idx] <- threshold(
        etaj_norm, vartheta, penalty, lambda1 * sqrt(n_groups), gamma
      ) * eta[idx] / etaj_norm
    }
    ## Transfer Global
    eta[global_indices] <- vapply(global_indices, function(idx) {
      threshold(eta[idx], vartheta, penalty, lambda2 * alpha, gamma)
    }, numeric(1))
    ## Transfer Local
    eta[local_indices] <- vapply(local_indices, function(idx) {
      threshold(eta[idx], vartheta, penalty, lambda2 * (1 - alpha), gamma)
    }, numeric(1))
    mu <- mu + vartheta * (contr %*% beta - eta)

    # Update the penalty parameter
    r <- Matrix::norm(contr %*% beta - eta, type = "I")
    s <- Matrix::norm(
      Matrix::crossprod(contr, eta - eta_old),
      type = "I"
    )
    if (r > 10 * s) vartheta <- vartheta * rho
    if (r < 10 * s) vartheta <- vartheta / rho
    vartheta <- max(1, vartheta)

    # Check the convergence
    if (n_iterations >= control$maxit) {
      convergence <- TRUE
      message <- paste0(
        "Maximum number of iterations reached (", control$maxit, ")."
      )
    }
    if (r < control$eps && s < control$eps) {
      convergence <- TRUE
      message <- paste0(
        "Convergence reached at iteration ", n_iterations, "."
      )
    }

    offset <- x_tilde %*% beta
    hazard <- exp(offset)
    risk_set <- ave(hazard, group, FUN = cumsum)
    loss <- -sum(status * (offset - log(risk_set)))
    if (-loss / null_deviance < 0.01) {
      convergence <- TRUE
      message <- paste0(
        "The log-likelihood is too small (", -loss / null_deviance,
        "). Stopping the algorithm."
      )
    }
    if (convergence) break
  }

  beta <- matrix(beta, nrow = n_features, ncol = n_groups + 1)
  xi <- matrix(eta[local_indices, 1], nrow = n_features)
  coefs_proc <- matrix(0, nrow = n_features, ncol = n_groups)
  coefs_group <- matrix(0, nrow = n_features, ncol = n_groups)
  for (i in 1:n_features) {
    is_processed <- rep(FALSE, n_groups)
    for (j in 1:n_groups) {
      if (is_processed[j]) next
      is_processed[j] <- TRUE
      coefs_group[i, j] <- j
      for (k in 1:n_groups) {
        if (is_processed[k]) next
        pos <- get_position(j, k, n_groups)
        if (abs(xi[i, pos]) < control$eps) {
          coefs_group[i, k] <- j
          is_processed[k] <- TRUE
        }
      }
    }
    for (j in unique(coefs_group[i, ])) {
      idx <- which(coefs_group[i, ] == j)
      coefs_proc[i, idx] <- mean(beta[i, idx])
    }
  }
  coefs <- coefs_proc
  coefs[abs(coefs) < control$eps] <- 0

  # Reassign the coefficients' group
  n_total_groups <- 0
  coefs_expanded <- c()
  coefs_group <- matrix(0, nrow = n_features, ncol = n_groups)
  for (j in 1:n_features) {
    feature_groups <- as.factor(as.character(coefs[j, ]))
    feature_levels <- levels(feature_groups)
    for (k in seq_along(feature_levels)) {
      idx <- feature_groups == feature_levels[k]
      coefs_group[j, idx] <- k + n_total_groups
    }
    coefs_expanded <- c(coefs_expanded, as.numeric(feature_levels))
    n_total_groups <- n_total_groups + length(feature_levels)
  }
  coefs_expanded <- as.matrix(coefs_expanded)
  sparse_mark <- coefs_expanded == 0

  # Group the features
  x_grouped <- c()
  for (k in 1:n_groups) {
    idx <- group_idxs[[k]]
    feature_map <- matrix(0, n_features, n_total_groups)
    for (j in 1:n_features) {
      feature_map[j, coefs_group[j, k]] <- 1
    }
    x_grouped_k <- x[idx, ] %*% feature_map
    x_grouped <- rbind(x_grouped, x_grouped_k)
  }
  x_grouped <- as.data.frame(x_grouped)

  # Calulate the variance-covariance matrix of non-sparse coefficients
  x_grouped_sparse <- x_grouped[, !sparse_mark]
  coefs_expanded_sparse <- coefs_expanded[!sparse_mark]
  offset <- as.matrix(x_grouped_sparse) %*% coefs_expanded_sparse
  n_nonzero <- sum(!sparse_mark)
  grads <- c()
  hessians <- c()
  for (k in 1:n_groups) {
    idx <- group_idxs[[k]]
    ghs <- calc_grads_hessians(
      offset[idx], x_grouped_sparse[idx, ], time[idx], status[idx]
    )
    grads <- rbind(grads, ghs$grads)
    hessians <- rbind(hessians, ghs$hessians)
  }
  hess <- matrix(colSums(hessians), nrow = n_nonzero, ncol = n_nonzero)
  cov_grad <- cov(grads) * n_samples / (n_samples - 1)

  sigma_coefs <- c()
  for (j in 1:n_features) {
    coef_norm <- norm(matrix(coefs[j, ]), type = "e")
    coef_j_nonzero <- coefs_group[j, abs(coefs[j, ]) > 0]
    coef_j_center <- beta[j, n_groups + 1]
    coef_idx <- unique(coef_j_nonzero)
    coef_j_sparse <- coefs[j, coef_idx]
    # TODO: pair-wise group sparsity needs to be implemented
    sigma_coefs <- c(
      sigma_coefs,
      penalty_grad(
        coef_norm, penalty, lambda1 * sqrt(n_groups), gamma
      ) / coef_norm + penalty_grad(
        coef_j_sparse - coef_j_center, penalty, lambda2 * alpha, gamma
      ) / abs(coef_j_sparse)
    )
  }
  sigma_coefs <- diag(sigma_coefs * n_samples)
  hess_inv <- solve(hess + sigma_coefs)
  var <- hess_inv %*% cov_grad %*% hess_inv

  # Unscale the coefficients
  coefficients <- cbind(coefs, beta[, n_groups + 1])
  coefficients[abs(coefficients) < control$eps] <- 0
  coefficients <- sweep(coefficients, 1, x_scale, `/`)
  colnames(coefficients) <- c(group_levels, "Center")
  rownames(coefficients) <- colnames(x)

  # Return the fitted model
  fit <- list(
    coefficients = coefficients, coefficients_group = coefs_group,
    coefficients_expanded = coefs_expanded, var = var,
    group_levels = group_levels, logLik = -loss, iter = n_iterations,
    message = message, penalty = penalty, lambda1 = lambda1, lambda2 = lambda2,
    alpha = alpha, gamma = gamma, formula = formula, call = match.call()
  )
  class(fit) <- "coxens"
  return(fit)
}
