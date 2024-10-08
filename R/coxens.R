#' @title Multiple-sources Cox proportional hazards model with group sparsity
#' and Local-Global transfer
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param lambda1 a non-negative value specifying the sparse penalty
#' parameter. The default is 0.
#' @param lambda2 a non-negative value specifying the global biased penalty
#' parameter. The default is 0.
#' @param lambda3 a non-negative value specifying the local biased penalty
#' parameter. The default is 0.
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
#'   lambda1 = 2e-2, lambda3 = 3e-3, penalty = "SCAD"
#' )
#' summary(fit)
coxens <- function(
    formula, data, group, lambda1 = 0, lambda2 = 0, lambda3 = 0,
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
  n_samples_total <- nrow(x)
  n_features <- ncol(x)
  n_groups <- length(unique(group))
  group_levels <- levels(group)
  group_idxs <- lapply(group_levels, function(x) which(group == x))
  n_samples_group <- sapply(group_idxs, length)

  risk_set_size <- ave(rep(1, n_samples_total), group, FUN = cumsum)
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
  init <- sweep(matrix(init, nrow = n_features), 1, x_scale, `*`)
  theta <- matrix(
    data = init[, 1:(n_groups + 1), drop = FALSE],
    nrow = n_features * (n_groups + 1), ncol = 1
  )

  # Initialize the training process
  n_iterations <- 0
  message <- ""
  convergence <- FALSE

  offset <- numeric(n_samples_total)
  w <- numeric(n_samples_total)
  z <- numeric(n_samples_total)

  idx <- which(lower.tri(matrix(1, n_groups, n_groups)), arr.ind = TRUE)
  e <- matrix(0, nrow = nrow(idx), ncol = n_groups)
  e[cbind(seq_len(nrow(idx)), idx[, 1])] <- 1
  e[cbind(seq_len(nrow(idx)), idx[, 2])] <- -1

  contr_sum <- Matrix::Matrix(
    cbind(
      kronecker(
        matrix(n_samples_group / n_samples_total, nrow = 1),
        diag(n_features)
      ),
      0 * diag(n_features)
    ),
    sparse = TRUE
  )
  contr_penalty <- rbind(
    cbind(
      Matrix::Diagonal(n_features * n_groups),
      kronecker(matrix(1, n_groups, 1), diag(n_features))
    ),
    cbind(
      Matrix::Diagonal(n_features * n_groups),
      Matrix::Matrix(0, n_features * n_groups, n_features, sparse = TRUE)
    ),
    cbind(
      kronecker(e, diag(n_features)),
      Matrix::Matrix(
        0, n_groups * (n_groups - 1) * n_features / 2, n_features,
        sparse = TRUE
      )
    )
  )
  n_constraints <- nrow(contr_penalty)
  contr_sum2 <- Matrix::crossprod(contr_sum)
  contr_penalty2 <- Matrix::crossprod(contr_penalty)

  sparse_idx <- 1:(n_groups * n_features)
  global_idx <- (n_groups * n_features + 1):(2 * n_groups * n_features)
  local_idx <- (2 * n_groups * n_features + 1):n_constraints

  x_tilde <- cbind(
    Matrix::bdiag(lapply(group_idxs, function(idx) x[idx, ])),
    do.call(rbind, lapply(group_idxs, function(idx) x[idx, ]))
  )
  time_tilde <- unlist(lapply(group_idxs, function(idx) time[idx]))
  status_tilde <- unlist(lapply(group_idxs, function(idx) status[idx]))

  alpha <- Matrix::Matrix(0, n_constraints, 1, sparse = TRUE)
  mu <- Matrix::Matrix(0, n_features, 1, sparse = TRUE)
  nu <- Matrix::Matrix(0, n_constraints, 1, sparse = TRUE)
  vartheta <- 1

  repeat {
    n_iterations <- n_iterations + 1

    # Calculate the weights and residuals
    offset <- x_tilde %*% theta
    n_passes <- 0
    for (k in 1:n_groups) {
      idx <- n_passes + seq_len(length(group_idxs[[k]]))
      n_passes <- n_passes + length(group_idxs[[k]])
      wls <- approx_likelihood(
        offset = offset[idx], time = time_tilde[idx], status = status_tilde[idx]
      )
      w[idx] <- wls$weights
      z[idx] <- wls$residuals + offset[idx]
    }

    # Update the coefficients
    xwx <- Matrix::crossprod(x_tilde, w * x_tilde) / n_samples_total
    xwz <- Matrix::crossprod(x_tilde, w * z) / n_samples_total
    lhs <- xwx + vartheta * (contr_sum2 + contr_penalty2)
    rhs <- xwz - Matrix::crossprod(contr_sum, mu) +
      vartheta * Matrix::crossprod(contr_penalty, alpha - nu / vartheta)
    theta <- Matrix::solve(lhs, rhs, sparse = TRUE, tol = control$eps)

    # Update the auxiliary variables
    alpha_old <- alpha
    alpha <- contr_penalty %*% theta + nu / vartheta

    alpha[sparse_idx] <- threshold_prox(
      alpha[sparse_idx], vartheta, penalty, lambda1, gamma
    )
    alpha[global_idx] <- threshold_prox(
      alpha[global_idx], vartheta, penalty, lambda2, gamma
    )
    alpha[local_idx] <- threshold_prox(
      alpha[local_idx], vartheta, penalty, lambda3, gamma
    )
    mu <- mu + vartheta * contr_sum %*% theta
    nu <- nu + vartheta * (contr_penalty %*% theta - alpha)

    # Update the penalty parameter
    r <- Matrix::norm(contr_penalty %*% theta - alpha, type = "I") +
      Matrix::norm(contr_sum %*% theta, type = "I")
    s <- Matrix::norm(
      Matrix::crossprod(contr_penalty, alpha - alpha_old),
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

    offset <- x_tilde %*% theta
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

  theta <- matrix(theta, nrow = n_features, ncol = n_groups + 1)
  alpha_local <- matrix(alpha[local_idx, 1], nrow = n_features)
  eta <- matrix(0, nrow = n_features, ncol = n_groups)
  eta_idx <- matrix(0, nrow = n_features, ncol = n_groups)
  for (i in 1:n_features) {
    is_processed <- rep(FALSE, n_groups)
    for (j in 1:n_groups) {
      if (is_processed[j]) next
      is_processed[j] <- TRUE
      eta_idx[i, j] <- j
      for (k in 1:n_groups) {
        if (is_processed[k]) next
        pos <- get_position(j, k, n_groups)
        if (abs(alpha_local[i, pos]) < control$eps) {
          eta_idx[i, k] <- j
          is_processed[k] <- TRUE
        }
      }
    }
    for (j in unique(eta_idx[i, ])) {
      idx <- which(eta_idx[i, ] == j)
      eta[i, idx] <- mean(theta[i, idx])
    }
  }
  eta[abs(eta) < control$eps] <- 0

  beta <- theta[, n_groups + 1]
  beta[abs(beta) < control$eps] <- 0

  # Unscale the coefficients
  coefficients <- sweep(cbind(eta, beta), 1, x_scale, `/`)
  colnames(coefficients) <- c(group_levels, "Center")
  rownames(coefficients) <- colnames(x)

  # Return the fitted model
  fit <- list(
    coefficients = coefficients,
    logLik = -loss,
    iter = n_iterations,
    message = message,
    penalty = penalty,
    lambda1 = lambda1,
    lambda2 = lambda2,
    lambda3 = lambda3,
    gamma = gamma,
    formula = formula,
    call = match.call(),
    time = time,
    status = status,
    group = group,
    x = sweep(x, 2, x_scale, `/`)
  )
  class(fit) <- "coxens"
  return(fit)
}
