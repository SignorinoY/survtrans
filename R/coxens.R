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
#'   lambda1 = 0.02, lambda2 = 0.005, penalty = "SCAD"
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
  E <- Matrix::Matrix(0, nrow(idx), n_groups, sparse = TRUE)
  E[cbind(seq_len(nrow(idx)), idx[, 1])] <- 1
  E[cbind(seq_len(nrow(idx)), idx[, 2])] <- -1

  A <- Matrix::Matrix(
    cbind(
      kronecker(
        matrix(1, nrow = 1, ncol = n_groups),
        diag(n_features)
      ),
      -n_groups * diag(n_features)
    ),
    sparse = TRUE
  )
  B <- rbind(
    cbind(
      Matrix::Diagonal(n_features * n_groups),
      matrix(0, nrow = n_features * n_groups, ncol = n_features)
    ),
    cbind(
      Matrix::Diagonal(n_features * n_groups),
      -kronecker(matrix(1, n_groups, 1), diag(n_features))
    ),
    cbind(
      kronecker(E, diag(n_features)),
      matrix(0, n_groups * (n_groups - 1) * n_features / 2, n_features)
    )
  )
  n_constraints <- nrow(B)
  AA <- Matrix::crossprod(A)
  BB <- Matrix::crossprod(B)

  x_tilde <- Matrix::bdiag(lapply(group_idxs, function(idx) x[idx, ]))
  x_tilde <- cbind(x_tilde, matrix(0, nrow = n_samples, ncol = n_features))
  time_tilde <- unlist(lapply(group_idxs, function(idx) time[idx]))
  status_tilde <- unlist(lapply(group_idxs, function(idx) status[idx]))

  eta <- Matrix::Matrix(0, n_constraints, 1, sparse = TRUE)
  mu <- Matrix::Matrix(0, n_features, 1, sparse = TRUE)
  nu <- Matrix::Matrix(0, n_constraints, 1, sparse = TRUE)
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
      xwx + vartheta * (AA + BB),
      xwz - Matrix::crossprod(A, mu) +
        vartheta * Matrix::crossprod(B, eta - nu / vartheta),
      sparse = TRUE
    )

    # Update the auxiliary variables
    eta_old <- eta
    eta <- B %*% beta + nu / vartheta

    ## Group Sparsity
    for (j in 1:n_features) {
      idx <- 1:n_groups * n_features - n_features + j
      etaj_norm <- norm(matrix(eta[idx]), type = "e")
      eta[idx] <- threshold(
        etaj_norm, vartheta, penalty, lambda1 * sqrt(n_groups), gamma
      ) * eta[idx] / etaj_norm
    }
    ## Transfer Global
    global_indices <- (n_groups * n_features + 1):
      (n_groups * n_features + n_features * n_groups)
    eta[global_indices] <- vapply(global_indices, function(idx) {
      threshold(eta[idx], vartheta, penalty, lambda2 * alpha, gamma)
    }, numeric(1))
    ## Transfer Local
    local_indices <- (2 * n_groups * n_features + 1):n_constraints
    eta[local_indices] <- vapply(local_indices, function(idx) {
      threshold(eta[idx], vartheta, penalty, lambda2 * (1 - alpha), gamma)
    }, numeric(1))
    mu <- mu + vartheta * A %*% beta
    nu <- nu + vartheta * (B %*% beta - eta)

    # Update the penalty parameter
    r <- max(
      Matrix::norm(A %*% beta, type = "I"),
      Matrix::norm(B %*% beta - eta, type = "I")
    )
    s <- Matrix::norm(Matrix::crossprod(B, eta - eta_old), type = "e")
    if (r > 10 * s) vartheta <- vartheta * rho
    if (r < 10 * s) vartheta <- vartheta / rho
    vartheta <- max(1, vartheta)

    # Check the convergence
    offset <- x_tilde %*% beta
    hazard <- exp(offset)
    risk_set <- ave(hazard, group, FUN = cumsum)
    loss <- -sum(status * (offset - log(risk_set)))

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
    if (-loss / null_deviance < 0.01) {
      convergence <- TRUE
      message <- paste0(
        "The log-likelihood is too small (", -loss / null_deviance,
        "). Stopping the algorithm."
      )
    }
    if (convergence) break
  }

  # Unscale the coefficients
  beta <- matrix(beta, nrow = n_features, ncol = n_groups + 1)
  beta <- beta[, 1:n_groups]
  beta[abs(beta) < control$eps] <- 0
  beta <- cbind(beta, rowMeans(beta))
  coefficients <- sweep(beta, 1, x_scale, `/`)
  colnames(coefficients) <- c(group_levels, "Center")
  rownames(coefficients) <- colnames(x)

  # Return the fitted model
  fit <- list(
    coefficients = coefficients, group_levels = group_levels, logLik = -loss,
    iter = n_iterations, message = message, penalty = penalty,
    lambda1 = lambda1, lambda2 = lambda2, alpha = alpha,
    gamma = gamma, formula = formula, call = match.call()
  )
  class(fit) <- "coxens"
  return(fit)
}
