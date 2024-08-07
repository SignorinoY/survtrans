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
#' fit <- coxens1(
#'   formula, sim2, group,
#'   lambda1 = 0.002, lambda2 = 0.005, penalty = "SCAD"
#' )
#' fit$coefficients
coxens1 <- function(
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
  lambda2_ <- lambda2
  alpha_ <- alpha
  lambda2 <- lambda2_ * alpha_
  lambda3 <- lambda2_ * (1 - alpha_)

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
      kronecker(matrix(1, n_groups, 1), diag(n_features))
    ),
    cbind(
      Matrix::Diagonal(n_features * n_groups),
      matrix(0, nrow = n_features * n_groups, ncol = n_features)
    ),
    cbind(
      kronecker(e, diag(n_features)),
      matrix(0, n_groups * (n_groups - 1) * n_features / 2, n_features)
    )
  )
  n_constraints <- nrow(contr)
  contr2 <- Matrix::crossprod(contr)

  sparse_idx <- 1:(n_features * n_groups)
  global_idx <- (n_features * n_groups + 1):(2 * n_features * n_groups)
  local_idx <- (n_features * n_groups * 2 + 1):n_constraints

  x_tilde <- cbind(
    Matrix::bdiag(lapply(group_idxs, function(idx) x[idx, ])),
    do.call(rbind, lapply(group_idxs, function(idx) x[idx, ]))
  )
  time_tilde <- unlist(lapply(group_idxs, function(idx) time[idx]))
  status_tilde <- unlist(lapply(group_idxs, function(idx) status[idx]))

  alpha <- Matrix::Matrix(0, n_constraints, 1, sparse = TRUE)
  mu <- Matrix::Matrix(0, n_constraints, 1, sparse = TRUE)
  vartheta <- 1

  repeat {
    n_iterations <- n_iterations + 1

    # Calculate the weights and residuals
    offset <- x_tilde %*% theta
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
    theta <- Matrix::solve(
      xwx + vartheta * contr2,
      xwz + vartheta * Matrix::crossprod(contr, alpha - mu / vartheta),
      sparse = TRUE
    )

    # Update the auxiliary variables
    alpha_old <- alpha
    alpha <- contr %*% theta + mu / vartheta

    ## Group Sparsity
    alpha[sparse_idx] <- vapply(sparse_idx, function(idx) {
      threshold(alpha[idx], vartheta, penalty, lambda1, gamma)
    }, numeric(1))
    ## Transfer Global
    alpha[global_idx] <- vapply(global_idx, function(idx) {
      threshold(alpha[idx], vartheta, penalty, lambda2, gamma)
    }, numeric(1))
    ## Transfer Local
    alpha[local_idx] <- vapply(local_idx, function(idx) {
      threshold(alpha[idx], vartheta, penalty, lambda3, gamma)
    }, numeric(1))
    mu <- mu + vartheta * (contr %*% theta - alpha)

    # Update the penalty parameter
    r <- Matrix::norm(contr %*% theta - alpha, type = "I")
    s <- Matrix::norm(
      Matrix::crossprod(contr, alpha - alpha_old),
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

  # Recover the coefficients
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

  # Reassign the coefficients' group
  n_total_groups <- 0
  eta_idx <- matrix(0, nrow = n_features, ncol = n_groups)
  eta_expanded <- c()
  for (j in 1:n_features) {
    feature_groups <- as.factor(as.character(eta[j, ]))
    feature_levels <- levels(feature_groups)
    for (k in seq_along(feature_levels)) {
      idx <- feature_groups == feature_levels[k]
      eta_idx[j, idx] <- k + n_total_groups
    }
    eta_expanded <- c(eta_expanded, as.numeric(feature_levels))
    n_total_groups <- n_total_groups + length(feature_levels)
  }
  theta_expanded <- as.matrix(c(eta_expanded, beta))

  # Group the features
  z <- c()
  for (k in 1:n_groups) {
    idx <- group_idxs[[k]]
    feature_map <- matrix(0, n_features, n_total_groups)
    for (j in 1:n_features) {
      feature_map[j, eta_idx[j, k]] <- 1
    }
    z <- rbind(z, x[idx, ] %*% feature_map)
  }

  # Calulate the variance-covariance matrix of non-sparse coefficients
  z1 <- as.matrix(cbind(z[, eta_expanded != 0], x[, beta != 0]))
  theta1 <- c(
    eta_expanded[eta_expanded != 0], beta[beta != 0]
  )
  offset <- z1 %*% theta1
  n_nonzero <- length(theta1)
  grads <- c()
  hessians <- c()
  for (k in 1:n_groups) {
    idx <- group_idxs[[k]]
    ghs <- calc_grads_hessians(
      offset[idx], z1[idx, ], time[idx], status[idx]
    )
    grads <- rbind(grads, ghs$grads)
    hessians <- rbind(hessians, ghs$hessians)
  }
  hess <- matrix(colSums(hessians), nrow = n_nonzero, ncol = n_nonzero)
  cov_grad <- cov(grads) * n_samples / (n_samples - 1)

  hess_inv <- solve(hess)
  var <- hess_inv %*% cov_grad %*% hess_inv

  # Unscale the coefficients
  coefficients <- sweep(cbind(eta, beta), 1, x_scale, `/`)
  colnames(coefficients) <- c(group_levels, "Center")
  rownames(coefficients) <- colnames(x)

  # Return the fitted model
  fit <- list(
    coefficients = coefficients, coefficients_group = eta_idx,
    coefficients_expanded = theta_expanded, var = var,
    group_levels = group_levels, logLik = -loss, iter = n_iterations,
    message = message, penalty = penalty, lambda1 = lambda1, lambda2 = lambda2_,
    alpha = alpha_, gamma = gamma, formula = formula, call = match.call()
  )
  class(fit) <- "coxens"
  return(fit)
}
