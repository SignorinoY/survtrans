#' Log-likelihood of Non-convex penalized Cox proportional hazards model
#' @param object a ncvcox object.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param offset a numeric vector specifying the offset.
#' @param ... Unused.
#' @export
logLik.ncvcox <- function(object, data, group, offset, ...) {
  # Load the data
  data <- preprocess_data(object$formula, data, group, offset)
  x <- data$x
  x_scale <- attr(x, "scale")
  time <- data$time
  status <- data$status
  group <- data$group
  offset <- data$offset

  # Properties of the data
  group_levels <- object$group_levels
  n_groups <- length(group_levels)
  n_samples <- nrow(x)

  beta <- object$coefficients
  beta <- beta * x_scale

  theta <- rep(0, n_samples)
  for (k in 1:n_groups) {
    idx <- group == group_levels[k]
    theta[idx] <- x[idx, ] %*% beta + offset[idx]
  }

  hazard <- exp(theta)
  risk_set <- ave(hazard, group, FUN = cumsum)
  for (k in 1:n_groups) {
    idx <- group == group_levels[k]
    risk_set[idx] <- ave(risk_set[idx], time[idx], FUN = max)
  }
  log_lik <- sum(status * (theta - log(risk_set)))
  return(log_lik)
}
