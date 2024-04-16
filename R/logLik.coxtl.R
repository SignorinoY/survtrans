#' Log-likelihood of Transfer Learning for Cox proportional hazards model
#' @param object a coxtl object.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param ... Unused.
#' @return the log-likelihood of the model.
#' @export
logLik.coxtl <- function(object, data, group, ...) {
  data <- preprocess_data(object$formula, data, group = group)
  x <- data$x
  x_scale <- attr(x, "scale")
  time <- data$time
  status <- data$status
  group <- data$group

  group_levels <- object$group_levels
  group_levels_drop <- group_levels[group_levels != object$target]
  n_groups <- length(group_levels)

  beta <- object$beta
  beta <- beta * x_scale
  eta <- object$eta
  eta <- sweep(eta, 1, x_scale, `*`)

  theta <- x %*% beta
  for (k in 1:(n_groups - 1)) {
    idx <- group == group_levels_drop[k]
    theta[idx] <- theta[idx] + x[idx, ] %*% eta[, k]
  }
  hazard <- exp(theta - max(theta))
  risk_set <- ave(hazard, group, FUN = cumsum)
  for (k in 1:n_groups) {
    ind <- group == group_levels[k]
    risk_set[ind] <- ave(risk_set[ind], time[ind], FUN = max)
  }

  loglik <- sum(status * (theta - log(risk_set) - max(theta)))
  return(loglik)
}
