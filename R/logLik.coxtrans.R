#' Log-likelihood of a Multi-source Cox proportional hazards model
#' @param object a coxtrans object.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param ... Unused.
#' @return the log-likelihood of the model.
#' @export
logLik.coxtrans <- function(object, data, group, ...) {
  data <- preprocess_data(object$formula, data, group = group)
  x <- data$x
  x_scale <- attr(x, "scale")
  time <- data$time
  status <- data$status
  group <- data$group

  group_levels <- object$group_levels
  n_groups <- length(group_levels)

  beta <- object$beta
  beta <- beta * x_scale
  eta <- object$eta
  eta <- sweep(eta, 2, x_scale, `*`)

  offset <- x %*% beta
  for (k in 1:n_groups) {
    ind <- group == group_levels[k]
    offset[ind] <- offset[ind] + x[ind, ] %*% eta[, k]
  }
  hazard <- exp(offset)
  risk_set <- ave(hazard, group, FUN = cumsum)
  for (k in 1:n_groups) {
    ind <- group == group_levels[k]
    risk_set[ind] <- ave(risk_set[ind], time[ind], FUN = max)
  }

  loglik <- sum(status * (offset - log(risk_set)))
  return(loglik)
}
