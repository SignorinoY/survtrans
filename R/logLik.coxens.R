#' Log-likelihood of a subgroup analysis for Cox proportional hazards model
#' @param object a coxens object.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param ... Unused.
#' @return the log-likelihood of the model.
#' @export
logLik.coxens <- function(object, data, group, ...) {
  data <- preprocess_data(object$formula, data, group = group)
  x <- data$x
  x_scale <- attr(x, "scale")
  time <- data$time
  status <- data$status
  group <- data$group

  group_levels <- object$group_levels
  n_groups <- length(group_levels)

  theta <- object$coefficients
  theta <- theta * x_scale

  beta <- theta[, 1:n_groups] + theta[, (n_groups + 1)]

  offset <- numeric(nrow(x))
  for (k in 1:n_groups) {
    ind <- group == group_levels[k]
    offset[ind] <- x[ind, ] %*% beta[, k]
  }
  hazard <- exp(offset - max(offset))
  risk_set <- ave(hazard, group, FUN = cumsum)
  for (k in 1:n_groups) {
    ind <- group == group_levels[k]
    risk_set[ind] <- ave(risk_set[ind], time[ind], FUN = max)
  }

  loglik <- sum(status * (offset - log(risk_set) - max(offset)))
  return(loglik)
}
