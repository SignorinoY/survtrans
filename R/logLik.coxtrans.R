#' @export
logLik.coxtrans <- function(object, data, group, ...) {
  data_ <- preprocess_data(object$formula, data, group = group)
  x <- data_$x
  time <- data_$time
  status <- data_$status
  group <- data_$group

  # Properties of the data
  n_groups <- length(unique(group))
  group_levels <- levels(group)

  # Calculate the log-likelihood
  eta <- x %*% object$coefficients[, n_groups + 1]
  for (k in 1:n_groups) {
    ind <- which(group == group_levels[k])
    eta[ind] <- eta[ind] + x[ind, ] %*% object$coefficients[, k]
  }
  haz <- exp(eta)
  risk_set <- ave(haz, group, FUN = cumsum)
  for (k in 1:n_groups) {
    ind <- which(group == group_levels[k])
    risk_set[ind] <- ave(risk_set[ind], time[ind], FUN = max)
  }
  log_lik <- sum(status * (eta - log(risk_set)))

  return(log_lik)
}
