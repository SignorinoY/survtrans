#' @export
logLik.coxtrans <- function(object, data, bgroup, egroup, ...) {
  data_ <- preprocess_data(object$formula, data)
  x <- data_$x
  time <- data_$time
  status <- data_$status
  bgroup <- bgroup[data_$sorted]
  egroup <- egroup[data_$sorted]

  # Properties of the data
  n_bgroups <- length(unique(bgroup))
  bgroup_levels <- levels(bgroup)
  n_egroups <- length(unique(egroup))
  egroup_levels <- levels(egroup)

  # Calculate the log-likelihood
  eta <- x %*% object$coefficients[, n_egroups + 1]
  for (k in 1:n_egroups) {
    ind <- which(egroup == egroup_levels[k])
    eta[ind] <- eta[ind] + x[ind, ] %*% object$coefficients[, k]
  }
  haz <- exp(eta)
  risk_set <- ave(haz, bgroup, FUN = cumsum)
  for (k in 1:n_bgroups) {
    ind <- which(bgroup == bgroup_levels[k])
    risk_set[ind] <- ave(risk_set[ind], time[ind], FUN = max)
  }
  log_lik <- sum(status * (eta - log(risk_set)))

  return(log_lik)
}
