#' @export
logLik.coxtrans <- function(object, data, bgroup, egroup, ...) {
  data_ <- preprocess_data(object$formula, data)
  x <- data_$x
  time <- data_$time
  status <- data_$status
  bgroup <- bgroup[data_$sorted]
  egroup <- egroup[data_$sorted, ]

  # Properties of the data
  n_features <- ncol(x)
  n_bgroups <- length(unique(bgroup))
  bgroup_levels <- levels(bgroup)
  n_egroups <- c()
  for (k in 1:n_features) {
    n_egroups[k] <- length(unique(egroup[, k]))
  }
  egroup_levels <- list()
  for (k in 1:n_features) {
    egroup_levels[[k]] <- levels(as.factor(egroup[, k]))
  }

  # Calculate the log-likelihood
  eta <- x %*% object$coefficients[, max(n_egroups) + 1]
  for (j in 1:n_features) {
    for (k in 1:n_egroups[j]) {
      ind <- which(egroup[, j] == egroup_levels[[j]][k])
      eta[ind] <- eta[ind] + x[ind, j] * object$coefficients[j, k]
    }
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
