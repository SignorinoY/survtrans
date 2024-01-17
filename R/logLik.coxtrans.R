#' @export
logLik.coxtrans <- function(object, data, group, ...) {
  # Load the data
  group_ <- group
  data_ <- data
  data <- preprocess_data(object$formula, data_, group = group_)
  x <- data$x
  time <- data$time
  status <- data$status
  group <- data$group

  group_levels <- object$group_levels
  n_groups <- length(group_levels)
  offset <- x %*% object$beta
  for (k in 1:n_groups) {
    ind <- group == group_levels[k]
    offset[ind] <- offset[ind] + x[ind, ] %*% object$eta[, k]
  }
  hazard <- exp(offset)
  risk_set <- ave(hazard, group, FUN = cumsum)
  for (k in 1:n_groups) {
    ind <- group == group_levels[k]
    risk_set[ind] <- ave(risk_set[ind], time[ind], FUN = max)
  }
  idx <- group == object$group_target
  risk_set[idx] <- ave(risk_set[idx], time[idx], FUN = max)

  loglik <- sum(status * (offset - log(risk_set)))
  return(loglik)
}
