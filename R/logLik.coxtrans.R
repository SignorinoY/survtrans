#' @export
logLik.coxtrans <- function(object, data, group, ...) {
  data_ <- preprocess_data(object$formula, data, group = group)
  x <- data_$x
  time <- data_$time
  status <- data_$status
  group <- data_$group

  group_levels <- unique(group)
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

  loglik <- sum(status * (offset - log(risk_set)))
  return(loglik)
}