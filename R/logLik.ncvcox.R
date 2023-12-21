#' @export
logLik.ncvcox <- function(object, data, offset, ...) {
  # load x from data
  mf <- model.frame(object$formula, data)
  y <- model.response(mf)
  x <- model.matrix(object$formula, data)
  x <- x[, -1] # Remove the intercept column

  # Sort the data by time
  time <- y[, 1]
  status <- y[, 2]
  sorted <- order(time, decreasing = TRUE)
  time <- time[sorted]
  status <- status[sorted]
  x <- x[sorted, , drop = FALSE]

  # Properties of the data
  n_samples <- nrow(x)

  # Check the offset argument
  if (missing(offset)) offset <- rep(0.0, n_samples)
  offset <- offset[sorted]

  eta <- x %*% object$coefficients + offset
  haz <- exp(eta)
  risk_set <- cumsum(haz)
  risk_set <- ave(risk_set, time, FUN = max)
  log_lik <- sum(status * (eta - log(risk_set)))

  return(log_lik)
}
