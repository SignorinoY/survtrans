calc_grads_hessians <- function(offset, x, time, status) {
  n_samples <- length(time)
  n_features <- ncol(x)
  if (n_samples == 1) {
    return(list(
      grads = rep(0, n_features),
      hessians = rep(0, n_features^2)
    ))
  }

  hazard <- exp(offset)
  risk_set <- cumsum(hazard)
  risk_set <- ave(risk_set, time, FUN = max)

  hazard_x <- sweep(x, 1, hazard, "*")
  risk_set_x <- apply(hazard_x, 2, cumsum)
  for (j in 1:n_features) {
    risk_set_x[, j] <- ave(risk_set_x[, j], time, FUN = max)
  }
  risk_set_x_ratio <- sweep(risk_set_x, 1, risk_set, "/")

  hazard_xx <- t(apply(x, 1, function(x) x %*% t(x)))
  hazard_xx <- sweep(hazard_xx, 1, hazard, "*")
  risk_set_xx <- apply(hazard_xx, 2, cumsum)
  for (j in 1:(n_features^2)) {
    risk_set_xx[, j] <- ave(risk_set_xx[, j], time, FUN = max)
  }
  risk_set_xx_ratio <- sweep(risk_set_xx, 1, risk_set, "/")
  risk_set_x2 <- t(apply(risk_set_x, 1, function(x) x %*% t(x)))
  risk_set_x2_ratio <- sweep(risk_set_x2, 1, risk_set^2, "/")

  grads <- sweep(x - risk_set_x_ratio, 1, status, "*")
  hessians <- sweep(risk_set_xx_ratio - risk_set_x2_ratio, 1, status, "*")
  return(list(grads = grads, hessians = hessians))
}
