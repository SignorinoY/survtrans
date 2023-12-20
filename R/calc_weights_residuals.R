calc_weights_residuals <- function(coef, x, time, status, offset) {
  n_samples <- nrow(x)

  # Update hazard
  haz <- exp(x %*% coef + offset)

  # Update risk set
  risk_set <- cumsum(haz)
  risk_set <- ave(risk_set, time, FUN = max)

  # Update weights and residuals
  weights <- rep(0.0, n_samples)
  residuals <- rep(0.0, n_samples)
  denominator_risk_set <- 0.0
  denominator_risk_set_sq <- 0.0
  i <- n_samples
  while (i > 0) {
    ti <- time[i]
    c <- 0
    k <- i
    while (k > 0 && ti == time[k]) {
      if (status[k] == 1) {
        c <- c + 1
      }
      k <- k - 1
    }

    v <- risk_set[i]
    denominator_risk_set <- denominator_risk_set + c / v
    denominator_risk_set_sq <- denominator_risk_set_sq + c / (v * v)

    while (i > k) {
      v <- haz[i]
      weights[i] <- v * (denominator_risk_set - v * denominator_risk_set_sq)
      residuals[i] <- (status[i] - v * denominator_risk_set) / weights[i]
      i <- i - 1
    }
  }
  return(list(weights = weights, residuals = residuals))
}
