calc_weights_residuals <- function(offset, time, status) {
  n_samples <- length(time)
  if (n_samples == 1) return(list(weights = 0, residuals = 0))
  # Update hazard
  haz <- exp(offset)

  # Update risk set
  risk_set <- cumsum(haz)
  for (i in n_samples:2) {
    if (time[i] == time[i - 1]) {
      risk_set[i - 1] <- risk_set[i]
    }
  }

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
  residuals[is.na(residuals)] <- 0
  return(list(weights = weights, residuals = residuals))
}
