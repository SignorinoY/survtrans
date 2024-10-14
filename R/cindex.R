cindex <- function(fit, newdata) {
  # load x from newdata
  mf <- model.frame(fit$formula, newdata)
  x <- model.matrix(fit$formula, newdata)
  x <- x[, -1] # Remove the intercept column

  # Load time and status from newdata
  y <- model.response(mf)
  time <- y[, 1]
  status <- y[, 2]

  # Calculate the linear predictor
  lp <- as.vector(x %*% fit$coefficients)

  # Calculate the concordance index
  time_index <- outer(time, time, `-`) > 0
  risk_index <- outer(lp, lp, `-`) < 0
  cindex <- sum(sweep(time_index * risk_index, MARGIN = 2, status, `*`)) /
    sum(sweep(time_index, MARGIN = 2, status, `*`))
  return(cindex)
}
