loss_mple <- function(data, fit) {
  form <- fit$formula
  coef <- fit$coefficients
  mf <- stats::model.frame(form, data)
  y <- stats::model.response(mf)
  x <- stats::model.matrix(form, data)
  x <- x[, !colnames(x) %in% "(Intercept)"]
  if (!is.matrix(x)) x <- as.matrix(x)

  time <- y[, 1]
  status <- y[, 2]
  sorted <- order(time, decreasing = TRUE)
  time <- time[sorted]
  status <- status[sorted]
  x <- x[sorted, ]
  phi <- x %*% coef$beta

  sum(status * phi - log(cumsum(exp(phi))))
}
