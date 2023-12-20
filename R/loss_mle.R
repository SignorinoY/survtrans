loss_mle <- function(data, cbh, fit) {
  form <- fit$formula
  coef <- fit$coefficients
  mf <- stats::model.frame(form, data)
  y <- stats::model.response(mf)
  x <- stats::model.matrix(form, data)
  x <- x[, !colnames(x) %in% "(Intercept)"]
  if (!is.matrix(x)) x <- as.matrix(x)

  if (is.null(coef$gamma)) {
    bh <- y[, 1]**2
  } else {
    gamma <- coef$gamma
    b <- splines::bs(y[, 1],
      knots = gamma$knots, Boundary.knots = gamma$Boundary.knots,
      degree = gamma$degree, intercept = TRUE
    )
    bh <- exp(b %*% gamma$alpha)
  }

  sum(y[, 2] * (log(bh) + x %*% coef$beta) - cbh * exp(x %*% coef$beta))
}
