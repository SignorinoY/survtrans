#' Plot the cross-validation curve from a cv_ncvcox x
#' @param x a cv_ncvcox x.
#' @param ... Unused.
#' @return NULL
#' @export
plot.cv_ncvcox <- function(x, ...) {
  cv_low <- x$cvm - x$cvsd
  cv_high <- x$cvm + x$cvsd
  lambda.min <- x$lambdas[which.max(x$cvm)]
  lambda.1se <- x$lambdas[which.max(x$cvm + x$cvsd)]
  n_zeros <- rowSums(x$coefficients == 0)

  plot(
    x = log(x$lambdas),
    y = x$cvm,
    ylim = range(cv_low, cv_high),
    xlab = expression(Log(lambda)),
    ylab = x$name,
    type = "n"
  )
  points(log(x$lambdas), x$cvm, pch = 20, col = "red")
  error.bars(
    log(x$lambdas), cv_high, cv_low,
    width = 0.01, col = "darkgrey"
  )
  axis(
    side = 3, at = log(x$lambdas), labels = n_zeros, tick = FALSE, line = 0
  )
  abline(v = log(lambda.min), lty = 3)
  abline(v = log(lambda.1se), lty = 3)
  invisible()
}
