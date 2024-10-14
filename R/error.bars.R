#' @note The implementation of the `error.bars` function was inspired by
#' or adapted from the `glmnet` package, which is available at
#' \url{https://cran.r-project.org/package=glmnet}.
error.bars <-
  function(x, upper, lower, width = 0.02, ...) {
    xlim <- range(x)
    barw <- diff(xlim) * width
    segments(x, upper, x, lower, ...)
    segments(x - barw, upper, x + barw, upper, ...)
    segments(x - barw, lower, x + barw, lower, ...)
    range(upper, lower)
  }
