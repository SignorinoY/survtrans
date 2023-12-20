#' @importFrom stats predict
#' @export
predict.ncvcox <- function(object, newdata, type = c("risk", "lp"), ...) {
  type <- match.arg(type)
  # Load X, y from formula and newdata
  x <- model.matrix(object$formula, newdata)
  x <- x[, -1] # Remove the intercept column
  if (type == "lp") {
    lp <- x %*% object$coefficients
    return(lp)
  }
}
