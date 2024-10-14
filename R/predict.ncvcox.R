#' Predict method for \code{ncvcox} objects
#' @param object a \code{ncvcox} object.
#' @param newdata a data frame in which to look for variables with which to
#' predict.
#' @param type the type of prediction to be carried out. The default is "risk".
#' @param ... Unused.
#' @return a vector of predicted values.
#' @export
predict.ncvcox <- function(object, newdata, type = c("risk", "lp"), ...) {
  type <- match.arg(type)
  x <- model.matrix(object$formula, newdata)[, -1]
  if (type == "lp") {
    lp <- x %*% object$coefficients
    return(lp)
  } else if (type == "risk") {
    lp <- predict(object, newdata = newdata, type = "lp")
    risk <- exp(lp)
    return(risk)
  } else {
    stop("type must be one of 'risk' or 'lp'")
  }
}
