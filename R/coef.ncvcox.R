#' Extract coefficients from a ncvcox object
#' @param object a ncvcox object.
#' @param ... Unused.
#' @return a vector of coefficients.
#' @export
coef.ncvcox <- function(object, ...) {
  coefficients <- object$coefficients
  return(coefficients[coefficients != 0])
}
