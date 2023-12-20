#' @export
coef.cv_ncvcox <- function(object, s = "lambda.min", ...) {
  if (s == "lambda.min") {
    return(object$coefs[which.max(object$cvm), , drop = FALSE])
  } else if (s == "lambda.1se") {
    cvm <- object$cvm
    cvsd <- object$cvsd
    return(object$coefs[which.max(cvm + cvsd), , drop = FALSE])
  } else {
    stop("s must be one of 'lambda.min' or 'lambda.1se'")
  }
}
