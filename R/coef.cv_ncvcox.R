#' @export
coef.cv_ncvcox <- function(object, s = "lambda.min", ...) {
  if (s == "lambda.min") {
    return(object$coefs[which.max(object$cvm), , drop = FALSE])
  } else if (s == "lambda.1se") {
    cvm <- object$cvm
    cvsd <- object$cvsd
    lambda.1se <- object$lambdas[which.max(cvm + cvsd)]
    return(object$coefs[which.min(abs(object$lambdas - lambda.1se)), , drop = FALSE])
  } else {
    stop("s must be one of 'lambda.min' or 'lambda.1se'")
  }
}