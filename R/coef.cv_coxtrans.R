#' @export
coef.cv_coxtrans <- function(object, s = "lambda.min", ...) {
  if (s == "lambda.min") {
    coef <- object$coefs[which.max(object$cvm), , drop = FALSE]
    coef <- matrix(coef, nrow = object$n_features)
    return(coef)
  } else if (s == "lambda.1se") {
    cvm <- object$cvm
    cvsd <- object$cvsd
    coef <- object$coefs[which.max(cvm + cvsd), , drop = FALSE]
    coef <- matrix(coef, nrow = object$n_features)
    return(coef)
  } else {
    stop("s must be one of 'lambda.min' or 'lambda.1se'")
  }
}
