#' Extract coefficients from a cv_ncvcox object
#' @param object a cv_ncvcox object.
#' @param s a character string specifying the rule to select the coefficients.
#' The default is "lambda.min". Other options are "lambda.1se".
#' @param ... Unused.
#' @return a vector of coefficients.
#' @export
coef.cv_ncvcox <- function(object, s = "lambda.min", ...) {
  if (!s %in% c("lambda.min", "lambda.1se")) {
    stop("s must be one of 'lambda.min' or 'lambda.1se'")
  }
  lambda_opt <- switch(s,
    "lambda.min" = object$lambda.min,
    "lambda.1se" = object$lambda.1se
  )
  coefficients <- object$coefficients[which(object$lambdas == lambda_opt), ]
  names(coefficients) <- colnames(object$coefficients)
  return(coefficients)
}
