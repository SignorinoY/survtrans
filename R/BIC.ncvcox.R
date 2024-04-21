#' BIC for Non-convex penalized Cox proportional hazards model
#' @param object a ncvcox object.
#' @param data a data frame containing the variables in the model, which
#' should be the same as the data used to fit the model.
#' @param offset a numeric vector specifying the offset for each sample.
#' @param ... Unused.
#' @return the BIC of the model.
#' @export
BIC.ncvcox <- function( # nolint: object_name_linter.
    object, data, offset, ...) {
  loglik <- logLik(object, data, offset)

  coef <- object$coefficients

  n_samples <- nrow(data)
  n_parameters <- sum(coef != 0)

  return(-2 * loglik + n_parameters * log(n_samples))
}
