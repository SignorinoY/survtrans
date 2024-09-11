#' BIC for Non-convex penalized Cox proportional hazards model
#' @param object a ncvcox object.
#' @param type A character string specifying the type of BIC to compute.
#' "traditional" corresponds to Cn=1, and "modified" corresponds to
#' Cn=log(log(d)).
#' @param ... Unused.
#' @return the BIC of the model.
#' @export
BIC.ncvcox <- function(object, type = c("traditional", "modified"), ...) {
  type <- match.arg(type)

  # Properties of the coxens object
  coefficients <- object$coefficients
  n_samples <- nrow(object$x)
  n_features <- nrow(coefficients)
  n_parameters <- sum(coefficients != 0)

  loglik <- logLik(object)

  # Calculate the BIC
  c_n <- ifelse(type == "traditional", 1, log(log(n_features)))
  return(-2 * loglik + c_n * n_parameters * log(n_samples))
}
