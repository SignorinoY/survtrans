#' @title BIC method for a \code{coxens} model
#' @param object a \code{coxens} object.
#' @param type A character string specifying the type of BIC to compute.
#' "traditional" corresponds to Cn=1, and "modified" corresponds to
#' Cn=log(log(d)).
#' @param ... Unused.
#' @return The BIC of the model.
#' @export
BIC.coxens <- function(object, type = c("traditional", "modified"), ...) {
  type <- match.arg(type)

  # Properties of the coxens object
  coefficients <- object$coefficients
  n_samples <- nrow(object$x)
  n_features <- nrow(coefficients)
  n_groups <- ncol(coefficients)

  # Log-likelihood of the model
  loglik <- logLik(object)

  # Calculate the BIC
  c_n <- ifelse(type == "traditional", 1, log(log(n_features * n_groups)))
  n_parameters <- sum(apply(coefficients, 1, function(coef_row) {
    length(unique(coef_row[coef_row != 0]))
  }))
  return(-2 * loglik + c_n * n_parameters * log(n_samples))
}
