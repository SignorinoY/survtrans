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
  n_groups <- ncol(coefficients) - 1

  # Number of active constraints
  eta_group <-  matrix(0, n_features, n_groups)
  for (j in seq_len(n_features)) {
    feature_values <- coefficients[j, 1:n_groups]
    feature_levels <- unique(feature_values)
    for (k in seq_along(feature_levels)) {
      eta_group[j, feature_values == feature_levels[k]] <- k
    }
  }
  n_active_constraints <- sum(apply(eta_group, 1, function(row) {
    length(unique(row)) > 1
  }))

  # The number of parameters should minus the number of active constraints
  n_parameters <- sum(apply(coefficients, 1, function(coef_row) {
    length(unique(coef_row[coef_row != 0]))
  })) - n_active_constraints

  # Log-likelihood of the model
  loglik <- logLik(object)

  # The parameter of the BIC
  c_n <- ifelse(type == "traditional", 1, log(log(n_features * n_groups)))

  return(-2 * loglik + c_n * n_parameters * log(n_samples))
}
