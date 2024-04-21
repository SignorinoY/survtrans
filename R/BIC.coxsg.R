#' BIC for Subgroup analysis of Cox proportional hazards model
#' @param object a coxsg object.
#' @param data a data frame containing the variables in the model, which
#' should be the same as the data used to fit the model.
#' @param group a factor specifying the group of each sample, which
#' should be the same as the group used to fit the model.
#' @param ... Unused.
#' @return the BIC of the model.
#' @export
BIC.coxsg <- function(  # nolint: object_name_linter.
    object, data, group, ...) {
  loglik <- logLik(object, data, group)

  beta <- object$coefficients

  n_samples <- nrow(data)
  n_features <- nrow(beta)

  n_parameters <- 0
  for (j in 1:n_features) {
    nonzeros <- beta[j, beta[j, ] != 0]
    if (length(nonzeros) > 0) {
      n_parameters <- n_parameters + length(unique(nonzeros))
    }
  }

  return(-2 * loglik + n_parameters * log(n_samples))
}
