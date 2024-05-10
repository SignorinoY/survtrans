#' BIC of Transfer Learning for Cox proportional hazards model
#' @param object a coxtl object.
#' @param data a data frame containing the variables in the model, which
#' should be the same as the data used to fit the model.
#' @param group a factor specifying the group of each sample, which
#' should be the same as the group used to fit the model.
#' @param type a character string specifying the type of BIC to compute.
#' The default is "trad", corresponding to Cn=1. The other option is
#' "mod", corresponding to Cn=log(log(d)).
#' @param ... Unused.
#' @return the BIC of the model.
#' @export
BIC.coxtl <- function( # nolint: object_name_linter.
    object, data, group, type = c("trad", "mod"), ...) {
  type <- match.arg(type)

  eta <- object$eta
  n_samples <- nrow(data)
  n_features <- nrow(eta)
  n_groups <- ncol(eta)

  loglik <- logLik(object, data, group)
  n_parameters <- n_features
  for (j in 1:n_features) {
    nonzeros <- eta[j, eta[j, ] != 0]
    if (length(nonzeros) > 0) {
      n_parameters <- n_parameters + length(unique(nonzeros))
    }
  }
  c <- ifelse(type == "trad", 1, log(log(n_features * n_groups)))
  return(-2 * loglik + n_parameters * log(n_samples))
}
