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
BIC.coxtl <- function(
    object, data, group, type = c("trad", "mod"), ...) {
  type <- match.arg(type)

  coef <- object$coefficients
  n_samples <- nrow(data)

  loglik <- logLik(object, data, group)
  n_parameters <- sum(coef != 0)

  c <- ifelse(type == "trad", 1, log(log(length(coef))))
  return(-2 * loglik + c * n_parameters * log(n_samples))
}
