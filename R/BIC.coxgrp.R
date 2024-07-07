#' BIC for Multiple-sources Cox proportional hazards model with group sparsity
#' @param object a coxsg object.
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
BIC.coxgrp <- function(
    object, data, group, type = c("trad", "mod"), ...) {
  type <- match.arg(type)

  coefficients <- object$coefficients
  n_samples <- nrow(data)
  n_features <- nrow(coefficients)
  n_groups <- ncol(coefficients)

  loglik <- logLik(object, data, group)
  n_parameters <- 0
  for (j in 1:n_features) {
    nonzeros <- coefficients[j, coefficients[j, ] != 0]
    if (length(nonzeros) > 0) {
      n_parameters <- n_parameters + length(unique(nonzeros))
    }
  }
  c <- ifelse(type == "trad", 1, log(log(n_features * n_groups)))
  return(-2 * loglik + c * n_parameters * log(n_samples))
}
