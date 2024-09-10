#' @title BIC method for a \code{coxens} model
#' @param object the result of a \code{coxens} fit.
#' @param ... Unused.
#' @return The log-likelihood of the model.
#' @export
logLik.coxens <- function(object, ...) {
  # Properties of the coxens object
  time <- object$time
  status <- object$status
  group <- object$group
  x <- object$x
  n_groups <- length(unique(group))
  group_levels <- levels(group)
  group_idxs <- lapply(group_levels, function(g) which(group == g))
  coefficients <- object$coefficients

  beta <- coefficients[, 1:n_groups] + coefficients[, (n_groups + 1)]

  # Calculate the log-likelihood
  offset <- numeric(nrow(x))
  for (k in seq_len(n_groups)) {
    idx <- group_idxs[[k]]
    offset[idx] <- x[idx, ] %*% beta[, k]
  }
  hazard <- exp(offset - max(offset))
  risk_set <- ave(hazard, group, FUN = cumsum)
  risk_set <- unlist(lapply(seq_len(n_groups), function(k) {
    idx <- group_idxs[[k]]
    ave(risk_set[idx], time[idx], FUN = max)
  })) # Update the risk set for each group based on unique time points
  return(sum(status * (offset - log(risk_set) - max(offset))))
}
