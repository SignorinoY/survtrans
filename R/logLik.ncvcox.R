#' @title Log-likelihood for \code{ncvcox} model
#' @param object a \code{ncvcox} object.
#' @param ... Unused.
#' @return The log-likelihood of the model.
#' @export
logLik.ncvcox <- function(object, ...) {
  # Properties of the coxens object
  time <- object$time
  status <- object$status
  group <- object$group
  x <- object$x
  offset <- object$offset
  n_groups <- length(unique(group))
  group_levels <- levels(group)
  group_idxs <- lapply(group_levels, function(g) which(group == g))
  coefficients <- object$coefficients

  # Calculate the log-likelihood
  offset <- x %*% coefficients + offset
  hazard <- exp(offset - max(offset))
  risk_set <- ave(hazard, group, FUN = cumsum)
  risk_set <- unlist(lapply(seq_len(n_groups), function(k) {
    idx <- group_idxs[[k]]
    ave(risk_set[idx], time[idx], FUN = max)
  })) # Update the risk set for each group based on unique time points
  return(sum(status * (offset - log(risk_set) - max(offset))))
}
