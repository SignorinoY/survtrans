penalty_grad <- function(params, penalty, lambda, gamma) {
  params.abs <- abs(params)
  if (penalty == "lasso") {
    grads <- ifelse(params.abs <= lambda, lambda, 0)
  } else if (penalty == "SCAD") {
    grads <- ifelse(
      params.abs <= lambda,
      lambda,
      ifelse(
        params.abs <= gamma * lambda,
        (gamma * lambda - params) / (gamma - 1),
        0
      )
    )
  }
  return(grads)
}
