check_convergence <- function(coef, loss, last_record) {
  convergence <- last_record$convergence
  n_iterations <- last_record$n_iterations
  last_coef <- last_record$coef
  null_deviance <- last_record$null_deviance

  eps <- last_record$eps
  maxit <- last_record$maxit

  if (n_iterations >= maxit) {
    last_record$message <- paste0(
      "Maximum number of iterations reached (", maxit, ")."
    )
    convergence <- TRUE
  }
  if (n_iterations > 1 && max(abs(coef - last_coef)) <= eps) {
    last_record$message <- paste0(
      "Convergence reached at iteration ", n_iterations, "."
    )
    convergence <- TRUE
  }

  if (-loss / null_deviance < 0.01) {
    last_record$message <- paste0(
      "The log-likelihood is too small (", loss / null_deviance,
      "). Stopping the algorithm."
    )
    convergence <- TRUE
  }

  last_record$coef <- coef
  last_record$convergence <- convergence
  last_record$n_iterations <- n_iterations + 1
  last_record
}
