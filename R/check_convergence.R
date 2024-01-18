check_convergence <- function(coef, loss, last_record, control) {
  convergence <- last_record$convergence
  n_iterations <- last_record$n_iterations
  n_iterations_no_improvement <- last_record$n_iterations_no_improvement
  best_loss <- last_record$best_loss
  last_coef <- last_record$coef
  null_deviance <- last_record$null_deviance

  n_iterations <- n_iterations + 1

  if (n_iterations >= control$maxit) {
    message <- paste0(
      "Maximum number of iterations reached (", control$maxit, ")."
    )
    convergence <- TRUE
  }
  if (max(abs(coef - last_coef)) <= control$eps) {
    message <- paste0(
      "Convergence reached at iteration ", n_iterations, "."
    )
    convergence <- TRUE
  }

  if (loss >= best_loss) {
    n_iterations_no_improvement <- n_iterations_no_improvement + 1
  } else {
    best_loss <- loss
    n_iterations_no_improvement <- 0
  }

  if (n_iterations_no_improvement >= control$patience) {
    message <- paste0(
      "No improvement for ", n_iterations_no_improvement,
      " iterations. Stopping the algorithm."
    )
    convergence <- TRUE
  }

  if (-loss / null_deviance < 0.01) {
    message <- paste0(
      "The log-likelihood is too small (", loss / null_deviance,
      "). Stopping the algorithm."
    )
    convergence <- TRUE
  }

  if (control$verbose) {
    cat(
      "Iteration ", n_iterations, ": loss = ", loss,
      ifelse(n_iterations_no_improvement, "*", ""), "\n",
      sep = ""
    )
  }

  list(
    convergence = convergence, n_iterations = n_iterations,
    n_iterations_no_improvement = n_iterations_no_improvement,
    best_loss = best_loss, coef = coef,
    message = message, null_deviance = null_deviance
  )
}
