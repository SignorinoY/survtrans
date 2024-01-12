check_convergence <- function(coef, loss, last_record, control) {
  convergence <- last_record$convergence
  n_iterations <- last_record$n_iterations
  n_iterations_no_improvement <- last_record$n_iterations_no_improvement
  best_loss <- last_record$best_loss
  best_coef <- last_record$best_coef
  last_coef <- last_record$coef

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
    best_coef <- coef
    n_iterations_no_improvement <- 0
  }

  if (n_iterations_no_improvement >= control$patience) {
    message <- paste0(
      "No improvement for ", n_iterations_no_improvement,
      " iterations. Stopping the algorithm."
    )
    convergence <- TRUE
  }

  list(
    convergence = convergence, n_iterations = n_iterations + 1,
    n_iterations_no_improvement = n_iterations_no_improvement,
    best_loss = best_loss, best_coef = best_coef, coef = coef,
    message = message
  )
}
