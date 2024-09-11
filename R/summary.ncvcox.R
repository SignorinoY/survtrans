#' @title Summary method for a \code{ncvcox} model
#' @param object the result of a \code{ncvcox} fit.
#' @param conf.int a numeric value between 0 and 1 indicating the confidence
#' level of the confidence interval. Default is 0.95.
#' @param compressed logical; if \code{TRUE}, the summary is compressed and
#' only includes the non-zero coefficients. Default is \code{TRUE}.
#' @param ... Unused.
#' @return A object of class \code{summary.ncvcox}, with the following
#' components:
#' \item{\code{n}, \code{nevent}}{number of observations and number of events,
#' respectively, in the fit}
#' \item{\code{logLik}}{the log partial likelihood at the final value}
#' \item{\code{BIC}}{the Bayesian Information Criterion at the final value}
#' \item{\code{coefficients}}{a matrix with one row for each coefficient, and
#' columns containing the coefficient, the hazard ratio exp(coef), standard
#' error, Wald statistic, and P value}
#' \item{conf.int}{a matrix with one row for each coefficient, containing the
#' confidence limits for exp(coef)}
#' @export
summary.ncvcox <- function(object, conf.int = 0.95, compressed = TRUE, ...) {
  # Extract necessary components from the object
  n_samples <- nrow(object$x)
  n_events <- sum(object$status)
  loglik <- logLik(object)
  bic_value <- BIC(object)
  variable_names <- colnames(object$x)

  # Standard errors
  vcov_matrix <- vcov(object)
  if (is.null(vcov_matrix)) {
    stop("Variance-covariance matrix is not available.")
  }
  se <- sqrt(diag(vcov_matrix))

  coefficients <- coef(object)
  z_scores <- coefficients / se
  p_values <- pchisq(z_scores^2, 1, lower.tail = FALSE)
  coef_matrix <- cbind(
    coefficients, exp(coefficients), se, z_scores, p_values
  )
  dimnames(coef_matrix) <- list(
    names(coefficients), c("coef", "exp(coef)", "se(coef)", "z", "Pr(>|z|)")
  )

  z <- qnorm((1 + conf.int) / 2)
  conf_int_matrix <- cbind(
    exp(coefficients), exp(-coefficients),
    exp(coefficients - z * se), exp(coefficients + z * se)
  )
  dimnames(conf_int_matrix) <- list(
    names(coefficients), c(
      "exp(coef)", "exp(-coef)",
      paste("lower .", round(100 * conf.int, 2), sep = ""),
      paste("upper .", round(100 * conf.int, 2), sep = "")
    )
  )

  if (!compressed) {
    coef_matrix_extract <- matrix(nrow = 0, ncol = ncol(coef_matrix))
    conf_int_matrix_extract <- matrix(nrow = 0, ncol = ncol(conf_int_matrix))
    nonzero_names <- rownames(coef_matrix)
    for (name in variable_names) {
      if (name %in% nonzero_names) {
        coef_matrix_extract <- rbind(coef_matrix_extract, coef_matrix[name, ])
        conf_int_matrix_extract <- rbind(
          conf_int_matrix_extract, conf_int_matrix[name, ]
        )
      } else {
        coef_matrix_extract <- rbind(coef_matrix_extract, c(0, 1, NA, NA, NA))
        conf_int_matrix_extract <- rbind(
          conf_int_matrix_extract, c(1, 1, NA, NA)
        )
      }
    }
    print(coef_matrix_extract)
    coef_matrix <- coef_matrix_extract
    conf_int_matrix <- conf_int_matrix_extract
    rownames(coef_matrix) <- variable_names
    rownames(conf_int_matrix) <- variable_names
  }

  # Create the summary object
  summary_object <- list(
    n = n_samples,
    nevent = n_events,
    logLik = loglik,
    call = object$call,
    BIC = bic_value,
    coefficients = coef_matrix,
    conf.int = conf_int_matrix
  )
  class(summary_object) <- "summary.ncvcox"
  return(summary_object)
}
