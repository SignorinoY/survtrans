#' Determine the maximum value of the penalty parameter lambda
#' @param formula a formula expression as for regression models, of the form
#' \code{response ~ predictors}. The response must be a survival object as
#' returned by the \code{\link{Surv}} function.
#' @param data a data frame containing the variables in the model.
#' @param group a factor specifying the group of each sample.
#' @param offset a numeric vector specifying the offset.
#' @return the maximum value of the penalty parameter lambda.
#' @export
calc_lambda_max <- function(formula, data, group, offset) {
  # Load the data
  data <- preprocess_data(formula, data, group, offset)
  x <- data$x
  time <- data$time
  status <- data$status
  group <- data$group
  offset <- data$offset

  # Properties of the data
  n_groups <- length(unique(group))
  group_levels <- levels(group)

  # Calculate the lambda_max
  lambda_max <- 0
  for (i in 1:n_groups) {
    idx <- which(group == group_levels[i])
    wls <- approx_likelihood(offset[idx], time[idx], status[idx])
    if (length(idx) > 1) {
      xwr <- colMeans(sweep(x[idx, ], 1, wls$residuals * wls$weights, `*`))
    } else {
      xwr <- 0
    }
    lambda_max <- max(lambda_max, max(abs(xwr), na.rm = TRUE))
  }
  return(lambda_max)
}
