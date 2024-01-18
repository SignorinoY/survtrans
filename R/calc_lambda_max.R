calc_lambda_max <- function(formula, data, group, offset) {
  if (missing(group)) {
    group <- rep(1, nrow(data))
  }
  if (missing(offset)) {
    offset <- rep(0, nrow(data))
  }
  # Load the data
  data <- preprocess_data(formula, data, group = group)
  x <- data$x
  time <- data$time
  status <- data$status
  group <- data$group

  # Properties of the data
  n_groups <- length(unique(group))
  group_levels <- levels(group)

  # Calculate the lambda_max
  lambda_max <- 0
  for (i in 1:n_groups) {
    idx <- which(group == group_levels[i])
    wls <- calc_weights_residuals(offset[idx], time[idx], status[idx])
    xwr <- colMeans(sweep(x[idx, ], 1, wls$residuals * wls$weights, `*`))
    lambda_max <- max(lambda_max, max(abs(xwr)))
  }
  return(lambda_max)
}
