preprocess_data <- function(formula, data, group, offset) {
  mf <- model.frame(formula, data)

  # Load time and status from formula and data
  y <- model.response(mf)
  time <- y[, 1]
  status <- y[, 2]

  # Load X from formula and data
  x <- model.matrix(formula, data)
  x <- x[, -1] # Remove the intercept column

  # Properties of the data
  n_samples <- nrow(x)

  # Standardize the covariates
  x <- scale(x)
  x_center <- attr(x, "scaled:center")
  x_scale <- attr(x, "scaled:scale")

  # Check the offset argument
  if (missing(offset)) offset <- rep(0.0, n_samples)

  # Check the group argument
  if (missing(group)) group <- rep(0, n_samples)
  if (!is.factor(group)) group <- factor(group)

  # Sort the data by time
  sorted <- order(time, decreasing = TRUE)
  time <- time[sorted]
  status <- status[sorted]
  x <- x[sorted, , drop = FALSE]
  attr(x, "center") <- x_center
  attr(x, "scale") <- x_scale

  offset <- offset[sorted]
  group <- group[sorted]

  list(
    x = x, time = time, status = status, group = group, offset = offset
  )
}
