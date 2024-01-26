soft_threshold <- function(x, lambda) {
  threshold <- abs(x) - lambda
  ifelse(threshold < 0, 0, threshold * sign(x))
}
