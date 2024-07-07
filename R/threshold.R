threshold <- function(y, vartheta, penalty, lambda, gamma) {
  if (penalty == "lasso") {
    x <- soft_threshold(y, lambda / vartheta)
  } else if (penalty == "MCP") {
    const <- 1 - 1 / (gamma * vartheta - 1)
    x <- ifelse(
      abs(y) <= lambda * gamma,
      soft_threshold(y, lambda / vartheta) * const,
      y
    )
  } else if (penalty == "SCAD") {
    const <- vartheta * (gamma - 1)
    if (abs(y) <= lambda + lambda / vartheta) {
      x <- soft_threshold(y, lambda / vartheta)
    } else if (abs(y) <= lambda * gamma) {
      x <- soft_threshold(y, lambda * gamma / const) * const / (const - 1)
    } else {
      x <- y
    }
  }
  return(x)
}
