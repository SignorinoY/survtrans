soft_threshold <- function(z, v, penalty, lambda, gamma) {
  st <- function(x, lambda) sign(x) * pmax(abs(x) - lambda, 0)
  if (penalty == "lasso") {
    eta <- st(z, lambda) / v
  } else if (penalty == "MCP") {
    eta <- ifelse(
      abs(z) <= gamma * lambda * v,
      st(z, lambda) / (v - 1 / gamma),
      z / v
    )
  } else if (penalty == "SCAD") {
    if (abs(z) <= (v + 1) * lambda) {
      eta <- st(z, lambda) / v
    } else if (abs(z) <= gamma * lambda * v) {
      eta <- st(z, gamma * lambda / (gamma - 1)) / (v - 1 / (gamma - 1))
    } else {
      eta <- z / v
    }
  }
  return(eta)
}
