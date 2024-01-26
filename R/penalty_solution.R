penalty_solution <- function(z, v, penalty, lambda, gamma) {
  if (penalty == "lasso") {
    eta <- soft_threshold(z, lambda) / v
  } else if (penalty == "MCP") {
    eta <- ifelse(
      abs(z) <= gamma * lambda * v,
      soft_threshold(z, lambda) / (v - 1 / gamma),
      z / v
    )
  } else if (penalty == "SCAD") {
    if (abs(z) <= (v + 1) * lambda) {
      eta <- soft_threshold(z, lambda) / v
    } else if (abs(z) <= gamma * lambda * v) {
      eta <- soft_threshold(z, gamma * lambda / (gamma - 1))
      eta <- eta / (v - 1 / (gamma - 1))
    } else {
      eta <- z / v
    }
  }
  return(eta)
}
