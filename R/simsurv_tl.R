#' Simulate survival data of Multi-source Cox model
#' @param beta the common coefficients, a vector of length p,
#' where p is the number of features
#' @param eta the group-specific coefficients, a matrix of size p x K,
#' where K is the number of groups
#' @param lambda the baseline hazard's scale parameters, a vector of length K
#' @param gamma the baseline hazard's shape parameters, a vector of length K
#' @param dist the distribution of the baseline hazard, a string, either
#' "exponential","weibull", or "gompertz"
#' @param maxt the maximum time to simulate, a positive number
#' @param n_samples a vector of length K specifying the number of samples or a
#' single number specifying the total number of samples
#' @param seed the random seed, the default is 0
#' @return a data frame with columns "id", "group", "X1", "X2", ..., "Xp",
#' "time", "status"
#' @export
#' @examples
#' beta <- c(1, 1)
#' eta <- matrix(c(0, 0, 1, 1), nrow = 2, ncol = 2)
#' lambda <- c(1, 2)
#' gamma <- c(2, 1)
#' dist <- c("gompertz", "weibull")
#' maxt <- 3
#' n_samples <- 100
#' df <- simsurv_tl(beta, eta, lambda, gamma, dist, maxt, n_samples)
#' head(df)
simsurv_tl <- function(
    beta, eta, lambda, gamma, dist, maxt, n_samples, seed = 0) {
  set.seed(seed)
  n_groups <- ncol(eta)
  n_features <- nrow(eta)
  names(beta) <- paste0("X", 1:n_features)
  if (length(n_samples) == 1) n_samples <- rep(n_samples, n_groups)
  mu <- rep(0, n_features)
  sigma <- diag(n_features)
  covs <- c()
  times <- c()
  for (k in 1:n_groups) {
    cov <- data.frame(
      id = seq_len(n_samples[k]), mvrnorm(n_samples[k], mu = mu, Sigma = sigma)
    )
    time <- simsurv(
      lambdas = lambda[k], gammas = gamma[k], dist = dist[k],
      x = cov, betas = beta + eta[, k], maxt = maxt
    )
    cov$group <- k
    time$group <- k
    covs <- rbind(covs, cov)
    times <- rbind(times, time)
  }
  df <- left_join(covs, times, by = c("id", "group")) %>%
    rename(time = "eventtime") %>%
    mutate(id = 1:(n_samples * n_groups))
  return(df)
}
