get_position <- function(i_vec, j_vec, s) { # nolint: cyclocomp_linter.
  if (length(i_vec) != length(j_vec)) {
    stop("The lengths of i and j must be the same")
  }

  n_search <- length(i_vec)
  pos <- numeric(n_search)
  for (k in seq_len(n_search)) {
    i <- i_vec[k]
    j <- j_vec[k]
    if (is.na(i) || is.na(j)) {
      pos[k] <- NA
    }
    if (i >= j || i > s || j > s) {
      pos[k] <- NA
    } else {
      if (i == 1) {
        pos[k] <- j - 1
      } else {
        pos[k] <- sum(s - seq_len(i - 1)) + (j - i)
      }
    }
  }
  return(pos)
}
