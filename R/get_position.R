get_position <- function(i_vec, j_vec, s) { # nolint: cyclocomp_linter.
  n_search <- length(i_vec)
  pos <- numeric(n_search)
  for (k in seq_len(n_search)) {
    i <- i_vec[k]
    j <- j_vec[k]
    if (is.na(i) || is.na(j) || i > s || j > s || i == j) {
      next
    } else if (i > j) {
      pos[k] <- get_position(j, i, s)
    } else {
      if (i == 1) {
        pos[k] <- j - 1
      } else {
        pos[k] <- sum(s - seq_len(i - 1)) + (j - i)
      }
    }
  }
  pos <- pos[pos != 0]
  return(pos)
}
