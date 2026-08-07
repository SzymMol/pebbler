compute_structure_coefficients <- function(x_mat, scores_mat) {
  p <- ncol(x_mat)
  k <- ncol(scores_mat)
  sc <- matrix(NA, nrow = p, ncol = k)
  for (j in seq_len(p)) for (d in seq_len(k)) {
    sc[j, d] <- suppressWarnings(cor(x_mat[, j], scores_mat[, d], use = "complete.obs"))
  }
  sc[!is.finite(sc)] <- 0
  sc
}
