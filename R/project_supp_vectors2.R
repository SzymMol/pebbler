project_supp_vectors2 <- function(scores_xy, supp_df) {
  # calculate supplementary variables projections for PCA
  supp <- scores_xy[FALSE, ]
  if (is.null(supp_df) || ncol(supp_df) == 0 || nrow(supp_df) == 0) return(supp)
  for (i in 1:length(supp_df)){
    for (j in 1:length(scores_xy)) {
      supp[i, j] = cor(scores_xy[, j], supp_df[, i], use = "complete.obs")
    }
  }
  rownames(supp) <- colnames(supp_df)
  return(supp)
}
