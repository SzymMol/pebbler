#' Project supplementary vectors
#'
#' Calculate supplementary variables projections using Pearson correlation.
#'
#'
#' @param scores_xy data frame of PCA scores from \code{\link{prcomp}}
#' @param supp_df data frame with supplementary variables observations
#' @returns data frame with supplementary vectors components
#' @examples
#' data('witow', package = 'pebbler')
#' dat <- witow
#' roles <- list(active = c('a', 'b', 'c', 'mass'),
#'               group = 'lithotype',
#'               supp = 'hgt_min')
#' x_active <- scale(dat[roles$active])
#' x_supp <- dat[roles$supp]
#' fit <- prcomp(x_active, center = TRUE, scale. = FALSE) # PCA calculation
#' s <- as.data.frame(fit$x) # scores
#' pebbler:::project_supp_vectors2(s, x_supp) # supplementary vectors
#' @importFrom stats cor

project_supp_vectors2 <- function(scores_xy, supp_df) {
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
