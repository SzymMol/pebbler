project_supp_vectors <- function(scores_xy, supp_df) {
  empty <- data.frame(var = character(), x = numeric(), y = numeric())
  if (is.null(supp_df) || ncol(supp_df) == 0 || nrow(supp_df) == 0) return(empty)

  out <- bind_rows(lapply(names(supp_df), function(v) {
    xv <- supp_df[[v]]
    if (!is.numeric(xv)) return(NULL)
    data.frame(
      var = v,
      x = suppressWarnings(cor(scores_xy[, 1], xv, use = "complete.obs")),
      y = suppressWarnings(cor(scores_xy[, 2], xv, use = "complete.obs"))
    )
  }))

  if (is.null(out) || nrow(out) == 0) return(empty)
  out %>% filter(is.finite(x), is.finite(y))
}
