axis_percent_from_pca_eigen <- function(eigen_df) {
  if (is.null(eigen_df) || nrow(eigen_df) == 0) return(data.frame())
  if ("dataset_id" %in% colnames(eigen_df)) {
    eigen_df %>%
      mutate(
        axis_index = suppressWarnings(as.integer(gsub("^PC", "", PC))),
        pct = explained_var_pct
      ) %>%
      select(dataset_id, axis_index, pct)
  }
  else {
    eigen_df %>%
      mutate(
        axis_index = suppressWarnings(as.integer(gsub("^PC", "", PC))),
        pct = explained_var_pct
      ) %>%
      select(axis_index, pct)
  }

}
