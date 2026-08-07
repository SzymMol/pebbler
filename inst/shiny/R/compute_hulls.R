compute_hulls <- function(scores_df, group_col, x_col, y_col, id_col = NULL) {
  if (is.null(group_col) || !(group_col %in% names(scores_df))) {
    return(scores_df[0, , drop = FALSE])
  }

  grps <- if (is.null(id_col)) group_col else c(group_col, id_col)
  h <- scores_df %>%
    group_by(across(all_of(grps))) %>%
    filter(dplyr::n_distinct(.data[[x_col]], .data[[y_col]]) >= 3) %>%
    slice(chull(.data[[x_col]], .data[[y_col]])) %>%
    ungroup()
  if (nrow(h) == 0) return(h)
  h %>%
    group_by(across(all_of(grps))) %>%
    group_modify(~ bind_rows(.x, .x[1, , drop = FALSE])) %>%
    ungroup()
}
