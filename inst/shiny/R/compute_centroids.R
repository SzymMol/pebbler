compute_centroids <- function(scores_df, group_col, x_col, y_col, id_col = NULL) {
  if (is.null(group_col) || !(group_col %in% names(scores_df))) {
    if (is.null(id_col)) {
      return(data.frame(group = character(), cx = numeric(), cy = numeric()))
    } else {
      return(data.frame(group = character(), dataset_id = character(), cx = numeric(), cy = numeric()))
    }
  }

  if (is.null(id_col)) {
    scores_df %>%
      group_by(.data[[group_col]]) %>%
      summarise(
        cx = mean(.data[[x_col]], na.rm = TRUE),
        cy = mean(.data[[y_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(group = 1)
  } else {
    scores_df %>%
      group_by(.data[[group_col]], .data[[id_col]]) %>%
      summarise(
        cx = mean(.data[[x_col]], na.rm = TRUE),
        cy = mean(.data[[y_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(group = 1, dataset_id = 2)
  }
}
