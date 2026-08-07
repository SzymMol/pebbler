add_id_suffix_to_vec_labels <- function(df, id_col = "dataset_id", var_col = "var") {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (!(id_col %in% names(df)) || !(var_col %in% names(df))) return(df)
  df %>%
    mutate(
      !!var_col := ifelse(
        is.na(.data[[id_col]]) | .data[[id_col]] == "",
        .data[[var_col]],
        paste0(.data[[var_col]], "_", as.character(.data[[id_col]]))
      )
    )
}
