make_axis_label_with_pct <- function(axis_prefix, axis_index, pct_df = NULL, split_mode = FALSE,
                                     facet_by_id = FALSE, digits = 1) {
  base_lab <- paste0(axis_prefix, axis_index)

  if (is.null(pct_df) || nrow(pct_df) == 0) return(base_lab)

  this_ax <- pct_df %>% filter(axis_index == !!axis_index)
  if (nrow(this_ax) == 0) return(base_lab)

  if (!isTRUE(split_mode)) {
    return(paste0(base_lab, " (", format_axis_pct_value(this_ax$pct[1], digits), ")"))
  }

  ds_ids <- unique(as.character(this_ax$dataset_id))

  if (length(ds_ids) == 1) {
    return(paste0(base_lab, " (", format_axis_pct_value(this_ax$pct[1], digits), ")"))
  }

  if (isTRUE(facet_by_id)) {
    detail <- paste0(
      as.character(this_ax$dataset_id),
      ": ",
      format_axis_pct_value(this_ax$pct, digits),
      collapse = "\n"
    )
    return(paste0(base_lab, "\n", detail))
  }

  detail <- paste0(
    as.character(this_ax$dataset_id),
    ": ",
    format_axis_pct_value(this_ax$pct, digits),
    collapse = "; "
  )
  paste0(base_lab, " (", detail, ")")
}
