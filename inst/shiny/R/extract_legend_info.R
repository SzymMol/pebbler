extract_legend_info <- function(p, scores_df, group_colname, multi_id) {
  FILLABLE_SHAPES <- c(21, 22, 23, 24, 25)

  litho_levels <- if (!is.null(group_colname) && group_colname %in% names(scores_df)) levels(factor(scores_df[[group_colname]])) else character(0)
  litho_colors <- NULL
  if (length(litho_levels) > 0) litho_colors <- get_ggplot_fill_colors(p)
  if (length(litho_levels) > 0 && is.null(litho_colors)) litho_colors <- get_default_group_colors(litho_levels)

  id_levels <- NULL
  id_shapes <- NULL
  if (multi_id) {
    id_levels <- levels(factor(scores_df$dataset_id))
    n_ids <- length(id_levels)
    shape_nums <- rep(FILLABLE_SHAPES, length.out = n_ids)
    id_shapes <- setNames(sapply(shape_nums, ggshape_to_plotly), id_levels)
  }

  list(litho_levels = litho_levels, litho_colors = litho_colors, id_levels = id_levels, id_shapes = id_shapes)
}
