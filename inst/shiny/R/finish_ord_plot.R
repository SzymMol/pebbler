finish_ord_plot <- function(p, xlab, ylab, multi_id, facet_input, interactive = FALSE, flip_x = FALSE, flip_y = FALSE, group_name = NULL, id_name = NULL) {
  AXIS_FONT_SIZE <- 16

  if (isTRUE(facet_input) && multi_id) p <- p + facet_wrap(~dataset_id)

  p <- p + labs(x = xlab, y = ylab)#, fill = "Lithotype", shape = "ID")

  if (isTRUE(flip_x)) p <- p + scale_x_reverse()
  if (isTRUE(flip_y)) p <- p + scale_y_reverse()

  is_faceted <- isTRUE(facet_input) && isTRUE(multi_id)

  if (interactive && !is_faceted) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + guides(
      fill = guide_legend(title = group_name, override.aes = list(shape = 21, size = 5, color = "transparent")),
      # shape = guide_legend(title = "ID", override.aes = list(size = 5, fill = "grey30", color = "transparent")),
      shape = guide_legend(title = id_name, override.aes = list(size = 5, fill = "grey70")),
      color = "none", linetype = "none"
    )
  }

  p + pub_theme(18, axis_font_size = AXIS_FONT_SIZE)
}
