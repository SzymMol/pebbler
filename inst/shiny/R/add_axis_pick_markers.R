add_axis_pick_markers <- function(pl, xlab_plain, ylab_plain, source_id) {
  ax <- get_virtual_axis_df(xlab_plain, ylab_plain)

  marker_df <- data.frame(
    uid = ax$uid,
    display_var = ax$display_var,
    x = c(0.85, -0.85, 0, 0),
    y = c(0, 0, 0.85, -0.85),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(marker_df))) {
    rw <- marker_df[i, , drop = FALSE]
    pl <- add_trace(
      pl,
      type = "scatter",
      mode = "markers",
      x = rw$x, y = rw$y,
      marker = list(size = 14, color = "rgba(0,0,0,0)", line = list(color = "rgba(0,0,0,0)", width = 0)),
      key = rw$uid,
      customdata = rw$uid,
      hoverinfo = "text",
      text = paste0("Axis selector: ", rw$display_var),
      showlegend = FALSE,
      inherit = FALSE
      # source = source_id
    )
  }

  pl
}
