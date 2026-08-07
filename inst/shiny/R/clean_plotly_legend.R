clean_plotly_legend <- function(pl, group_colname = "Lithotype", id_colname = "ID",
                                litho_levels = NULL, litho_colors = NULL,
                                id_levels = NULL, id_shapes = NULL, is_faceted = FALSE) {
  if (isTRUE(is_faceted)) return(pl)

  b <- plotly_build(pl)
  for (i in seq_along(b$x$data)) b$x$data[[i]]$showlegend <- FALSE
  b$x$layout$legend$title$text <- ""

  if (!is.null(litho_levels) && length(litho_levels) > 0) {
    for (j in seq_along(litho_levels)) {
      lname <- litho_levels[j]
      col <- if (!is.null(litho_colors) && lname %in% names(litho_colors)) litho_colors[[lname]] else "grey50"
      b$x$data <- c(b$x$data, list(list(
        type = "scatter", mode = "markers", x = list(NULL), y = list(NULL),
        marker = list(symbol = "circle", size = 10, color = col, line = list(color = "black", width = 0.5)),
        name = lname, legendgroup = "lithotypes",
        legendgrouptitle = list(text = if (j == 1) group_colname else ""),
        showlegend = TRUE, hoverinfo = "skip"
      )))
    }
  }

  if (!is.null(id_levels) && length(id_levels) > 0) {
    default_shapes <- c("circle", "square", "diamond", "triangle-up", "triangle-down")
    for (j in seq_along(id_levels)) {
      idname <- id_levels[j]
      sym <- if (!is.null(id_shapes) && idname %in% names(id_shapes)) id_shapes[[idname]] else default_shapes[((j - 1) %% length(default_shapes)) + 1]
      b$x$data <- c(b$x$data, list(list(
        type = "scatter", mode = "markers", x = list(NULL), y = list(NULL),
        marker = list(symbol = sym, size = 10, color = "grey50", line = list(color = "black", width = 1.5)),
        name = idname, legendgroup = "ids",
        legendgrouptitle = list(text = if (j == 1) id_colname else ""),
        showlegend = TRUE, hoverinfo = "skip"
      )))
    }
  }

  layout(
    b,
    legend = list(itemsizing = "constant", tracegroupgap = 15, traceorder = "grouped", font = list(size = 13))
  )
}
