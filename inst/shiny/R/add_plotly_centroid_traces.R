add_plotly_centroid_traces <- function(pl, centroids_df, source_id, group_colors = NULL, id_shapes = NULL, ref_uid = NULL, hover_uid = NULL, base_size = 13) {
  if (is.null(centroids_df) || nrow(centroids_df) == 0) return(pl)
  pair_metrics <- compute_centroid_pair_metrics(centroids_df, ref_uid, hover_uid)
  disp <- compute_centroid_displacements(centroids_df, ref_uid)

  ref_group <- NULL
  if (!is.null(ref_uid) && nzchar(ref_uid)) {
    rr <- centroids_df %>% filter(uid == ref_uid) %>% slice(1)
    if (nrow(rr) > 0) ref_group <- as.character(rr$group)
  }

  for (i in seq_len(nrow(centroids_df))) {
    rw <- centroids_df[i, , drop = FALSE]
    grp <- as.character(rw$group)
    ds <- as.character(rw$dataset_id)
    base_col <- if (!is.null(group_colors) && grp %in% names(group_colors)) group_colors[[grp]] else "grey40"
    base_symbol <- if (!is.null(id_shapes) && nzchar(ds) && ds %in% names(id_shapes)) id_shapes[[ds]] else "circle"
    is_ref <- !is.null(ref_uid) && nzchar(ref_uid) && rw$uid == ref_uid
    is_hov <- !is.null(hover_uid) && nzchar(hover_uid) && rw$uid == hover_uid
    has_ref <- !is.null(ref_uid) && nzchar(ref_uid)
    same_group <- !is.null(ref_group) && nzchar(ref_group) && identical(grp, ref_group)
    outline_group <- has_ref && (is_ref || same_group)
    outline_symbol <- if (nzchar(base_symbol)) paste0(base_symbol, "-open") else "circle-open"
    base_opacity <- if (has_ref && !is_ref && !is_hov && !same_group) 0.30 else if (has_ref && same_group) 0.95 else 1
    hover_txt <- paste0(
      "Centroid: ", rw$label,
      "<br>Group: ", grp,
      if (nzchar(ds)) paste0("<br>ID: ", ds) else "",
      "<br>x: ", signif(rw$cx, 4),
      "<br>y: ", signif(rw$cy, 4),
      if (!is.null(pair_metrics) && (rw$uid == pair_metrics$ref_uid || rw$uid == pair_metrics$hov_uid)) paste0(
        "<br>dx: ", sprintf("%.4f", pair_metrics$dx),
        "<br>dy: ", sprintf("%.4f", pair_metrics$dy),
        "<br>d: ", sprintf("%.4f", pair_metrics$dist)
      ) else ""
    )

    pl <- add_trace(
      pl,
      type = "scatter",
      mode = "markers",
      x = rw$cx,
      y = rw$cy,
      color = I(base_col),
      marker = list(
        symbol = base_symbol,
        size = base_size,
        color = base_col,
        opacity = base_opacity,
        line = list(color = "black", width = 1.1)
      ),
      key = rw$uid,
      customdata = rw$uid,
      text = hover_txt,
      hoverinfo = "text",
      showlegend = FALSE,
      inherit = FALSE
      # source = source_id
    )

    if (outline_group) {
      pl <- add_trace(
        pl,
        type = "scatter",
        mode = "markers",
        x = rw$cx,
        y = rw$cy,
        marker = list(
          symbol = outline_symbol,
          size = base_size + 8,
          color = "black",
          line = list(width = 4.8)
        ),
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
        # source = source_id
      )
    }
  }

  if (nrow(disp) > 0) {
    for (i in seq_len(nrow(disp))) {
      rw <- disp[i, , drop = FALSE]
      tgt_group <- as.character(rw$group)
      tgt_col <- if (!is.null(group_colors) && tgt_group %in% names(group_colors)) group_colors[[tgt_group]] else "rgba(35,35,35,0.95)"
      pl <- add_annotations(
        pl,
        x = rw$x2, y = rw$y2,
        ax = rw$x1, ay = rw$y1,
        xref = "x", yref = "y", axref = "x", ayref = "y",
        text = "",
        showarrow = TRUE,
        arrowhead = 3,
        arrowsize = 1.2,
        arrowwidth = 2.2,
        arrowcolor = tgt_col,
        opacity = 0.95
      )
    }
  }

  if (!is.null(pair_metrics)) {
    pl <- add_annotations(
      pl,
      x = pair_metrics$x2, y = pair_metrics$y2,
      ax = pair_metrics$x1, ay = pair_metrics$y1,
      xref = "x", yref = "y", axref = "x", ayref = "y",
      text = "",
      showarrow = TRUE,
      arrowhead = 0,
      arrowsize = 1,
      arrowwidth = 3.2,
      arrowcolor = "rgba(0, 0, 0, 0.88)",
      opacity = 1
    )

    pl <- add_annotations(
      pl,
      x = pair_metrics$mx,
      y = pair_metrics$my,
      xref = "x",
      yref = "y",
      text = paste0("<b>d = ", sprintf("%.2f", pair_metrics$dist), "</b>"),
      showarrow = FALSE,
      font = list(color = "black", size = 24),
      xanchor = "center",
      yanchor = "middle",
      opacity = 1
    )
  }

  pl
}
