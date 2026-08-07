add_plotly_vector_traces <- function(pl, vectors_df, source_id, ref_uid = NULL, hover_uid = NULL,
                                     xlab_plain = NULL, ylab_plain = NULL) {
  if (is.null(vectors_df) || nrow(vectors_df) == 0) return(pl)

  all_df <- merge_vectors_with_axes(vectors_df, xlab_plain, ylab_plain)
  pair_metrics <- compute_pair_metrics(all_df, ref_uid, hover_uid)

  for (i in seq_len(nrow(vectors_df))) {
    rw <- vectors_df[i, , drop = FALSE]
    base_col <- if (rw$type == "supp") "red3" else "black"
    base_width <- if (rw$type == "supp") 2.5 else 2

    is_ref <- !is.null(ref_uid) && nzchar(ref_uid) && rw$uid == ref_uid
    is_hov <- !is.null(hover_uid) && nzchar(hover_uid) && rw$uid == hover_uid
    col <- if (is_ref) "magenta" else if (is_hov) "#ff7f0e" else base_col
    lw <- if (is_ref) 5 else if (is_hov) 4 else base_width

    extra_cos <- ""
    if (!is.null(pair_metrics)) {
      if (rw$uid == pair_metrics$ref_uid || rw$uid == pair_metrics$hov_uid) {
        extra_cos <- paste0("<br>cos(θ) vs reference: ", sprintf("%.4f", pair_metrics$cs))
      }
    }

    hover_txt <- paste0(
      "Variable: ", rw$display_var,
      "<br>Type: ", ifelse(rw$type == "supp", "supplementary", "active"),
      "<br>x: ", signif(rw$xend, 4),
      "<br>y: ", signif(rw$yend, 4),
      extra_cos
    )

    pl <- add_segments(
      pl,
      x = 0, y = 0, xend = rw$xend, yend = rw$yend,
      color = I(col),
      line = list(width = lw),
      inherit = FALSE,
      # source = source_id,
      key = rw$uid,
      customdata = rw$uid,
      text = hover_txt,
      hoverinfo = "text",
      showlegend = FALSE
    )
  }

  pl <- add_axis_pick_markers(pl, xlab_plain, ylab_plain, source_id = source_id)

  if (!is.null(pair_metrics)) {
    pl <- add_sector_with_label(pl, pair_metrics, source_id = source_id)
  }

  pl
}
