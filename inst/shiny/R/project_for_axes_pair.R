project_for_axes_pair <- function(res, sc_key, vec_key, method = c("pca", "lda"), pair_x = 1, pair_y = 2, facet_by_id = FALSE) {
  method <- match.arg(method)
  # ij <- parse_axes_pair(pair)
  # i <- ij[1]
  # j <- ij[2]
  i <- pair_x
  j <- pair_y

  sc <- res[[sc_key]]
  # sc <- res$scores
  # validate(need(all(c("Axis1", "Axis2", "Axis3") %in% names(sc)), "Internal score axis columns missing."))
  # validate(need(i <= 3 && j <= 3, "Only 3 axes are supported for display."))
  # validate(need(any(abs(sc[[paste0("Axis", j)]]) > 0, na.rm = TRUE), paste0(
    # toupper(method), ": ", ifelse(method == "pca", paste0("PC", j), paste0("LD", j)),
    # " not available for this run."
  # )))

  sc_plot <- sc
  # sc_plot$Axis1_plot <- sc_plot[[paste0("Axis", i)]]
  # sc_plot$Axis2_plot <- sc_plot[[paste0("Axis", j)]]
  sc_plot$Axis1_plot <- sc_plot[, i]
  sc_plot$Axis2_plot <- sc_plot[, j]
  sc_plot$Axis1 <- sc_plot$Axis1_plot
  sc_plot$Axis2 <- sc_plot$Axis2_plot

  # vec_key <- if (method == "pca") "eig_vectors" else "scaling"
  vv <- res[[vec_key]]
  # validate(need(all(c("x", "y", "z") %in% names(vv)), "Internal loading/scaling axis columns missing."))
  vv_plot <- vv
  # vv_plot$x_plot <- vv_plot[[c("x", "y", "z")[i]]]
  # vv_plot$y_plot <- vv_plot[[c("x", "y", "z")[j]]]
  vv_plot$x_plot <- vv_plot[, i]
  vv_plot$y_plot <- vv_plot[, j]
  vv_plot$x <- vv_plot$x_plot
  vv_plot$y <- vv_plot$y_plot

  sv <- res$supp_vectors
  if (nrow(sv) > 0) {
  # if (has_supp_rows(sv) && all(c("x", "y", "z") %in% names(sv))) {
    sv_plot <- sv
    # sv_plot$x_plot <- sv_plot[[c("x", "y", "z")[i]]]
    # sv_plot$y_plot <- sv_plot[[c("x", "y", "z")[j]]]
    sv_plot$x_plot <- sv_plot[, i]
    sv_plot$y_plot <- sv_plot[, j]
    sv_plot$x <- sv_plot$x_plot
    sv_plot$y <- sv_plot$y_plot
  } else {
    sv_plot <- sv
  }

  labs_out <- build_axis_pct_labels(
    res = res,
    method = method,
    pair_x = pair_x,
    pair_y = pair_y,
    # pair = pair,
    facet_by_id = facet_by_id
  )

  list(
    scores = sc_plot,
    vectors = vv_plot,
    supp_vectors = sv_plot,
    xlab = labs_out$x_display,
    ylab = labs_out$y_display,
    xlab_plain = labs_out$x_plain,
    ylab_plain = labs_out$y_plain
  )
}
