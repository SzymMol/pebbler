# PCA and LDA select data for biplot
project_for_axes_pair <- function(res, sc_key, vec_key, method = c("pca", "lda"), pair_x = 1, pair_y = 2, facet_by_id = FALSE) {
  method <- match.arg(method)
  i <- pair_x
  j <- pair_y

  sc <- res[[sc_key]]

  sc_plot <- sc
  sc_plot$Axis1_plot <- sc_plot[, i]
  sc_plot$Axis2_plot <- sc_plot[, j]
  sc_plot$Axis1 <- sc_plot$Axis1_plot
  sc_plot$Axis2 <- sc_plot$Axis2_plot

  vv <- res[[vec_key]]
  vv_plot <- vv
  vv_plot$x_plot <- vv_plot[, i]
  vv_plot$y_plot <- vv_plot[, j]
  vv_plot$x <- vv_plot$x_plot
  vv_plot$y <- vv_plot$y_plot

  sv <- res$supp_vectors
  if (nrow(sv) > 0) {
    sv_plot <- sv
    sv_plot$x_plot <- sv_plot[, i]
    sv_plot$y_plot <- sv_plot[, j]
    sv_plot$x <- sv_plot$x_plot
    sv_plot$y <- sv_plot$y_plot
  }
  else {
    sv_plot <- sv
  }

  labs_out <- build_axis_pct_labels(
    res = res,
    method = method,
    pair_x = pair_x,
    pair_y = pair_y,
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
