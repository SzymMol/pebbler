# PCA and LDA calculation
make_ord_result <- function(dat, roles, has_id, id_col, compare_mode,
                            method, flip_axs_vals) {
  mode <- if (has_id) (compare_mode %||% "pooled") else "pooled"

  # split datasets mode
  if (has_id && mode == "split") {
    res <- run_split_ordination(
      dat = dat, id_col = id_col, method = method,
      flip_axs_vals = flip_axs_vals, roles = roles
    )
    return(res)
  }
  # single dataset mode
  if (method == "pca") {
    one <- run_pca(x_active = dat[roles$active],
                   x_supp = dat[roles$supp],
                   y_group = dat[roles$group],
                   flip_cols = flip_axs_vals[1, ]
                   )
    list(
      scores = one$scores,
      eig_vectors = one$eig_vects,
      scores_std = one$scores_std,
      loadings = one$loadings,
      supp_vectors = one$supp,
      eigen = one$eigen,
      split_mode = FALSE
    )
  } else {
    one <- run_lda(x_active = dat[roles$active],
                   x_supp = dat[roles$supp],
                   y_group = dat[roles$group],
                   flip_cols = flip_axs_vals[1, ]
                   )
    list(
      scores = one$scores,
      scaling = one$scaling,
      supp_vectors = one$supp,
      axis_pct = one$axis_pct,
      split_mode = FALSE
    )
  }
}
