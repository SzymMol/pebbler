make_ord_result <- function(dat, roles, has_id, id_col, compare_mode,
                            method, flip_axs_vals, align_mode, ref_id) {
  mode <- if (has_id) (compare_mode %||% "pooled") else "pooled"

  if (has_id && mode == "split") {
    res <- run_split_ordination(
      dat = dat, id_col = id_col, method = method,
      flip_axs_vals = flip_axs_vals, roles = roles
    )
    res <- apply_sign_alignment_if_needed(res, align_mode = align_mode, ref_id = ref_id)
    return(res)
  }

  if (method == "pca") {
    one <- run_pca(x_active = dat[roles$active], x_supp = dat[roles$supp],
                   y_group = dat[roles$group],
                   flip_cols = flip_axs_vals[1, ], roles = roles)
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
    # one <- run_ordination(
    #   df_sub = dat, idv = "all", method = method,
    #   flip1 = flip_axs_vals[1, 1], flip2 = flip_axs_vals[1, 2], roles = roles
    # )
    one <- run_lda(x_active = dat[roles$active], x_supp = dat[roles$supp],
                   y_group = dat[roles$group],
                   flip_cols = flip_axs_vals[1, ], roles = roles)
    # list(
    #   fit = one$fit,
    #   scores = one$scores,
    #   scaling = one$loadings,
    #   supp_vectors = one$supp,
    #   axis_pct = one$axis_pct,
    #   split_mode = FALSE
    # )
    list(
      # fit = one$fit,
      scores = one$scores,
      scaling = one$scaling,
      supp_vectors = one$supp,
      axis_pct = one$axis_pct,
      split_mode = FALSE
    )
  }
}
