run_split_ordination <- function(dat, id_col, method, flip_axs_vals, roles) {
  ids <- unique(as.character(dat[[id_col]]))
  parts <- lapply(ids, function(idv) {
    dat_split <- dat %>% filter(as.character(.data[[id_col]]) == idv)
    if (method == "pca") {
      run_pca(
        x_active = dat_split[roles$active],
        x_supp = dat_split[roles$supp],
        y_group = dat_split[roles$group],
        idv = idv,
        flip_cols = flip_axs_vals[idv, ],
        roles = roles
      )
    }
    else {
      run_lda(
        x_active = dat_split[roles$active],
        x_supp = dat_split[roles$supp],
        y_group = dat_split[roles$group],
        idv = idv,
        flip_cols = flip_axs_vals[idv, ],
        roles = roles
      )
    }
  })

  if (method == "pca") {
    scores <- bind_rows(lapply(parts, `[[`, "scores"))
    eig_vects <- bind_rows(lapply(parts, `[[`, "eig_vects"))
    scores_std <- bind_rows(lapply(parts, `[[`, "scores_std"))
    loadings <- bind_rows(lapply(parts, `[[`, "loadings"))
    supp <- bind_rows(lapply(parts, `[[`, "supp"))
    out <- list(scores = scores, eig_vectors = eig_vects,
                scores_std = scores_std, loadings = loadings,
                supp_vectors = supp, split_mode = TRUE)
  }
  else {
    scores <- bind_rows(lapply(parts, `[[`, "scores"))
    scaling <- bind_rows(lapply(parts, `[[`, "scaling"))
    supp <- bind_rows(lapply(parts, `[[`, "supp"))
    out <- list(scores = scores, scaling = scaling,
                supp_vectors = supp, split_mode = TRUE)

  }
  if (method == "pca") out$eigen <- bind_rows(lapply(parts, `[[`, "eigen"))
  if (method == "lda") out$axis_pct <- bind_rows(lapply(parts, `[[`, "axis_pct"))
  out
}
