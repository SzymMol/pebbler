build_axis_pct_labels <- function(res, method = c("pca", "lda"), pair_x = 1, pair_y = 2, #pair = "1_2",
                                  facet_by_id = FALSE, digits = 1) {
  method <- match.arg(method)
  # ij <- parse_axes_pair(pair)
  # i <- ij[1]
  # j <- ij[2]
  i <- pair_x
  j <- pair_y

  if (method == "pca") {
    # pct_df <- axis_percent_from_pca_eigen(res$eigen)
    # prefix <- "PC"
    pct_df <- res$eigen %>%
              mutate(pct = explained_var_pct, axis_index = PC)
  } else {
    # pct_df <- res$axis_pct %||% data.frame()
    # prefix <- "LD"
    pct_df <- res$axis_pct %>%
              mutate(axis_index = LD)
  }

  if (isTRUE(res$split_mode)) {
    pct_df <- pct_df %>% select(dataset_id, axis_index, pct)
  }
  else {
    pct_df <- pct_df %>% select(axis_index, pct)
  }
  prefix <- ""

  list(
    x_display = make_axis_label_with_pct(prefix, i, pct_df, split_mode = isTRUE(res$split_mode),
                                         facet_by_id = facet_by_id, digits = digits),
    y_display = make_axis_label_with_pct(prefix, j, pct_df, split_mode = isTRUE(res$split_mode),
                                         facet_by_id = facet_by_id, digits = digits),
    x_plain = paste0(prefix, i),
    y_plain = paste0(prefix, j)
  )
}
