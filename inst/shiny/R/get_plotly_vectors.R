get_plotly_vectors <- function(projected, cfg, multi_id, include_id_suffix = FALSE) {
  vv <- projected$vectors
  if (is.null(vv) || nrow(vv) == 0) return(data.frame())
  av <- vector_scale_df(vv, "x", "y", cfg$vec_scale_active)
  av$type <- "active"
  av$dataset_id <- as.character(av$dataset_id %||% "")
  av$uid <- if (multi_id) paste0("var__", av$original_var, "__", av$dataset_id) else paste0("var__", as.character(av$original_var))
  av$display_var <- as.character(av$original_var)
  if (multi_id && include_id_suffix) av$display_var <- paste0(av$display_var, "_", av$dataset_id)

  sv <- projected$supp_vectors
  sv_scale <- if (cfg$lock_vec_scale) cfg$vec_scale_active else cfg$vec_scale_supp
  if (has_supp_rows(sv)) {
    sv2 <- vector_scale_df(sv, "x", "y", sv_scale)
    sv2$type <- "supp"
    sv2$dataset_id <- as.character(sv2$dataset_id %||% "")
    sv2$uid <- if (multi_id) paste0("supp__", sv2$original_var, "__", sv2$dataset_id) else paste0("supp__", as.character(sv2$original_var))
    sv2$display_var <- as.character(sv2$original_var)
    if (multi_id && include_id_suffix) sv2$display_var <- paste0(sv2$display_var, "_", sv2$dataset_id)
    out <- bind_rows(av, sv2)
  } else {
    out <- av
  }
  out
}
