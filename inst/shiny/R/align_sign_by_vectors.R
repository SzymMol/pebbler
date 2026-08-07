align_sign_by_vectors <- function(scores_df, vec_df, ref_id) {
  ids <- unique(as.character(vec_df$dataset_id))
  if (!(ref_id %in% ids)) {
    return(list(
      scores = scores_df,
      vec = vec_df,
      signs = data.frame(dataset_id = ids, sgn1 = 1, sgn2 = 1)
    ))
  }

  ref <- vec_df %>% filter(as.character(dataset_id) == ref_id) %>% arrange(var)
  out_s <- list()
  out_v <- list()
  out_g <- list()

  for (idv in ids) {
    v <- vec_df %>% filter(as.character(dataset_id) == idv) %>% arrange(var)
    s <- scores_df %>% filter(as.character(dataset_id) == idv)

    common <- intersect(ref$var, v$var)
    sgn1 <- 1
    sgn2 <- 1

    if (length(common) >= 2) {
      r <- ref %>% filter(var %in% common) %>% arrange(var)
      w <- v %>% filter(var %in% common) %>% arrange(var)
      c1 <- suppressWarnings(cor(w$x, r$x, use = "complete.obs"))
      c2 <- suppressWarnings(cor(w$y, r$y, use = "complete.obs"))
      if (is.finite(c1) && c1 < 0) sgn1 <- -1
      if (is.finite(c2) && c2 < 0) sgn2 <- -1
    }

    s$Axis1 <- s$Axis1 * sgn1
    s$Axis2 <- s$Axis2 * sgn2
    if ("Axis3" %in% names(s)) s$Axis3 <- s$Axis3 * 1
    v$x <- v$x * sgn1
    v$y <- v$y * sgn2
    if ("z" %in% names(v)) v$z <- v$z * 1

    out_s[[idv]] <- s
    out_v[[idv]] <- v
    out_g[[idv]] <- data.frame(dataset_id = idv, sgn1 = sgn1, sgn2 = sgn2)
  }

  list(scores = bind_rows(out_s), vec = bind_rows(out_v), signs = bind_rows(out_g))
}
