apply_sign_alignment_if_needed <- function(res, align_mode, ref_id) {
  if ((align_mode %||% "none") != "sign") return(res)

  al <- align_sign_by_vectors(res$scores, res$loadings, ref_id)
  res$scores <- al$scores
  res$loadings <- al$vec

  if (has_supp_rows(res$supp_vectors)) {
    res$supp_vectors <- res$supp_vectors %>%
      left_join(al$signs, by = "dataset_id") %>%
      mutate(
        sgn1 = ifelse(is.na(sgn1), 1, sgn1),
        sgn2 = ifelse(is.na(sgn2), 1, sgn2),
        x = x * sgn1,
        y = y * sgn2
      ) %>%
      select(-sgn1, -sgn2)
  }

  res
}
