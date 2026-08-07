compute_centroid_displacements <- function(centroids_df, ref_uid) {
  if (is.null(ref_uid) || !nzchar(ref_uid) || is.null(centroids_df) || nrow(centroids_df) == 0) return(data.frame())
  ref <- centroids_df %>% filter(uid == ref_uid) %>% slice(1)
  if (nrow(ref) == 0) return(data.frame())

  out <- centroids_df %>%
    filter(group == ref$group, uid != ref$uid) %>%
    mutate(
      ref_uid = ref$uid,
      ref_group = ref$group,
      ref_dataset_id = ref$dataset_id,
      x1 = ref$cx, y1 = ref$cy,
      x2 = cx, y2 = cy,
      dx = x2 - x1,
      dy = y2 - y1,
      dist = sqrt(dx^2 + dy^2),
      mx = (x1 + x2) / 2,
      my = (y1 + y2) / 2
    )
  out
}
