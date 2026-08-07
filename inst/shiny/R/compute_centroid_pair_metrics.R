compute_centroid_pair_metrics <- function(centroids_df, ref_uid, hov_uid) {
  if (is.null(ref_uid) || is.null(hov_uid) || !nzchar(ref_uid) || !nzchar(hov_uid)) return(NULL)
  r <- centroids_df %>% filter(uid == ref_uid) %>% slice(1)
  h <- centroids_df %>% filter(uid == hov_uid) %>% slice(1)
  if (nrow(r) == 0 || nrow(h) == 0) return(NULL)

  x1 <- as.numeric(r$cx)
  y1 <- as.numeric(r$cy)
  x2 <- as.numeric(h$cx)
  y2 <- as.numeric(h$cy)
  dx <- x2 - x1
  dy <- y2 - y1
  d <- sqrt(dx^2 + dy^2)
  if (!is.finite(d)) return(NULL)

  list(
    ref = as.character(r$label),
    hov = as.character(h$label),
    ref_uid = as.character(r$uid),
    hov_uid = as.character(h$uid),
    ref_group = as.character(r$group),
    hov_group = as.character(h$group),
    ref_dataset_id = as.character(r$dataset_id),
    hov_dataset_id = as.character(h$dataset_id),
    x1 = x1, y1 = y1,
    x2 = x2, y2 = y2,
    dx = dx, dy = dy,
    dist = d,
    mx = (x1 + x2) / 2,
    my = (y1 + y2) / 2
  )
}
