compute_pair_metrics <- function(all_df, ref_uid, hov_uid) {
  if (is.null(ref_uid) || is.null(hov_uid) || !nzchar(ref_uid) || !nzchar(hov_uid)) return(NULL)
  r <- all_df %>% filter(uid == ref_uid) %>% slice(1)
  h <- all_df %>% filter(uid == hov_uid) %>% slice(1)
  if (nrow(r) == 0 || nrow(h) == 0) return(NULL)

  rx <- as.numeric(r$xend); ry <- as.numeric(r$yend)
  hx <- as.numeric(h$xend); hy <- as.numeric(h$yend)
  nr <- sqrt(rx^2 + ry^2); nh <- sqrt(hx^2 + hy^2)
  if (!is.finite(nr) || !is.finite(nh) || nr == 0 || nh == 0) return(NULL)

  cs <- (rx * hx + ry * hy) / (nr * nh)
  cs <- max(-1, min(1, cs))
  ang <- acos(cs) * 180 / pi

  tr <- atan2(ry, rx)
  th <- atan2(hy, hx)
  d <- th - tr
  while (d > pi) d <- d - 2 * pi
  while (d < -pi) d <- d + 2 * pi

  list(
    ref = as.character(r$display_var),
    hov = as.character(h$display_var),
    ref_uid = as.character(r$uid),
    hov_uid = as.character(h$uid),
    ref_type = as.character(r$type),
    hov_type = as.character(h$type),
    rx = rx, ry = ry, hx = hx, hy = hy,
    nr = nr, nh = nh,
    ang = ang, cs = cs,
    theta_ref = tr, theta_hov = th, dtheta = d
  )
}
