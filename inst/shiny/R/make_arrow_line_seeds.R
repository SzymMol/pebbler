make_arrow_line_seeds <- function(xend, yend, n_per_arrow = 12) {
  if (length(xend) == 0) return(data.frame(x = numeric(), y = numeric()))
  fracs <- seq(0.10, 0.98, length.out = n_per_arrow)
  bind_rows(lapply(seq_along(xend), function(i) {
    data.frame(x = xend[i] * fracs, y = yend[i] * fracs)
  }))
}
