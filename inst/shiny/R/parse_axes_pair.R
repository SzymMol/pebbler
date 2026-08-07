parse_axes_pair <- function(x) {
  parts <- strsplit(x %||% "1_2", "_", fixed = TRUE)[[1]]
  if (length(parts) != 2) return(c(1L, 2L))
  out <- suppressWarnings(as.integer(parts))
  if (any(is.na(out))) c(1L, 2L) else out
}
