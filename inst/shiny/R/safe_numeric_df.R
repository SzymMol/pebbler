safe_numeric_df <- function(df, cols) {
  if (is.null(cols) || length(cols) == 0) return(data.frame())
  out <- df[, cols, drop = FALSE]
  out[, sapply(out, is.numeric), drop = FALSE]
}
