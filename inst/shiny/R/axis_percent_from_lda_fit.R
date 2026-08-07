axis_percent_from_lda_fit <- function(fit, dataset_id = "all") {
  if (is.null(fit) || is.null(fit$svd)) return(data.frame())
  vals <- fit$svd^2
  if (length(vals) == 0) return(data.frame())
  pct <- vals / sum(vals) * 100
  data.frame(
    dataset_id = dataset_id,
    axis_index = seq_along(pct),
    pct = pct,
    stringsAsFactors = FALSE
  )
}
