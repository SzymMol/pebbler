vector_scale_df <- function(df, xcol, ycol, scale_factor = 1) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df %>% mutate(xend = .data[[xcol]] * scale_factor, yend = .data[[ycol]] * scale_factor)
}
