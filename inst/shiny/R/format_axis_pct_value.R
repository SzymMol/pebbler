format_axis_pct_value <- function(x, digits = 1) {
  ifelse(is.na(x), "NA", paste0(formatC(x, format = "f", digits = digits), "%"))
}
