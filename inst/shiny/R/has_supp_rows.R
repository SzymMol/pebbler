has_supp_rows <- function(sv) {
  !is.null(sv) && is.data.frame(sv) && nrow(sv) > 0 && "x" %in% names(sv) && "y" %in% names(sv)
}
