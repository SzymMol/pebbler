get_ggplot_fill_colors <- function(p) {
  tryCatch({
    gb <- ggplot_build(p)
    sc <- gb$plot$scales$get_scales("fill")
    if (!is.null(sc)) {
      lvls <- sc$get_limits()
      cols <- sc$map(lvls)
      if (length(cols) == length(lvls)) return(setNames(cols, lvls))
    }
    NULL
  }, error = function(e) NULL)
}
