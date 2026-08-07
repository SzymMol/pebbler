ggshape_to_plotly <- function(shape_num) {
  map <- c("21" = "circle", "22" = "square", "23" = "diamond", "24" = "triangle-up", "25" = "triangle-down")
  s <- as.character(shape_num)
  if (s %in% names(map)) map[[s]] else "circle"
}
