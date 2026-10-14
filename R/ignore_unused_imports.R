#' Ignore unused imports
#'
#' Avoid disturbing Import notes when running devtools::check()
#'
ignore_unused_imports <- function() {
  DT::addRow
  ggrepel::element_text_repel
  grid::absolute.size
  plotly::add_annotations
  readr::as.col_spec
  scales::abs_area
  tidyr::all_of
}
