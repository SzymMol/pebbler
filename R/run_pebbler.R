#' Run pebbler
#'
#' Run pebbler ShinyApp
#'
#' @export
#' @examples
#' run_pebbler()
#' @importFrom shiny runApp

run_pebbler <- function() {
  runApp("inst/shiny/pebbler_app.R")
}
