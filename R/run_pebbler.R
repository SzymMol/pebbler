#' Run pebbler
#'
#' Run pebbler ShinyApp
#'
#' @export
#' @examples
#' if (interactive()) {
#'   run_pebbler()
#' }
#' @importFrom shiny runApp

run_pebbler <- function() {
  runApp(system.file("shiny", package = "pebbler")) # for user, devtools::load_all() changes for development mode
  # runApp("inst/shiny/app.R") # for development
}
