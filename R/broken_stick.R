#' broken stick
#'
#' Calculate "broken stick" model values
#'
#' @param p integer
#' @returns vector of output values in the "broken stick" model
#' @examples
#' broken_stick(5)

broken_stick <- function(p) sapply(seq_len(p), function(k) sum(1 / (k:p)) / p)
