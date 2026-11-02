#' Pebble morphometry measurements from Witów site (southern Poland)
#'
#' A dataset containing 3D morphometric measurements of pebble grains
#' (with diameter >= 8 mm) from a 4.4-m-high section of the Witów Series
#' gravel deposits in the Witów Quarry (southern Poland).
#' For details of geological setting, sampling and measurement methods,
#' refer to \doi{doi:10.14241/asgp.2024.11}.
#'
#' @format A data frame with 1432 rows and 7 columns:
#' \describe{
#'   \item{hgt_min}{Stratigraphic height at a sampled horizon bottom measured from
#'    the entire section bottom (in meters).}
#'   \item{hgt_max}{Stratigraphic height at a sampled horizon top measured from
#'    the entire section bottom (in meters).}
#'   \item{a}{Grain "length", i.e., the longest dimension (in millimeters).}
#'   \item{b}{Grain "width", i.e., the longest dimension perpendicular to
#'    a (in millimeters).}
#'   \item{c}{Grain "thickness", i.e., the longest dimension perpendicular to
#'    both a and b (in millimeters).}
#'   \item{mass}{Grain mass (in grams).}
#'   \item{lithotype}{Grain lithotype identifier.
#'    Abbreviations:
#'    very coarse-grained sandstone (vc),
#'    coarse-grained sandstone (c),
#'    medium-grained sandstone (m),
#'    fine-grained sandstone (f),
#'    very fine-grained sandstone (vf),
#'    quartz (q),
#'    lydite (l),
#'    granite (g).
#'    }
#' }
#'
#' @source \doi{doi:10.5281/zenodo.23438681}
#' @docType data
#' @name witow
"witow"
