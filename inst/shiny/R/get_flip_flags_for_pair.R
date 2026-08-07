get_flip_flags_for_pair <- function(pair, flip_axis1, flip_axis2) {
  list(flip_x = isTRUE(flip_axis1), flip_y = isTRUE(flip_axis2))
}
