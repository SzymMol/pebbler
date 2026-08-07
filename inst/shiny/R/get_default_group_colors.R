get_default_group_colors <- function(group_levels) {
  n <- length(group_levels)
  if (n == 0) return(character(0))
  hues <- seq(15, 375, length.out = n + 1)[1:n]
  setNames(hcl(h = hues, l = 65, c = 100), group_levels)
}
