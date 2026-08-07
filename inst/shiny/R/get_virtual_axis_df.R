get_virtual_axis_df <- function(xlab_plain, ylab_plain) {
  data.frame(
    uid = c(
      paste0("axis__", xlab_plain, "_pos"),
      paste0("axis__", xlab_plain, "_neg"),
      paste0("axis__", ylab_plain, "_pos"),
      paste0("axis__", ylab_plain, "_neg")
    ),
    display_var = c(xlab_plain, paste0("-", xlab_plain), ylab_plain, paste0("-", ylab_plain)),
    xend = c(1, -1, 0, 0),
    yend = c(0, 0, 1, -1),
    type = "axis",
    dataset_id = "",
    stringsAsFactors = FALSE
  )
}
