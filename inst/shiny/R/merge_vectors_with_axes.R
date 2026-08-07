merge_vectors_with_axes <- function(vec_df, xlab_plain, ylab_plain) {
  bind_rows(vec_df, get_virtual_axis_df(xlab_plain, ylab_plain))
}
