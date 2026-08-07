get_ord_cfg <- function(input, prefix) {
  list(
    show_points = isTRUE(input[[paste0(prefix, "_show_points")]]),
    fill_hulls = isTRUE(input[[paste0(prefix, "_fill_hulls")]]),
    show_hull_outlines = isTRUE(input[[paste0(prefix, "_show_hull_outlines")]]),
    show_zero_axes = isTRUE(input[[paste0(prefix, "_show_zero_axes")]]),
    hull_fill_alpha = input[[paste0(prefix, "_hull_fill_alpha")]],
    pt_alpha = input[[paste0(prefix, "_pt_alpha")]],
    pt_size = input[[paste0(prefix, "_pt_size")]],
    centroid_size = input[[paste0(prefix, "_centroid_size")]],
    label_size = input[[paste0(prefix, "_label_size")]],
    show_centroid_labels = isTRUE(input[[paste0(prefix, "_show_centroid_labels")]]),
    show_vector_labels = isTRUE(input[[paste0(prefix, "_show_vector_labels")]]),
    id_in_vector_labels = isTRUE(input[[paste0(prefix, "_id_in_vector_labels")]]),
    vec_scale_active = input[[paste0(prefix, "_vec_scale_active")]],
    vec_scale_supp = input[[paste0(prefix, "_vec_scale_supp")]],
    lock_vec_scale = isTRUE(input[[paste0(prefix, "_lock_vec_scale")]]),
    equal_aspect = isTRUE(input[[paste0(prefix, "_equal_aspect")]]),
    show_centroids = isTRUE(input[[paste0(prefix, "_show_centroids")]])
  )
}
