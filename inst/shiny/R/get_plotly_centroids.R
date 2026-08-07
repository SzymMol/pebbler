get_plotly_centroids <- function(projected, group_colname, show_centroids, flip_x = FALSE, flip_y = FALSE) {
  if (is.null(group_colname) || !isTRUE(show_centroids)) return(data.frame())
  sc <- projected$scores
  if (is.null(sc) || nrow(sc) == 0 || !(group_colname %in% names(sc))) return(data.frame())
  multi_id <- n_distinct(sc$dataset_id) > 1
  ce <- compute_centroids(sc, group_colname, "Axis1", "Axis2", if (multi_id) "dataset_id" else NULL)
  if (is.null(ce) || nrow(ce) == 0) return(data.frame())
  if (!("dataset_id" %in% names(ce))) ce$dataset_id <- ""
  ce$group <- as.character(ce$group)
  ce$dataset_id <- as.character(ce$dataset_id %||% "")
  ce$uid <- ifelse(
    is.na(ce$dataset_id) | ce$dataset_id == "",
    paste0("cent__", ce$group),
    paste0("cent__", ce$group, "__", ce$dataset_id)
  )
  ce$label <- ifelse(
    is.na(ce$dataset_id) | ce$dataset_id == "",
    ce$group,
    paste0(ce$group, " (", ce$dataset_id, ")")
  )
  if (isTRUE(flip_x)) ce$cx <- -ce$cx
  if (isTRUE(flip_y)) ce$cy <- -ce$cy
  ce
}
