build_all_labels <- function(av = NULL, sv = NULL, cents = NULL,
                             show_centroid_labels = TRUE,
                             show_vector_labels = TRUE,
                             centroid_colors = NULL,
                             vector_seed_density = 4) {
  parts <- list()
  seed <- function(x, y, dataset_id = NA) data.frame(
    x = x, y = y, label = "",
    label_color = "black", seg_color = "black",
    type = "seed", dataset_id = dataset_id,
    stringsAsFactors = FALSE
  )

  ds_name_bkgd <- if ("dataset_id" %in% names(av)) av$dataset_id[1] else NA
  parts$origin <- seed(0, 0, ds_name_bkgd)

  if (!is.null(cents) && nrow(cents) > 0) {
    grps <- as.character(cents$group)
    ds_cent <- if ("dataset_id" %in% names(cents)) cents$dataset_id else NA
    ccols <- if (!is.null(centroid_colors)) {
      ifelse(grps %in% names(centroid_colors), centroid_colors[grps], "grey40")
    } else {
      rep("grey40", length(grps))
    }

    if (show_centroid_labels) {
      parts$cent_labels <- data.frame(
        x = cents$cx, y = cents$cy, label = grps,
        label_color = ccols, seg_color = ccols,
        type = "centroid_label",
        dataset_id = ds_cent,
        stringsAsFactors = FALSE
      )
    }
    parts$cent_seeds <- seed(cents$cx, cents$cy, ds_cent)
  }
  if (!is.null(av) && nrow(av) > 0) {
    ds_av <- if ("dataset_id" %in% names(av)) av$dataset_id else NA
    if (show_vector_labels) {
      parts$av_labels <- data.frame(
        x = av$xend, y = av$yend, label = av$original_var,
        label_color = "black", seg_color = "grey40",
        type = "vec_label",
        dataset_id = ds_av,
        stringsAsFactors = FALSE
      )
    }
    line_seeds <- make_arrow_line_seeds(av$xend, av$yend, n_per_arrow = vector_seed_density)
    if (nrow(line_seeds) > 0) {
      ds_av_line <- if (all(is.na(ds_av))) NA else rep(ds_av, each = vector_seed_density)
      parts$av_line_seeds <- seed(line_seeds$x, line_seeds$y, ds_av_line)
    }
  }

  if (!is.null(sv) && nrow(sv) > 0) {
    ds_sv <- if ("dataset_id" %in% names(sv)) sv$dataset_id else NA
    if (show_vector_labels) {
      parts$sv_labels <- data.frame(
        x = sv$xend, y = sv$yend, label = sv$original_var,
        label_color = "red3", seg_color = "firebrick",
        type = "vec_label",
        dataset_id = ds_sv,
        stringsAsFactors = FALSE
      )
    }
    line_seeds <- make_arrow_line_seeds(sv$xend, sv$yend, n_per_arrow = vector_seed_density)
    if (nrow(line_seeds) > 0) {
      ds_sv_line <- if (all(is.na(ds_sv))) NA else rep(ds_sv, each = vector_seed_density)
      parts$sv_line_seeds <- seed(line_seeds$x, line_seeds$y, ds_sv_line)
    }
  }

  all_x <- c(0)
  all_y <- c(0)

  if (!is.null(cents) && nrow(cents) > 0) {
    all_x <- c(all_x, cents$cx)
    all_y <- c(all_y, cents$cy)
  }
  if (!is.null(av) && nrow(av) > 0) {
    all_x <- c(all_x, av$xend)
    all_y <- c(all_y, av$yend)
  }
  if (!is.null(sv) && nrow(sv) > 0) {
    all_x <- c(all_x, sv$xend)
    all_y <- c(all_y, sv$yend)
  }

  xr <- range(all_x, na.rm = TRUE)
  yr <- range(all_y, na.rm = TRUE)

  padx <- max(diff(xr) * 0.08, 0.15)
  pady <- max(diff(yr) * 0.08, 0.15)

  axis_x <- seq(xr[1] - padx, xr[2] + padx, length.out = 120)
  axis_y <- seq(yr[1] - pady, yr[2] + pady, length.out = 120)

  parts$xaxis_seeds <- seed(axis_x, rep(0, length(axis_x)), ds_name_bkgd)
  parts$yaxis_seeds <- seed(rep(0, length(axis_y)), axis_y, ds_name_bkgd)

  out <- bind_rows(parts)
  if (is.null(out) || nrow(out) == 0) {
    return(data.frame(
      x = numeric(), y = numeric(), label = character(),
      label_color = character(), seg_color = character(),
      type = character(), dataset_id = factor()
    ))
  }
  out
}
