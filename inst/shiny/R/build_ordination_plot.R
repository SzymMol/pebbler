build_ordination_plot <- function(sc, hulls, cents, av, sv, group_colname,
                                  multi_id, cfg,
                                  interactive = FALSE) {
  FILLABLE_SHAPES <- c(21, 22, 23, 24, 25)

  sv_present <- nrow(sv) > 0
  if (isTRUE(multi_id) && isTRUE(cfg$id_in_vector_labels)) {
    if (!is.null(av) && nrow(av) > 0) av <- add_id_suffix_to_vec_labels(av, "dataset_id", "original_var")
    if (sv_present && !is.null(sv) && nrow(sv) > 0) sv <- add_id_suffix_to_vec_labels(sv, "dataset_id", "original_var")
  }

  # group_present <- "group" %in% names(sc) && nrow(sc) > 0
  group_present <- !is.null(group_colname)
  p <- ggplot()

  if (isTRUE(cfg$fill_hulls) && nrow(hulls) > 0 && group_present) {
    if (multi_id) {
      p <- p + geom_polygon(
        data = hulls,
        aes(Axis1, Axis2, group = interaction(.data[[group_colname]], dataset_id), fill = .data[[group_colname]]),
        alpha = cfg$hull_fill_alpha, color = NA, show.legend = FALSE
      )
    } else {
      p <- p + geom_polygon(
        data = hulls,
        aes(Axis1, Axis2, group = .data[[group_colname]], fill = .data[[group_colname]]),
        alpha = cfg$hull_fill_alpha, color = NA, show.legend = FALSE
      )
    }
  }

  if (isTRUE(cfg$show_hull_outlines) && nrow(hulls) > 0 && group_present) {
    if (multi_id) {
      p <- p + geom_polygon(
        data = hulls,
        aes(Axis1, Axis2, group = interaction(.data[[group_colname]], dataset_id), color = .data[[group_colname]], linetype = dataset_id),
        fill = NA, linewidth = 1.0, alpha = 0.98, show.legend = FALSE
      )
    } else {
      p <- p + geom_polygon(
        data = hulls,
        aes(Axis1, Axis2, group = .data[[group_colname]], color = .data[[group_colname]]),
        fill = NA, linewidth = 1.0, alpha = 0.98, show.legend = FALSE
      )
    }
  }

  if (isTRUE(cfg$show_zero_axes)) {
    p <- p +
      geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.9, color = "black") +
      geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.9, color = "black")
  }

  if (isTRUE(cfg$show_points)) {
    if (group_present) {
      if (multi_id) {
        p <- p + geom_point(
          data = sc,
          aes(Axis1, Axis2, color = .data[[group_colname]], fill = .data[[group_colname]], shape = dataset_id),
          size = cfg$pt_size, alpha = cfg$pt_alpha, show.legend = !isTRUE(cfg$show_centroids)
        )
      } else {
        p <- p + geom_point(
          data = sc,
          aes(Axis1, Axis2, color = .data[[group_colname]], fill = .data[[group_colname]]),
          shape = 21, size = cfg$pt_size, alpha = cfg$pt_alpha, show.legend = !isTRUE(cfg$show_centroids)
        )
      }
    } else {
      if (multi_id) {
        p <- p + geom_point(
          data = sc,
          aes(Axis1, Axis2, shape = dataset_id),
          size = cfg$pt_size, alpha = cfg$pt_alpha, color = "grey30", fill = "grey70", show.legend = !isTRUE(cfg$show_centroids)
        )
      } else {
        p <- p + geom_point(
          data = sc,
          aes(Axis1, Axis2),
          shape = 21, size = cfg$pt_size, alpha = cfg$pt_alpha, color = "grey30", fill = "grey70", show.legend = !isTRUE(cfg$show_centroids)
        )
      }
    }
  }

  if (isTRUE(cfg$show_centroids) && nrow(cents) > 0 && group_present) {
    if (multi_id) {
      id_levels <- levels(factor(sc$dataset_id))
      if (length(id_levels) == 0) id_levels <- unique(as.character(sc$dataset_id))
      sv2 <- setNames(rep(FILLABLE_SHAPES, length.out = length(id_levels)), id_levels)

      p <- p +
        geom_point(
          data = cents,
          aes(cx, cy, fill = group, shape = dataset_id),
          size = cfg$centroid_size, color = "black", stroke = 0.35, alpha = 1, show.legend = TRUE
        ) +
        scale_shape_manual(values = sv2)
    } else {
      p <- p + geom_point(
        data = cents,
        aes(cx, cy, fill = group),
        size = cfg$centroid_size, shape = 21, color = "black", stroke = 0.35, alpha = 1, show.legend = TRUE
      )
    }
  }

  if (!interactive && !is.null(av) && nrow(av) > 0) {
    if (multi_id) {
      p <- p + geom_segment(
        data = av, aes(x = 0, y = 0, xend = xend, yend = yend, linetype = dataset_id),
        arrow = arrow(length = unit(0.18, "cm")), color = "black", linewidth = 0.9, show.legend = FALSE, inherit.aes = FALSE
      )
    } else {
      p <- p + geom_segment(
        data = av, aes(x = 0, y = 0, xend = xend, yend = yend),
        arrow = arrow(length = unit(0.18, "cm")), color = "black", linewidth = 0.9, show.legend = FALSE, inherit.aes = FALSE
      )
    }
  }

  if (!interactive && sv_present) {
    if (multi_id) {
      p <- p + geom_segment(
        data = sv, aes(x = 0, y = 0, xend = xend, yend = yend, linetype = dataset_id),
        arrow = arrow(length = unit(0.24, "cm")), color = "red3", linewidth = 1.1, show.legend = FALSE, inherit.aes = FALSE
      )
    } else {
      p <- p + geom_segment(
        data = sv, aes(x = 0, y = 0, xend = xend, yend = yend),
        arrow = arrow(length = unit(0.24, "cm")), color = "red3", linewidth = 1.1, show.legend = FALSE, inherit.aes = FALSE
      )
    }
  }

  show_cent <- isTRUE(cfg$show_centroids) && isTRUE(cfg$show_centroid_labels) && nrow(cents) > 0 && group_present
  show_vec <- isTRUE(cfg$show_vector_labels)
  repel_layer_index <- NA
  all_lab <- NULL

  if (show_cent || show_vec) {
    group_levels <- if (group_present) levels(factor(sc[[group_colname]])) else character(0)
    centroid_colors <- get_default_group_colors(group_levels)

    vector_seed_density <- 4

    if (!interactive) {
      vector_seed_density <- 12
    }

    all_lab <- build_all_labels(
      av = if (!is.null(av) && nrow(av) > 0) av else NULL,
      sv = if (sv_present) sv else NULL,
      # cents = if (nrow(cents) > 0 && group_present) cents else NULL,
      cents = if (show_cent) cents else NULL,
      show_centroid_labels = show_cent,
      show_vector_labels = show_vec,
      centroid_colors = centroid_colors,
      vector_seed_density = vector_seed_density
    )

    if (nrow(all_lab) > 0) {
      p <- p + ggrepel::geom_text_repel(
        data = all_lab,
        aes(x = x, y = y, label = label),
        color = all_lab$label_color,
        segment.color = all_lab$seg_color,
        size = cfg$label_size / 2.2,
        box.padding = 0.8, point.padding = 0.5,
        force = 12, force_pull = 0.8,
        max.overlaps = Inf, min.segment.length = 0,
        segment.size = 0.35, max.iter = 30000, max.time = 8,
        seed = 123,
        show.legend = FALSE
      )
      repel_layer_index <- length(p$layers)
    }
  }

  list(plot = p, repel_layer_index = repel_layer_index, all_lab = all_lab, label_size = cfg$label_size)
}
