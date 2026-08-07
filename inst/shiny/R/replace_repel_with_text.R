replace_repel_with_text <- function(p, repel_layer_index, all_lab, label_size_pt) {
  if (is.na(repel_layer_index) || repel_layer_index > length(p$layers)) return(p)

  point_to_segment_distance <- function(px, py, x1, y1, x2, y2) {
    vx <- x2 - x1
    vy <- y2 - y1
    wx <- px - x1
    wy <- py - y1
    c1 <- vx * wx + vy * wy
    c2 <- vx * vx + vy * vy
    if (!is.finite(c2) || c2 <= 1e-12) return(sqrt((px - x1)^2 + (py - y1)^2))
    t <- c1 / c2
    t <- max(0, min(1, t))
    qx <- x1 + t * vx
    qy <- y1 + t * vy
    sqrt((px - qx)^2 + (py - qy)^2)
  }

  gb <- ggplot_build(p)
  repel_built <- gb$data[[repel_layer_index]]
  p$layers[[repel_layer_index]] <- NULL

  if (is.null(repel_built) || nrow(repel_built) == 0) return(p)

  has_label <- nzchar(all_lab$label) & !is.na(all_lab$label)

  lab_positions <- data.frame(
    label_x = repel_built$x[has_label],
    label_y = repel_built$y[has_label],
    anchor_x = all_lab$x[has_label],
    anchor_y = all_lab$y[has_label],
    label = all_lab$label[has_label],
    label_color = all_lab$label_color[has_label],
    seg_color = all_lab$seg_color[has_label],
    type = all_lab$type[has_label],
    stringsAsFactors = FALSE
  )
  if ("dataset_id" %in% names(all_lab)) {
    lab_positions$dataset_id <- all_lab$dataset_id[has_label]
  }

  if (nrow(lab_positions) == 0) return(p)

  vec_rows <- all_lab %>% filter(type == "vec_label")
  av <- vec_rows %>% filter(label_color == "black")
  sv <- vec_rows %>% filter(label_color != "black")

  segs <- data.frame(x1 = numeric(), y1 = numeric(), x2 = numeric(), y2 = numeric())
  if (nrow(av) > 0) segs <- bind_rows(segs, data.frame(x1 = 0, y1 = 0, x2 = av$x, y2 = av$y))
  if (nrow(sv) > 0) segs <- bind_rows(segs, data.frame(x1 = 0, y1 = 0, x2 = sv$x, y2 = sv$y))

  pts <- all_lab %>% filter(type == "centroid_label")
  if (nrow(pts) > 0) segs <- bind_rows(segs, data.frame(x1 = pts$x, y1 = pts$y, x2 = pts$x, y2 = pts$y))

  xr <- max(abs(c(lab_positions$anchor_x, lab_positions$label_x, segs$x2)), na.rm = TRUE)
  yr <- max(abs(c(lab_positions$anchor_y, lab_positions$label_y, segs$y2)), na.rm = TRUE)
  if (!is.finite(xr) || xr <= 0) xr <- 1
  if (!is.finite(yr) || yr <= 0) yr <- 1

  segs <- bind_rows(
    segs,
    data.frame(x1 = -xr, y1 = 0, x2 = xr, y2 = 0),
    data.frame(x1 = 0, y1 = -yr, x2 = 0, y2 = yr)
  )

  min_dist <- max(0.020 * min(xr, yr), 0.007 * label_size_pt * min(xr, yr))

  lab_positions <- lab_positions %>%
    mutate(
      v_len = sqrt(anchor_x^2 + anchor_y^2),

      dir_x = ifelse(type == "vec_label", ifelse(v_len > 1e-5, anchor_x / v_len, 0.707), 0),
      dir_y = ifelse(type == "vec_label", ifelse(v_len > 1e-5, anchor_y / v_len, 0.707), 0),

      dir_x = ifelse(type == "centroid_label", ifelse(v_len > 1e-5, anchor_x / v_len, 0.707), dir_x),
      dir_y = ifelse(type == "centroid_label", ifelse(v_len > 1e-5, anchor_y / v_len, 0.707), dir_y),

      target_r = ifelse(type == "vec_label", min_dist * 0.8, min_dist * 1.5),

      final_x = anchor_x + dir_x * target_r,
      final_y = anchor_y + dir_y * target_r
    )

  for (iter in seq_len(36)) {
    moved_any <- FALSE

    for (i in seq_len(nrow(lab_positions))) {
      lx <- lab_positions$final_x[i]
      ly <- lab_positions$final_y[i]
      ax <- lab_positions$anchor_x[i]
      ay <- lab_positions$anchor_y[i]

      repel_x <- 0
      repel_y <- 0
      colliding <- FALSE

      effective_min_dist <- if (lab_positions$type[i] == "vec_label") min_dist * 0.9 else min_dist * 1.1

      for (j in seq_len(nrow(segs))) {
        d <- point_to_segment_distance(lx, ly, segs$x1[j], segs$y1[j], segs$x2[j], segs$y2[j])

        is_own_anchor <- FALSE
        if (abs(segs$x1[j] - segs$x2[j]) < 1e-5 && abs(segs$y1[j] - segs$y2[j]) < 1e-5) {
          if (abs(segs$x1[j] - ax) < 1e-5 && abs(segs$y1[j] - ay) < 1e-5) {
            is_own_anchor <- TRUE
          }
        }

        if (is.finite(d) && d < effective_min_dist) {
          if (is_own_anchor) next

          x1 <- segs$x1[j]; y1 <- segs$y1[j]
          x2 <- segs$x2[j]; y2 <- segs$y2[j]
          vx <- x2 - x1; vy <- y2 - y1
          vv <- vx * vx + vy * vy

          if (!is.finite(vv) || vv <= 1e-12) {
            nx <- lx - x1; ny <- ly - y1
            nn <- sqrt(nx * nx + ny * ny)
            if (nn > 1e-10) { nx <- nx / nn; ny <- ny / nn } else { nx <- 0.707; ny <- 0.707 }
          } else {
            t <- ((lx - x1) * vx + (ly - y1) * vy) / vv
            t <- max(0, min(1, t))
            qx <- x1 + t * vx
            qy <- y1 + t * vy
            nx <- lx - qx; ny <- ly - qy
            nn <- sqrt(nx * nx + ny * ny)
            if (nn > 1e-10) { nx <- nx / nn; ny <- ny / nn } else { nx <- 0.707; ny <- 0.707 }
          }

          force <- (effective_min_dist - d) / effective_min_dist
          repel_x <- repel_x + nx * force * effective_min_dist * 0.15
          repel_y <- repel_y + ny * force * effective_min_dist * 0.15
          colliding <- TRUE
        }
      }

      for (k in seq_len(nrow(lab_positions))) {
        if (i == k) next
        d <- sqrt((lx - lab_positions$final_x[k])^2 + (ly - lab_positions$final_y[k])^2)

        label_dist_thresh <- if (lab_positions$type[i] == "vec_label") effective_min_dist * 1.5 else effective_min_dist * 2.0

        if (is.finite(d) && d < label_dist_thresh) {
          nx <- lx - lab_positions$final_x[k]
          ny <- ly - lab_positions$final_y[k]
          nn <- sqrt(nx * nx + ny * ny)
          if (nn > 1e-10) { nx <- nx / nn; ny <- ny / nn } else { nx <- 0.707; ny <- 0.707 }

          force <- (label_dist_thresh - d) / label_dist_thresh
          repel_x <- repel_x + nx * force * effective_min_dist * 0.40
          repel_y <- repel_y + ny * force * effective_min_dist * 0.40
          colliding <- TRUE
        }
      }

      pull_x <- 0
      pull_y <- 0

      d_from_anchor <- sqrt((lx - ax)^2 + (ly - ay)^2)

      target_dist <- lab_positions$target_r[i]

      if (d_from_anchor > target_dist) {
        dir_ax <- (ax - lx) / d_from_anchor
        dir_ay <- (ay - ly) / d_from_anchor
        pull_strength <- if (colliding) 0.01 else 0.15
        pull_x <- dir_ax * (d_from_anchor - target_dist) * pull_strength
        pull_y <- dir_ay * (d_from_anchor - target_dist) * pull_strength
      } else if (d_from_anchor < target_dist * 0.95) {
        dir_ox <- (lx - ax) / (d_from_anchor + 1e-8)
        dir_oy <- (ly - ay) / (d_from_anchor + 1e-8)

        if (d_from_anchor < 1e-5) {
          dir_ox <- lab_positions$dir_x[i]
          dir_oy <- lab_positions$dir_y[i]
        }

        pull_strength <- 0.15
        pull_x <- dir_ox * (target_dist - d_from_anchor) * pull_strength
        pull_y <- dir_oy * (target_dist - d_from_anchor) * pull_strength
      }

      if (colliding && d_from_anchor > 1e-5) {
        dir_ox <- (lx - ax) / d_from_anchor
        dir_oy <- (ly - ay) / d_from_anchor
        tangent_dot <- repel_x * (-dir_oy) + repel_y * dir_ox
        repel_x <- repel_x + (-dir_oy) * tangent_dot * 0.2
        repel_y <- repel_y + dir_ox * tangent_dot * 0.2
      }

      total_dx <- repel_x + pull_x
      total_dy <- repel_y + pull_y

      if (abs(total_dx) > 1e-5 || abs(total_dy) > 1e-5) {
        lab_positions$final_x[i] <- lx + total_dx
        lab_positions$final_y[i] <- ly + total_dy
        moved_any <- TRUE
      }
    }

    if (!moved_any) break
  }

  for (i in seq_len(nrow(lab_positions))) {
    d_from_anchor <- sqrt((lab_positions$final_x[i] - lab_positions$anchor_x[i])^2 +
                            (lab_positions$final_y[i] - lab_positions$anchor_y[i])^2)

    max_wander <- min_dist * 3.5
    if (d_from_anchor > max_wander) {
      dir_ax <- (lab_positions$final_x[i] - lab_positions$anchor_x[i]) / d_from_anchor
      dir_ay <- (lab_positions$final_y[i] - lab_positions$anchor_y[i]) / d_from_anchor
      lab_positions$final_x[i] <- lab_positions$anchor_x[i] + dir_ax * max_wander
      lab_positions$final_y[i] <- lab_positions$anchor_y[i] + dir_ay * max_wander
    }
  }

  p <- p + geom_text(
    data = lab_positions,
    aes(x = final_x, y = final_y, label = label),
    color = lab_positions$label_color,
    size = label_size_pt / 2.2,
    show.legend = FALSE
  )

  p
}
