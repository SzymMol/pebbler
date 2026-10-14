# pebbler: multivariate pebble morphometry made easy

# required_packages <- c("shiny", "ggplot2", "ggrepel", "dplyr", "readr", "DT", "MASS", "scales", "tidyr", "plotly")
# new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
# if (length(new_packages)) install.packages(new_packages)

library(shiny)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(readr)
library(DT)
library(MASS)
library(scales)
library(tidyr)
library(plotly)
library(grid)
library(pebbler)

# source("../../R/broken_stick.R")
# source("../../R/plot_pca_scree.R")
# source("../../R/run_pca.R")
# source("../../R/run_lda.R")


# `%||%` <- function(x, y) if (is.null(x)) y else x
FILLABLE_SHAPES <- c(21, 22, 23, 24, 25)

AXIS_FONT_SIZE <- 16
# broken_stick <- function(p) sapply(seq_len(p), function(k) sum(1 / (k:p)) / p)


ui <- fluidPage(
  tags$head(tags$style(HTML("
.top-aligned-row { display: flex; align-items: flex-end; flex-wrap: wrap; }
.top-aligned-row > div { float: none; }
.checkbox { margin-top: 0px; margin-bottom: 0px; }
.vector-note { margin-top: 6px; margin-bottom: 0px; font-size: 14px; color: #444; }
"))),
  titlePanel("pebbler: multivariate pebble morphometry"),
  tabsetPanel(
    id = "main_tabs",
    # tabPanel(
    #   "Data", br(),
    #   fluidRow(
    #     column(4, fileInput("file", "Upload CSV", accept = ".csv")),
    #     column(4, div(style = "padding-top: 25px;", actionButton("apply_roles", "Apply variables")))
    #   ),
    #   tags$div(class = "top-aligned-row", column(4, uiOutput("active_ui")), column(4, uiOutput("group_ui")), column(4, uiOutput("supp_ui"))),
    #   hr(), uiOutput("comparison_heading"),
    #   fluidRow(column(6, uiOutput("compare_ids_ui")), column(6, uiOutput("compare_mode_ui"))),
    #   hr(), DTOutput("preview")
    # ),
    input_data_ui("Data"),
    preprocessing_ui("Preprocessing"),
    # tabPanel(
    #   "Preprocessing", br(), uiOutput("prep_cols_ui"),
    #   checkboxInput("do_log", "Log10 transform selected columns", FALSE),
    #   checkboxInput("do_scale", "Z-score standardize selected columns", FALSE),
    #   fluidRow(
    #     column(4, actionButton("apply_prep", "Apply preprocessing")),
    #     column(4, actionButton("undo_prep", "Undo preprocessing"))
    #   ),
    #   br(), h4("Active variable plots"),
    #   plotOutput("inspect_line_plot", height = 420), plotOutput("inspect_hist_plot", height = 660), hr(), verbatimTextOutput("prep_log")
    # ),
    pca_ui("PCA"),
    # tabPanel(
    #   "PCA", br(), ord_top_controls("pca", "PCA"),
    #   hr(), uiOutput("pca_comparison_heading"),
    #   fluidRow(column(4, uiOutput("pca_align_mode_ui")), column(4, uiOutput("pca_ref_id_ui")), column(4, uiOutput("pca_facet_ui"))),
    #   hr(), h4("PCA ordination"), uiOutput("pca_plot_container"),
    #   fluidRow(
    #     column(6, htmlOutput("pca_vector_note")),
    #     column(6, div(style = "padding-top: 2px;", uiOutput("pca_angle_controls"), uiOutput("pca_centroid_controls")))
    #   ),
    #   hr(),
    #   fluidRow(
    #     column(3, checkboxInput("pca_show_points", "Show points", FALSE)),
    #     column(3, checkboxInput("pca_fill_hulls", "Fill convex hulls", FALSE)),
    #     column(3, checkboxInput("pca_show_hull_outlines", "Show hull outlines", FALSE)),
    #     column(3, checkboxInput("pca_show_zero_axes", "Bold zero axes", TRUE))
    #   ),
    #   fluidRow(
    #     column(3, sliderInput("pca_hull_fill_alpha", "Hull fill opacity", 0, 1, 0.5, 0.05)),
    #     column(3, sliderInput("pca_pt_alpha", "Point opacity", 0, 1, 0.5, 0.05)),
    #     column(3, sliderInput("pca_pt_size", "Point size", 0.5, 8, 2.3, 0.1)),
    #     column(3, sliderInput("pca_centroid_size", "Centroid size", 1, 12, 4.5, 0.5))
    #   ),
    #   fluidRow(
    #     column(3, sliderInput("pca_label_size", "Label size", 3, 24, 10, 0.5)),
    #     column(3, checkboxInput("pca_show_centroid_labels", "Show centroid labels", TRUE)),
    #     column(2, checkboxInput("pca_show_vector_labels", "Show vector labels", TRUE)),
    #     column(2, checkboxInput("pca_id_in_vector_labels", "ID in vector labels", TRUE))
    #   ),
    #   fluidRow(
    #     column(4, sliderInput("pca_vec_scale_active", "Active variable vector scale", 0.1, 8, 1.5, 0.1)),
    #     column(4, sliderInput("pca_vec_scale_supp", "Supplementary variable vector scale", 0.1, 100, 6, 0.1)),
    #     column(4, checkboxInput("pca_lock_vec_scale", "Lock vector scaling", FALSE))
    #   ),
    #   hr(), plotOutput("scree_plot", height = 320), DTOutput("pca_tbl")
    # ),
    lda_ui("LDA")

    # tabPanel(
    #   "LDA", br(), ord_top_controls("lda", "LDA"),
    #   hr(), uiOutput("lda_comparison_heading"),
    #   fluidRow(column(4, uiOutput("lda_align_mode_ui")), column(4, uiOutput("lda_ref_id_ui")), column(4, uiOutput("lda_facet_ui"))),
    #   hr(), h4("LDA ordination"), uiOutput("lda_plot_container"),
    #   fluidRow(
    #     column(6, htmlOutput("lda_vector_note")),
    #     column(6, div(style = "padding-top: 2px;", uiOutput("lda_angle_controls"), uiOutput("lda_centroid_controls")))
    #   ),
    #   hr(),
    #   fluidRow(
    #     column(3, checkboxInput("lda_show_points", "Show points", FALSE)),
    #     column(3, checkboxInput("lda_fill_hulls", "Fill convex hulls", FALSE)),
    #     column(3, checkboxInput("lda_show_hull_outlines", "Show hull outlines", FALSE)),
    #     column(3, checkboxInput("lda_show_zero_axes", "Bold zero axes", TRUE))
    #   ),
    #   fluidRow(
    #     column(3, sliderInput("lda_hull_fill_alpha", "Hull fill opacity", 0, 1, 0.5, 0.05)),
    #     column(3, sliderInput("lda_pt_alpha", "Point opacity", 0, 1, 0.5, 0.05)),
    #     column(3, sliderInput("lda_pt_size", "Point size", 0.5, 8, 2.3, 0.1)),
    #     column(3, sliderInput("lda_centroid_size", "Centroid size", 1, 12, 4.5, 0.5))
    #   ),
    #   fluidRow(
    #     column(3, sliderInput("lda_label_size", "Label size", 3, 24, 10, 0.5)),
    #     column(3, checkboxInput("lda_show_centroid_labels", "Show centroid labels", TRUE)),
    #     column(2, checkboxInput("lda_show_vector_labels", "Show vector labels", TRUE)),
    #     column(2, checkboxInput("lda_id_in_vector_labels", "ID in vector labels", TRUE))
    #   ),
    #   fluidRow(
    #     column(4, sliderInput("lda_vec_scale_active", "Active variable vector scale", 0.1, 8, 1.5, 0.1)),
    #     column(4, sliderInput("lda_vec_scale_supp", "Supplementary variable vector scale", 0.1, 100, 6, 0.1)),
    #     column(4, checkboxInput("lda_lock_vec_scale", "Lock vector scaling", FALSE))
    #   ),
    #   hr(), verbatimTextOutput("lda_info"), DTOutput("lda_tbl")
    # )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    raw_data = NULL, data = NULL, id_col = NULL,
    roles = list(active = character(), group = NULL, supp = character()),
    roles_applied = FALSE, prep_log = list(),
    pca_lock_sync = FALSE, lda_lock_sync = FALSE,
    cmp_mode = NULL
  )

  angle_rv <- reactiveValues(pca_ref = NULL, pca_hover = NULL, lda_ref = NULL, lda_hover = NULL)
  centroid_rv <- reactiveValues(pca_ref = NULL, pca_hover = NULL, lda_ref = NULL, lda_hover = NULL)

  input_data_server("Data", rv)
  preprocessing_server("Preprocessing", rv)
  pca_server("PCA", rv)
  lda_server("LDA", rv)

  # get_plotly_vectors <- function(projected, cfg, multi_id, include_id_suffix = FALSE) {
  #   vv <- projected$vectors
  #   if (is.null(vv) || nrow(vv) == 0) return(data.frame())
  #   av <- vector_scale_df(vv, "x", "y", cfg$vec_scale_active)
  #   av$type <- "active"
  #   av$dataset_id <- as.character(av$dataset_id %||% "")
  #   av$uid <- if (multi_id) paste0("var__", av$var, "__", av$dataset_id) else paste0("var__", as.character(av$var))
  #   av$display_var <- as.character(av$var)
  #   if (multi_id && include_id_suffix) av$display_var <- paste0(av$display_var, "_", av$dataset_id)
  #
  #   sv <- projected$supp_vectors
  #   sv_scale <- if (cfg$lock_vec_scale) cfg$vec_scale_active else cfg$vec_scale_supp
  #   if (has_supp_rows(sv)) {
  #     sv2 <- vector_scale_df(sv, "x", "y", sv_scale)
  #     sv2$type <- "supp"
  #     sv2$dataset_id <- as.character(sv2$dataset_id %||% "")
  #     sv2$uid <- if (multi_id) paste0("supp__", sv2$var, "__", sv2$dataset_id) else paste0("supp__", as.character(sv2$var))
  #     sv2$display_var <- as.character(sv2$var)
  #     if (multi_id && include_id_suffix) sv2$display_var <- paste0(sv2$display_var, "_", sv2$dataset_id)
  #     out <- bind_rows(av, sv2)
  #   } else {
  #     out <- av
  #   }
  #   out
  # }
  #
  # get_plotly_centroids <- function(projected, flip_x = FALSE, flip_y = FALSE) {
  #   sc <- projected$scores
  #   if (is.null(sc) || nrow(sc) == 0 || !("group" %in% names(sc))) return(data.frame())
  #   multi_id <- n_distinct(sc$dataset_id) > 1
  #   ce <- compute_centroids(sc, "group", "Axis1", "Axis2", if (multi_id) "dataset_id" else NULL)
  #   if (is.null(ce) || nrow(ce) == 0) return(data.frame())
  #   if (!("dataset_id" %in% names(ce))) ce$dataset_id <- ""
  #   ce$group <- as.character(ce$group)
  #   ce$dataset_id <- as.character(ce$dataset_id %||% "")
  #   ce$uid <- ifelse(
  #     is.na(ce$dataset_id) | ce$dataset_id == "",
  #     paste0("cent__", ce$group),
  #     paste0("cent__", ce$group, "__", ce$dataset_id)
  #   )
  #   ce$label <- ifelse(
  #     is.na(ce$dataset_id) | ce$dataset_id == "",
  #     ce$group,
  #     paste0(ce$group, " (", ce$dataset_id, ")")
  #   )
  #   if (isTRUE(flip_x)) ce$cx <- -ce$cx
  #   if (isTRUE(flip_y)) ce$cy <- -ce$cy
  #   ce
  # }
  #
  # get_virtual_axis_df <- function(xlab_plain, ylab_plain) {
  #   data.frame(
  #     uid = c(
  #       paste0("axis__", xlab_plain, "_pos"),
  #       paste0("axis__", xlab_plain, "_neg"),
  #       paste0("axis__", ylab_plain, "_pos"),
  #       paste0("axis__", ylab_plain, "_neg")
  #     ),
  #     display_var = c(xlab_plain, paste0("-", xlab_plain), ylab_plain, paste0("-", ylab_plain)),
  #     xend = c(1, -1, 0, 0),
  #     yend = c(0, 0, 1, -1),
  #     type = "axis",
  #     dataset_id = "",
  #     stringsAsFactors = FALSE
  #   )
  # }
  #
  # merge_vectors_with_axes <- function(vec_df, xlab_plain, ylab_plain) {
  #   bind_rows(vec_df, get_virtual_axis_df(xlab_plain, ylab_plain))
  # }
  #
  # compute_pair_metrics <- function(all_df, ref_uid, hov_uid) {
  #   if (is.null(ref_uid) || is.null(hov_uid) || !nzchar(ref_uid) || !nzchar(hov_uid)) return(NULL)
  #   r <- all_df %>% filter(uid == ref_uid) %>% slice(1)
  #   h <- all_df %>% filter(uid == hov_uid) %>% slice(1)
  #   if (nrow(r) == 0 || nrow(h) == 0) return(NULL)
  #
  #   rx <- as.numeric(r$xend); ry <- as.numeric(r$yend)
  #   hx <- as.numeric(h$xend); hy <- as.numeric(h$yend)
  #   nr <- sqrt(rx^2 + ry^2); nh <- sqrt(hx^2 + hy^2)
  #   if (!is.finite(nr) || !is.finite(nh) || nr == 0 || nh == 0) return(NULL)
  #
  #   cs <- (rx * hx + ry * hy) / (nr * nh)
  #   cs <- max(-1, min(1, cs))
  #   ang <- acos(cs) * 180 / pi
  #
  #   tr <- atan2(ry, rx)
  #   th <- atan2(hy, hx)
  #   d <- th - tr
  #   while (d > pi) d <- d - 2 * pi
  #   while (d < -pi) d <- d + 2 * pi
  #
  #   list(
  #     ref = as.character(r$display_var),
  #     hov = as.character(h$display_var),
  #     ref_uid = as.character(r$uid),
  #     hov_uid = as.character(h$uid),
  #     ref_type = as.character(r$type),
  #     hov_type = as.character(h$type),
  #     rx = rx, ry = ry, hx = hx, hy = hy,
  #     nr = nr, nh = nh,
  #     ang = ang, cs = cs,
  #     theta_ref = tr, theta_hov = th, dtheta = d
  #   )
  # }
  #
  # compute_centroid_pair_metrics <- function(centroids_df, ref_uid, hov_uid) {
  #   if (is.null(ref_uid) || is.null(hov_uid) || !nzchar(ref_uid) || !nzchar(hov_uid)) return(NULL)
  #   r <- centroids_df %>% filter(uid == ref_uid) %>% slice(1)
  #   h <- centroids_df %>% filter(uid == hov_uid) %>% slice(1)
  #   if (nrow(r) == 0 || nrow(h) == 0) return(NULL)
  #
  #   x1 <- as.numeric(r$cx)
  #   y1 <- as.numeric(r$cy)
  #   x2 <- as.numeric(h$cx)
  #   y2 <- as.numeric(h$cy)
  #   dx <- x2 - x1
  #   dy <- y2 - y1
  #   d <- sqrt(dx^2 + dy^2)
  #   if (!is.finite(d)) return(NULL)
  #
  #   list(
  #     ref = as.character(r$label),
  #     hov = as.character(h$label),
  #     ref_uid = as.character(r$uid),
  #     hov_uid = as.character(h$uid),
  #     ref_group = as.character(r$group),
  #     hov_group = as.character(h$group),
  #     ref_dataset_id = as.character(r$dataset_id),
  #     hov_dataset_id = as.character(h$dataset_id),
  #     x1 = x1, y1 = y1,
  #     x2 = x2, y2 = y2,
  #     dx = dx, dy = dy,
  #     dist = d,
  #     mx = (x1 + x2) / 2,
  #     my = (y1 + y2) / 2
  #   )
  # }
  #
  # compute_centroid_displacements <- function(centroids_df, ref_uid) {
  #   if (is.null(ref_uid) || !nzchar(ref_uid) || is.null(centroids_df) || nrow(centroids_df) == 0) return(data.frame())
  #   ref <- centroids_df %>% filter(uid == ref_uid) %>% slice(1)
  #   if (nrow(ref) == 0) return(data.frame())
  #
  #   out <- centroids_df %>%
  #     filter(group == ref$group, uid != ref$uid) %>%
  #     mutate(
  #       ref_uid = ref$uid,
  #       ref_group = ref$group,
  #       ref_dataset_id = ref$dataset_id,
  #       x1 = ref$cx, y1 = ref$cy,
  #       x2 = cx, y2 = cy,
  #       dx = x2 - x1,
  #       dy = y2 - y1,
  #       dist = sqrt(dx^2 + dy^2),
  #       mx = (x1 + x2) / 2,
  #       my = (y1 + y2) / 2
  #     )
  #   out
  # }
  #
  # add_plotly_centroid_traces <- function(pl, centroids_df, source_id, group_colors = NULL, id_shapes = NULL, ref_uid = NULL, hover_uid = NULL, base_size = 13) {
  #   if (is.null(centroids_df) || nrow(centroids_df) == 0) return(pl)
  #   pair_metrics <- compute_centroid_pair_metrics(centroids_df, ref_uid, hover_uid)
  #   disp <- compute_centroid_displacements(centroids_df, ref_uid)
  #
  #   ref_group <- NULL
  #   if (!is.null(ref_uid) && nzchar(ref_uid)) {
  #     rr <- centroids_df %>% filter(uid == ref_uid) %>% slice(1)
  #     if (nrow(rr) > 0) ref_group <- as.character(rr$group)
  #   }
  #
  #   for (i in seq_len(nrow(centroids_df))) {
  #     rw <- centroids_df[i, , drop = FALSE]
  #     grp <- as.character(rw$group)
  #     ds <- as.character(rw$dataset_id)
  #     base_col <- if (!is.null(group_colors) && grp %in% names(group_colors)) group_colors[[grp]] else "grey40"
  #     base_symbol <- if (!is.null(id_shapes) && nzchar(ds) && ds %in% names(id_shapes)) id_shapes[[ds]] else "circle"
  #     is_ref <- !is.null(ref_uid) && nzchar(ref_uid) && rw$uid == ref_uid
  #     is_hov <- !is.null(hover_uid) && nzchar(hover_uid) && rw$uid == hover_uid
  #     has_ref <- !is.null(ref_uid) && nzchar(ref_uid)
  #     same_group <- !is.null(ref_group) && nzchar(ref_group) && identical(grp, ref_group)
  #     outline_group <- has_ref && (is_ref || same_group)
  #     outline_symbol <- if (nzchar(base_symbol)) paste0(base_symbol, "-open") else "circle-open"
  #     base_opacity <- if (has_ref && !is_ref && !is_hov && !same_group) 0.30 else if (has_ref && same_group) 0.95 else 1
  #     hover_txt <- paste0(
  #       "Centroid: ", rw$label,
  #       "<br>Group: ", grp,
  #       if (nzchar(ds)) paste0("<br>ID: ", ds) else "",
  #       "<br>x: ", signif(rw$cx, 4),
  #       "<br>y: ", signif(rw$cy, 4),
  #       if (!is.null(pair_metrics) && (rw$uid == pair_metrics$ref_uid || rw$uid == pair_metrics$hov_uid)) paste0(
  #         "<br>dx: ", sprintf("%.4f", pair_metrics$dx),
  #         "<br>dy: ", sprintf("%.4f", pair_metrics$dy),
  #         "<br>d: ", sprintf("%.4f", pair_metrics$dist)
  #       ) else ""
  #     )
  #
  #     pl <- add_trace(
  #       pl,
  #       type = "scatter",
  #       mode = "markers",
  #       x = rw$cx,
  #       y = rw$cy,
  #       marker = list(
  #         symbol = base_symbol,
  #         size = base_size,
  #         color = base_col,
  #         opacity = base_opacity,
  #         line = list(color = "black", width = 1.1)
  #       ),
  #       key = rw$uid,
  #       customdata = rw$uid,
  #       text = hover_txt,
  #       hoverinfo = "text",
  #       showlegend = FALSE,
  #       inherit = FALSE,
  #       source = source_id
  #     )
  #
  #     if (outline_group) {
  #       pl <- add_trace(
  #         pl,
  #         type = "scatter",
  #         mode = "markers",
  #         x = rw$cx,
  #         y = rw$cy,
  #         marker = list(
  #           symbol = outline_symbol,
  #           size = base_size + 8,
  #           color = "black",
  #           line = list(width = 4.8)
  #         ),
  #         hoverinfo = "skip",
  #         showlegend = FALSE,
  #         inherit = FALSE,
  #         source = source_id
  #       )
  #     }
  #   }
  #
  #   if (nrow(disp) > 0) {
  #     for (i in seq_len(nrow(disp))) {
  #       rw <- disp[i, , drop = FALSE]
  #       tgt_group <- as.character(rw$group)
  #       tgt_col <- if (!is.null(group_colors) && tgt_group %in% names(group_colors)) group_colors[[tgt_group]] else "rgba(35,35,35,0.95)"
  #       pl <- add_annotations(
  #         pl,
  #         x = rw$x2, y = rw$y2,
  #         ax = rw$x1, ay = rw$y1,
  #         xref = "x", yref = "y", axref = "x", ayref = "y",
  #         text = "",
  #         showarrow = TRUE,
  #         arrowhead = 3,
  #         arrowsize = 1.2,
  #         arrowwidth = 2.2,
  #         arrowcolor = tgt_col,
  #         opacity = 0.95
  #       )
  #     }
  #   }
  #
  #   if (!is.null(pair_metrics)) {
  #     pl <- add_annotations(
  #       pl,
  #       x = pair_metrics$x2, y = pair_metrics$y2,
  #       ax = pair_metrics$x1, ay = pair_metrics$y1,
  #       xref = "x", yref = "y", axref = "x", ayref = "y",
  #       text = "",
  #       showarrow = TRUE,
  #       arrowhead = 0,
  #       arrowsize = 1,
  #       arrowwidth = 3.2,
  #       arrowcolor = "rgba(0, 0, 0, 0.88)",
  #       opacity = 1
  #     )
  #
  #     pl <- add_annotations(
  #       pl,
  #       x = pair_metrics$mx,
  #       y = pair_metrics$my,
  #       xref = "x",
  #       yref = "y",
  #       text = paste0("<b>d = ", sprintf("%.3f", pair_metrics$dist), "</b>"),
  #       showarrow = FALSE,
  #       font = list(color = "black", size = 24),
  #       xanchor = "center",
  #       yanchor = "middle",
  #       opacity = 1
  #     )
  #   }
  #
  #   pl
  # }
  #
  # add_sector_with_label <- function(pl, metrics, source_id = NULL) {
  #   if (is.null(metrics)) return(pl)
  #
  #   a0 <- metrics$theta_ref
  #   a1 <- metrics$theta_ref + metrics$dtheta
  #   n <- 120
  #   th <- seq(a0, a1, length.out = n)
  #   r_sector <- 0.33 * min(metrics$nr, metrics$nh)
  #   if (!is.finite(r_sector) || r_sector <= 0) return(pl)
  #
  #   x_poly <- c(0, r_sector * cos(th), 0)
  #   y_poly <- c(0, r_sector * sin(th), 0)
  #
  #   pl <- add_trace(
  #     pl,
  #     type = "scatter",
  #     mode = "lines",
  #     x = x_poly,
  #     y = y_poly,
  #     fill = "toself",
  #     fillcolor = "rgba(0, 170, 0, 0.25)",
  #     line = list(color = "rgba(0, 150, 0, 0.55)", width = 1.5),
  #     hoverinfo = "skip",
  #     showlegend = FALSE,
  #     inherit = FALSE,
  #     source = source_id
  #   )
  #
  #   amid <- metrics$theta_ref + metrics$dtheta / 2
  #   rlab <- r_sector * 0.62
  #   xl <- rlab * cos(amid)
  #   yl <- rlab * sin(amid)
  #
  #   pl <- add_trace(
  #     pl,
  #     type = "scatter",
  #     mode = "text",
  #     x = xl, y = yl,
  #     text = paste0("<b>cos(θ)=", sprintf("%.4f", metrics$cs), "</b>"),
  #     textfont = list(color = "darkgreen", size = 24),
  #     hoverinfo = "skip",
  #     showlegend = FALSE,
  #     inherit = FALSE,
  #     source = source_id
  #   )
  #
  #   pl
  # }
  #
  # add_axis_pick_markers <- function(pl, xlab_plain, ylab_plain, source_id) {
  #   ax <- get_virtual_axis_df(xlab_plain, ylab_plain)
  #
  #   marker_df <- data.frame(
  #     uid = ax$uid,
  #     display_var = ax$display_var,
  #     x = c(0.85, -0.85, 0, 0),
  #     y = c(0, 0, 0.85, -0.85),
  #     stringsAsFactors = FALSE
  #   )
  #
  #   for (i in seq_len(nrow(marker_df))) {
  #     rw <- marker_df[i, , drop = FALSE]
  #     pl <- add_trace(
  #       pl,
  #       type = "scatter",
  #       mode = "markers",
  #       x = rw$x, y = rw$y,
  #       marker = list(size = 14, color = "rgba(0,0,0,0)", line = list(color = "rgba(0,0,0,0)", width = 0)),
  #       key = rw$uid,
  #       customdata = rw$uid,
  #       hoverinfo = "text",
  #       text = paste0("Axis selector: ", rw$display_var),
  #       showlegend = FALSE,
  #       inherit = FALSE,
  #       source = source_id
  #     )
  #   }
  #
  #   pl
  # }
  #
  # add_plotly_vector_traces <- function(pl, vectors_df, source_id, ref_uid = NULL, hover_uid = NULL,
  #                                      xlab_plain = NULL, ylab_plain = NULL) {
  #   if (is.null(vectors_df) || nrow(vectors_df) == 0) return(pl)
  #
  #   all_df <- merge_vectors_with_axes(vectors_df, xlab_plain, ylab_plain)
  #   pair_metrics <- compute_pair_metrics(all_df, ref_uid, hover_uid)
  #
  #   for (i in seq_len(nrow(vectors_df))) {
  #     rw <- vectors_df[i, , drop = FALSE]
  #     base_col <- if (rw$type == "supp") "red3" else "black"
  #     base_width <- if (rw$type == "supp") 2.5 else 2
  #
  #     is_ref <- !is.null(ref_uid) && nzchar(ref_uid) && rw$uid == ref_uid
  #     is_hov <- !is.null(hover_uid) && nzchar(hover_uid) && rw$uid == hover_uid
  #     col <- if (is_ref) "magenta" else if (is_hov) "#ff7f0e" else base_col
  #     lw <- if (is_ref) 5 else if (is_hov) 4 else base_width
  #
  #     extra_cos <- ""
  #     if (!is.null(pair_metrics)) {
  #       if (rw$uid == pair_metrics$ref_uid || rw$uid == pair_metrics$hov_uid) {
  #         extra_cos <- paste0("<br>cos(θ) vs reference: ", sprintf("%.4f", pair_metrics$cs))
  #       }
  #     }
  #
  #     hover_txt <- paste0(
  #       "Variable: ", rw$display_var,
  #       "<br>Type: ", ifelse(rw$type == "supp", "supplementary", "active"),
  #       "<br>x: ", signif(rw$xend, 4),
  #       "<br>y: ", signif(rw$yend, 4),
  #       extra_cos
  #     )
  #
  #     pl <- add_segments(
  #       pl,
  #       x = 0, y = 0, xend = rw$xend, yend = rw$yend,
  #       line = list(color = col, width = lw),
  #       inherit = FALSE,
  #       source = source_id,
  #       key = rw$uid,
  #       customdata = rw$uid,
  #       text = hover_txt,
  #       hoverinfo = "text",
  #       showlegend = FALSE
  #     )
  #   }
  #
  #   pl <- add_axis_pick_markers(pl, xlab_plain, ylab_plain, source_id = source_id)
  #
  #   if (!is.null(pair_metrics)) {
  #     pl <- add_sector_with_label(pl, pair_metrics, source_id = source_id)
  #   }
  #
  #   pl
  # }

  # observeEvent(input$file, {
  #   req(input$file)
  #   dat <- read_csv(input$file$datapath, show_col_types = FALSE)
  #   rv$raw_data <- dat
  #   rv$data <- dat
  #   rv$id_col <- detect_id_col(names(dat))
  #   rv$roles_applied <- FALSE
  #   rv$prep_log <- list()
  # })

  has_id <- reactive(!is.null(rv$id_col))

  # output$comparison_heading <- renderUI({
  #   req(rv$data)
  #   if (has_id()) tags$h4("Comparison mode ON") else tags$h4("Comparison mode OFF (no ID column)")
  # })
  # output$pca_comparison_heading <- renderUI({
  #   req(rv$data)
  #   if (has_id()) tags$h4("Comparison mode ON") else tags$h4("Comparison mode OFF (no ID column)")
  # })
  output$lda_comparison_heading <- renderUI({
    req(rv$data)
    if (has_id()) tags$h4("Comparison mode ON") else tags$h4("Comparison mode OFF (no ID column)")
  })

  # output$compare_ids_ui <- renderUI({
  #   req(rv$data)
  #   if (!has_id()) return(NULL)
  #   ids <- unique(as.character(rv$data[[rv$id_col]]))
  #   selectizeInput("compare_ids", "Section IDs to compare", choices = ids, selected = ids, multiple = TRUE)
  # })

  # output$compare_mode_ui <- renderUI({
  #   req(rv$data)
  #   if (!has_id()) return(NULL)
  #   selectInput(
  #     "compare_mode", "Ordination mode",
  #     choices = c("Common for selected IDs" = "pooled", "Separate for each ID (overlaid)" = "split"),
  #     selected = "split"
  #   )
  # })

  # output$pca_align_mode_ui <- renderUI({
  #   req(rv$data)
  #   if (!has_id()) return(NULL)
  #   selectInput(
  #     "pca_align_mode", "How to align separate section plots",
  #     choices = c("None (raw orientation)" = "none", "Auto-flip axes to match reference section" = "sign"),
  #     selected = "none"
  #   )
  # })

  output$lda_align_mode_ui <- renderUI({
    req(rv$data)
    if (!has_id()) return(NULL)
    selectInput(
      "lda_align_mode", "How to align separate section plots",
      choices = c("None (raw orientation)" = "none", "Auto-flip axes to match reference section" = "sign"),
      selected = "none"
    )
  })

  # output$pca_ref_id_ui <- renderUI({
  #   req(rv$data)
  #   if (!has_id()) return(NULL)
  #   ids <- unique(as.character(rv$data[[rv$id_col]]))
  #   selectInput("pca_ref_id", "Reference section for alignment", choices = ids, selected = ids[1])
  # })

  output$lda_ref_id_ui <- renderUI({
    req(rv$data)
    if (!has_id()) return(NULL)
    ids <- unique(as.character(rv$data[[rv$id_col]]))
    selectInput("lda_ref_id", "Reference section for alignment", choices = ids, selected = ids[1])
  })

  # output$pca_facet_ui <- renderUI({
  #   req(rv$data)
  #   if (!has_id()) return(NULL)
  #   checkboxInput("pca_facet_by_id", "Show each section in a separate panel", FALSE)
  # })

  # output$lda_facet_ui <- renderUI({
  #   req(rv$data)
  #   if (!has_id()) return(NULL)
  #   checkboxInput("lda_facet_by_id", "Show each section in a separate panel", FALSE)
  # })

  # filtered_data <- reactive({
  #   req(rv$data)
  #   dat <- rv$data
  #   if (has_id()) {
  #     keep_ids <- input$compare_ids %||% unique(as.character(dat[[rv$id_col]]))
  #     dat <- dat %>% filter(as.character(.data[[rv$id_col]]) %in% keep_ids)
  #   }
  #   dat
  # })

  # output$preview <- renderDT({
  #   req(filtered_data())
  #   datatable(head(filtered_data(), 100), options = list(scrollX = TRUE))
  # })

  # output$active_ui <- renderUI({
  #   req(rv$data)
  #   selectizeInput("active_vars", "Pebble measurements (active variables)", choices = names(rv$data), multiple = TRUE)
  # })

  # output$group_ui <- renderUI({
  #   req(rv$data)
  #   selectInput(
  #     "group_var", "Lithotype (grouping variable)",
  #     choices = c(" " = "", names(rv$data)),
  #     selected = character(0)
  #   )
  # })

  # output$supp_ui <- renderUI({
  #   req(rv$data)
  #   selectizeInput("supp_vars", "Stratigraphic height/depth (supplementary variable)", choices = names(rv$data), multiple = TRUE)
  # })

  # observeEvent(input$apply_roles, {
  #   req(input$active_vars)
  #
  #   rv$roles <- list(
  #     active = input$active_vars %||% character(0),
  #     group = if (!is.null(input$group_var) && nzchar(input$group_var)) input$group_var else NULL,
  #     supp = input$supp_vars %||% character(0)
  #   )
  #   rv$roles_applied <- TRUE
  #
  #   showNotification(
  #     paste0(
  #       "Roles applied: active=", length(rv$roles$active),
  #       ", group=", ifelse(is.null(rv$roles$group), "none", rv$roles$group),
  #       ", supplementary=", length(rv$roles$supp)
  #     ),
  #     type = "message", duration = 4
  #   )
  # })

  # roles_ready <- reactive({
  #   isTRUE(rv$roles_applied) &&
  #     length(rv$roles$active %||% character(0)) > 0
  # })

  # output$prep_cols_ui <- renderUI({
  #   req(filtered_data(), roles_ready())
  #   ch <- setdiff(rv$roles$active %||% character(0), rv$roles$supp %||% character(0))
  #   ch <- ch[ch %in% names(filtered_data())[sapply(filtered_data(), is.numeric)]]
  #   validate(need(length(ch) > 0, "No eligible active numeric variables."))
  #   selectizeInput("prep_cols", "Columns to preprocess (selected active only)", choices = ch, selected = ch, multiple = TRUE)
  # })

  # observeEvent(input$apply_prep, {
  #   req(rv$data, roles_ready(), input$prep_cols)
  #
  #   active_numeric <- setdiff(rv$roles$active %||% character(0), rv$roles$supp %||% character(0))
  #   active_numeric <- active_numeric[active_numeric %in% names(rv$data)[sapply(rv$data, is.numeric)]]
  #   cols <- intersect(input$prep_cols, active_numeric)
  #
  #   if (length(cols) == 0) return()
  #   if (!isTRUE(input$do_log) && !isTRUE(input$do_scale)) {
  #     showNotification("Select at least one preprocessing option.", type = "warning")
  #     return()
  #   }
  #
  #   dat <- rv$data
  #
  #   if (isTRUE(input$do_log)) {
  #     for (cl in cols) {
  #       if (any(dat[[cl]] <= 0, na.rm = TRUE)) {
  #         showNotification(paste("Skipping log10 for", cl, "(<=0 values)."), type = "warning")
  #       } else {
  #         dat[[cl]] <- log10(dat[[cl]])
  #       }
  #     }
  #   }
  #
  #   if (isTRUE(input$do_scale)) {
  #     for (cl in cols) dat[[cl]] <- as.numeric(scale(dat[[cl]]))
  #   }
  #
  #   rv$data <- dat
  #   rv$prep_log <- append(rv$prep_log, list(list(
  #     time = as.character(Sys.time()),
  #     cols = cols,
  #     log10 = isTRUE(input$do_log),
  #     zscore = isTRUE(input$do_scale)
  #   )))
  #   showNotification("Preprocessing applied.", type = "message")
  # })

  # observeEvent(input$undo_prep, {
  #   req(rv$raw_data)
  #   rv$data <- rv$raw_data
  #   rv$id_col <- detect_id_col(names(rv$raw_data))
  #   rv$prep_log <- list()
  #   updateCheckboxInput(session, "do_log", value = FALSE)
  #   updateCheckboxInput(session, "do_scale", value = FALSE)
  #   showNotification("Preprocessing undone. Original uploaded data restored.", type = "message")
  # })

  # output$prep_log <- renderPrint({
  #   if (length(rv$prep_log) == 0) {
  #     cat("No preprocessing applied.")
  #   } else {
  #     print(rv$prep_log)
  #   }
  # })

  # get_inspect_long <- reactive({
  #   req(filtered_data(), roles_ready())
  #   dat <- filtered_data()
  #   ic <- setdiff(rv$roles$active %||% character(0), rv$roles$supp %||% character(0))
  #   ic <- ic[ic %in% names(dat)[sapply(dat, is.numeric)]]
  #   validate(need(length(ic) > 0, "No numeric active variables selected."))
  #   iv <- if (has_id()) as.factor(dat[[rv$id_col]]) else as.factor("all")
  #   dat %>%
  #     mutate(dataset_id = iv, .row_id = row_number()) %>%
  #     select(dataset_id, .row_id, all_of(ic)) %>%
  #     pivot_longer(cols = all_of(ic), names_to = "variable", values_to = "value")
  # })

  # output$inspect_line_plot <- renderPlot({
  #   df <- get_inspect_long()
  #   nb <- if ((session$clientData$output_inspect_line_plot_width %||% 1200) < 700) 4 else 8
  #   p <- ggplot(df, aes(.row_id, value)) +
  #     geom_line(color = "#2C7FB8", linewidth = 0.45, alpha = 0.95) +
  #     scale_x_continuous(n.breaks = nb) +
  #     theme_minimal(base_size = 20) +
  #     theme(axis.title = element_text(size = 16), axis.text = element_text(size = 16), strip.text = element_text(size = 16)) +
  #     labs(x = "Sample order (row index)", y = "Value")
  #   if (n_distinct(df$dataset_id) > 1) p + facet_grid(dataset_id ~ variable, scales = "free_y") else p + facet_wrap(~variable, scales = "free_y", ncol = 4)
  # })

  # output$inspect_hist_plot <- renderPlot({
  #   df <- get_inspect_long()
  #   nb <- if ((session$clientData$output_inspect_hist_plot_width %||% 1200) < 700) 4 else 8
  #   p <- ggplot(df, aes(value)) +
  #     geom_histogram(bins = 30, fill = "#4C78A8", alpha = 0.85, color = "white") +
  #     scale_x_continuous(n.breaks = nb) +
  #     theme_minimal(base_size = 20) +
  #     theme(axis.title = element_text(size = 16), axis.text = element_text(size = 16), strip.text = element_text(size = 16)) +
  #     labs(x = NULL, y = "Count")
  #   if (n_distinct(df$dataset_id) > 1) p + facet_grid(dataset_id ~ variable, scales = "free") else p + facet_wrap(~variable, scales = "free", ncol = 4)
  # })

  # register_vec_lock_sync(input, session, rv, "pca", "pca_lock_sync")
  register_vec_lock_sync(input, session, rv, "lda", "lda_lock_sync")

  # pca_result <- reactive({
  #   req(roles_ready())
  #   dat <- filtered_data()
  #   make_ord_result(
  #     dat = dat, roles = rv$roles,
  #     has_id = has_id(), id_col = rv$id_col, compare_mode = input$compare_mode,
  #     method = "pca",
  #     flip1 = input$flip_pc1, flip2 = input$flip_pc2,
  #     align_mode = input$pca_align_mode, ref_id = input$pca_ref_id
  #   )
  # })

  # lda_result <- reactive({
  #   req(roles_ready())
  #   validate(need(!is.null(rv$roles$group) && nzchar(rv$roles$group), "Choose a grouping variable for LDA."))
  #   dat <- filtered_data()
  #   res <- make_ord_result(
  #     dat = dat, roles = rv$roles,
  #     has_id = has_id(), id_col = rv$id_col, compare_mode = input$compare_mode,
  #     method = "lda",
  #     flip1 = input$flip_ld1, flip2 = input$flip_ld2,
  #     align_mode = input$lda_align_mode, ref_id = input$lda_ref_id
  #   )
  #   names(res)[names(res) == "loadings"] <- "scaling"
  #   res
  # })

  # pca_projected <- reactive({
  #   req(pca_result())
  #   project_for_axes_pair(
  #     pca_result(),
  #     method = "pca",
  #     pair = input$pca_axes_pair %||% "1_2",
  #     facet_by_id = isTRUE(input$pca_facet_by_id)
  #   )
  # })

  # lda_projected <- reactive({
  #   req(lda_result())
  #   project_for_axes_pair(
  #     lda_result(),
  #     method = "lda",
  #     pair = input$lda_axes_pair %||% "1_2",
  #     facet_by_id = isTRUE(input$lda_facet_by_id)
  #   )
  # })
  #
  # make_plot_build <- function(prefix, projected, interactive = FALSE) {
  #   cfg <- get_ord_cfg(input, prefix)
  #   sc <- projected$scores
  #   multi_id <- n_distinct(sc$dataset_id) > 1
  #   group_present <- "group" %in% names(sc)
  #
  #   hu <- if (group_present) compute_hulls(sc, "group", "Axis1", "Axis2", if (multi_id) "dataset_id" else NULL) else sc[0, , drop = FALSE]
  #   ce <- if (group_present) compute_centroids(sc, "group", "Axis1", "Axis2", if (multi_id) "dataset_id" else NULL) else data.frame()
  #
  #   av <- vector_scale_df(projected$vectors, "x", "y", cfg$vec_scale_active)
  #
  #   sv_scale <- if (cfg$lock_vec_scale) cfg$vec_scale_active else cfg$vec_scale_supp
  #   sv <- projected$supp_vectors
  #   sv_present <- has_supp_rows(sv)
  #   if (sv_present) sv <- vector_scale_df(sv, "x", "y", sv_scale)
  #
  #   b <- build_ordination_plot(
  #     sc = sc, hulls = hu, cents = ce,
  #     av = av, sv = sv, sv_present = sv_present,
  #     multi_id = multi_id, cfg = cfg,
  #     interactive = interactive
  #   )
  #   b$xlab <- projected$xlab
  #   b$ylab <- projected$ylab
  #   b$xlab_plain <- projected$xlab_plain
  #   b$ylab_plain <- projected$ylab_plain
  #   b
  # }

  # pca_plot_build <- reactive({
  #   req(pca_projected())
  #   make_plot_build("pca", pca_projected(),
  #                   interactive = isTRUE(input$pca_interactive))
  # })

  # lda_plot_build <- reactive({
  #   req(lda_projected())
  #   make_plot_build("lda", lda_projected(),
  #                   interactive = isTRUE(input$lda_interactive))
  # })

  # output$pca_vector_note <- renderUI({
  #   req(pca_projected())
  #   tags$p(
  #     class = "vector-note",
  #     HTML("<span style='color:black; font-weight:600;'>Black vectors</span>: active variables &nbsp;&nbsp; <span style='color:red; font-weight:600;'>Red vectors</span>: supplementary variables")
  #   )
  # })

  # output$lda_vector_note <- renderUI({
  #   req(lda_projected())
  #   tags$p(
  #     class = "vector-note",
  #     HTML("<span style='color:black; font-weight:600;'>Black vectors</span>: active variables &nbsp;&nbsp; <span style='color:red; font-weight:600;'>Red vectors</span>: supplementary variables")
  #   )
  # })

  # output$pca_angle_controls <- renderUI({
  #   tagList(
  #     fluidRow(
  #       column(4, actionButton("pca_clear_ref", "Clear vector reference")),
  #       column(8, htmlOutput("pca_angle_info"))
  #     )
  #   )
  # })

  # output$pca_centroid_controls <- renderUI({
  #   tagList(
  #     fluidRow(
  #       column(4, actionButton("pca_clear_cent_ref", "Clear centroid reference")),
  #       column(8, htmlOutput("pca_centroid_info"))
  #     )
  #   )
  # })

  # output$lda_angle_controls <- renderUI({
  #   tagList(
  #     fluidRow(
  #       column(4, actionButton("lda_clear_ref", "Clear vector reference")),
  #       column(8, htmlOutput("lda_angle_info"))
  #     )
  #   )
  # })

  # output$lda_centroid_controls <- renderUI({
  #   tagList(
  #     fluidRow(
  #       column(4, actionButton("lda_clear_cent_ref", "Clear centroid reference")),
  #       column(8, htmlOutput("lda_centroid_info"))
  #     )
  #   )
  # })

  # observe({
  #   req(input$pca_axes_pair)
  #   ij <- parse_axes_pair(input$pca_axes_pair)
  #   updateCheckboxInput(session, "flip_pc1", label = paste0("Flip PC", ij[1], " axis"))
  #   updateCheckboxInput(session, "flip_pc2", label = paste0("Flip PC", ij[2], " axis"))
  # })

  # observe({
  #   req(input$lda_axes_pair)
  #   ij <- parse_axes_pair(input$lda_axes_pair)
  #   updateCheckboxInput(session, "flip_ld1", label = paste0("Flip LD", ij[1], " axis"))
  #   updateCheckboxInput(session, "flip_ld2", label = paste0("Flip LD", ij[2], " axis"))
  # })

  # output$pca_tbl <- renderDT({
  #   datatable(
  #     pca_result()$eigen %>%
  #       rename(`explained variance %` = explained_var_pct, `broken-stick %` = broken_stick_pct)
  #   )
  # })
  #
  # output$scree_plot <- renderPlot({
  #   df <- pca_result()$eigen %>% mutate(PCn = as.integer(gsub("PC", "", PC)))
  #   ggplot(df, aes(PCn, explained_var_pct, fill = dataset_id)) +
  #     geom_col(position = position_dodge(0.8), alpha = 0.85) +
  #     geom_line(aes(y = broken_stick_pct, color = dataset_id, group = dataset_id), linewidth = 1) +
  #     geom_point(aes(y = broken_stick_pct, color = dataset_id), size = 2) +
  #     scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  #     labs(x = "Principal component", y = "Explained variance %", fill = "ID", color = "ID") +
  #     pub_theme(18)
  # })
  #
  # output$pca_plot_container <- renderUI({
  #   if (isTRUE(input$pca_interactive)) plotlyOutput("pca_ord_plotly", height = 700) else plotOutput("pca_ord_plot", height = 700)
  # })
  #
  # output$pca_ord_plot <- renderPlot({
  #   req(!isTRUE(input$pca_interactive))
  #   bld <- pca_plot_build()
  #   mi <- n_distinct(pca_projected()$scores$dataset_id) > 1
  #   flips <- get_flip_flags_for_pair(input$pca_axes_pair %||% "1_2", input$flip_pc1, input$flip_pc2)
  #   finish_ord_plot(bld$plot, bld$xlab, bld$ylab, mi, input$pca_facet_by_id, FALSE, flip_x = flips$flip_x, flip_y = flips$flip_y)
  # })

  # output$pca_ord_plotly <- renderPlotly({
  #   req(isTRUE(input$pca_interactive))
  #   bld <- pca_plot_build()
  #   mi <- n_distinct(pca_projected()$scores$dataset_id) > 1
  #   flips <- get_flip_flags_for_pair(input$pca_axes_pair %||% "1_2", input$flip_pc1, input$flip_pc2)
  #   faceted <- isTRUE(input$pca_facet_by_id) && isTRUE(mi)
  #
  #   p <- bld$plot
  #   if (!is.na(bld$repel_layer_index) && !is.null(bld$all_lab)) {
  #     p <- replace_repel_with_text(p, bld$repel_layer_index, bld$all_lab, bld$label_size)
  #   }
  #
  #   p <- finish_ord_plot(p, bld$xlab, bld$ylab, mi, input$pca_facet_by_id, TRUE, flip_x = flips$flip_x, flip_y = flips$flip_y)
  #   pl <- ggplotly(p, tooltip = c("x", "y"), source = "pca_ord_plotly")
  #   li <- extract_legend_info(p, pca_projected()$scores, mi)
  #
  #   if (!faceted) {
  #     pl <- clean_plotly_legend(
  #       pl,
  #       litho_levels = li$litho_levels, litho_colors = li$litho_colors,
  #       id_levels = li$id_levels, id_shapes = li$id_shapes,
  #       is_faceted = FALSE
  #     )
  #   }
  #
  #   pl <- layout(
  #     pl,
  #     xaxis = list(
  #       title = list(text = bld$xlab, font = list(size = AXIS_FONT_SIZE)),
  #       tickfont = list(size = AXIS_FONT_SIZE)
  #     ),
  #     yaxis = list(
  #       title = list(text = bld$ylab, font = list(size = AXIS_FONT_SIZE)),
  #       tickfont = list(size = AXIS_FONT_SIZE)
  #     )
  #   )
  #
  #   pca_cfg <- get_ord_cfg(input, "pca")
  #   pca_vec_df <- get_plotly_vectors(
  #     projected = pca_projected(),
  #     cfg = pca_cfg,
  #     multi_id = mi,
  #     include_id_suffix = isTRUE(pca_cfg$id_in_vector_labels)
  #   )
  #
  #   if (isTRUE(flips$flip_x) && "xend" %in% names(pca_vec_df)) {
  #     pca_vec_df$xend <- -pca_vec_df$xend
  #   }
  #   if (isTRUE(flips$flip_y) && "yend" %in% names(pca_vec_df)) {
  #     pca_vec_df$yend <- -pca_vec_df$yend
  #   }
  #   if (!isTRUE(faceted)) {
  #     pl <- add_plotly_vector_traces(
  #       pl,
  #       vectors_df = pca_vec_df,
  #       source_id = "pca_ord_plotly",
  #       ref_uid = angle_rv$pca_ref,
  #       hover_uid = angle_rv$pca_hover,
  #       xlab_plain = bld$xlab_plain,
  #       ylab_plain = bld$ylab_plain
  #     )
  #     pl <- add_plotly_centroid_traces(
  #       pl,
  #       centroids_df = pca_centroids_current(),
  #       source_id = "pca_ord_plotly",
  #       group_colors = li$litho_colors,
  #       id_shapes = li$id_shapes,
  #       ref_uid = centroid_rv$pca_ref,
  #       hover_uid = centroid_rv$pca_hover,
  #       base_size = max(10, pca_cfg$centroid_size * 2.8)
  #     )
  #   }
  #   pl <- event_register(pl, "plotly_click")
  #   pl <- event_register(pl, "plotly_hover")
  #
  #   pl
  # })

  # output$lda_info <- renderPrint({
  #   res <- lda_result()
  #   if (isTRUE(res$split_mode)) cat("LDA split by ID and overlaid.\n") else print(res$fit)
  # })

  # output$lda_tbl <- renderDT({
  #   datatable(head(lda_projected()$scores, 300))
  # })
  #
  # output$lda_plot_container <- renderUI({
  #   if (isTRUE(input$lda_interactive)) plotlyOutput("lda_ord_plotly", height = 700) else plotOutput("lda_ord_plot", height = 700)
  # })

  # output$lda_ord_plot <- renderPlot({
  #   req(!isTRUE(input$lda_interactive))
  #   bld <- lda_plot_build()
  #   mi <- n_distinct(lda_projected()$scores$dataset_id) > 1
  #   flips <- get_flip_flags_for_pair(input$lda_axes_pair %||% "1_2", input$flip_ld1, input$flip_ld2)
  #   finish_ord_plot(bld$plot, bld$xlab, bld$ylab, mi, input$lda_facet_by_id, FALSE, flip_x = flips$flip_x, flip_y = flips$flip_y)
  # })
  #
  # output$lda_ord_plotly <- renderPlotly({
  #   req(isTRUE(input$lda_interactive))
  #   bld <- lda_plot_build()
  #   mi <- n_distinct(lda_projected()$scores$dataset_id) > 1
  #   flips <- get_flip_flags_for_pair(input$lda_axes_pair %||% "1_2", input$flip_ld1, input$flip_ld2)
  #   faceted <- isTRUE(input$lda_facet_by_id) && isTRUE(mi)
  #
  #   p <- bld$plot
  #   if (!is.na(bld$repel_layer_index) && !is.null(bld$all_lab)) {
  #     p <- replace_repel_with_text(p, bld$repel_layer_index, bld$all_lab, bld$label_size)
  #   }
  #
  #   p <- finish_ord_plot(p, bld$xlab, bld$ylab, mi, input$lda_facet_by_id, TRUE, flip_x = flips$flip_x, flip_y = flips$flip_y)
  #   pl <- ggplotly(p, tooltip = c("x", "y"), source = "lda_ord_plotly")
  #   li <- extract_legend_info(p, lda_projected()$scores, mi)
  #
  #   if (!faceted) {
  #     pl <- clean_plotly_legend(
  #       pl,
  #       litho_levels = li$litho_levels, litho_colors = li$litho_colors,
  #       id_levels = li$id_levels, id_shapes = li$id_shapes,
  #       is_faceted = FALSE
  #     )
  #   }
  #
  #   pl <- layout(
  #     pl,
  #     xaxis = list(
  #       title = list(text = bld$xlab, font = list(size = AXIS_FONT_SIZE)),
  #       tickfont = list(size = AXIS_FONT_SIZE)
  #     ),
  #     yaxis = list(
  #       title = list(text = bld$ylab, font = list(size = AXIS_FONT_SIZE)),
  #       tickfont = list(size = AXIS_FONT_SIZE)
  #     )
  #   )
  #
  #   lda_cfg <- get_ord_cfg(input, "lda")
  #   lda_vec_df <- get_plotly_vectors(
  #     projected = lda_projected(),
  #     cfg = lda_cfg,
  #     multi_id = mi,
  #     include_id_suffix = isTRUE(lda_cfg$id_in_vector_labels)
  #   )
  #
  #   if (isTRUE(flips$flip_x) && "xend" %in% names(lda_vec_df)) {
  #     lda_vec_df$xend <- -lda_vec_df$xend
  #   }
  #   if (isTRUE(flips$flip_y) && "yend" %in% names(lda_vec_df)) {
  #     lda_vec_df$yend <- -lda_vec_df$yend
  #   }
  #   if (!isTRUE(faceted)) {
  #     pl <- add_plotly_vector_traces(
  #       pl,
  #       vectors_df = lda_vec_df,
  #       source_id = "lda_ord_plotly",
  #       ref_uid = angle_rv$lda_ref,
  #       hover_uid = angle_rv$lda_hover,
  #       xlab_plain = bld$xlab_plain,
  #       ylab_plain = bld$ylab_plain
  #     )
  #     pl <- add_plotly_centroid_traces(
  #       pl,
  #       centroids_df = lda_centroids_current(),
  #       source_id = "lda_ord_plotly",
  #       group_colors = li$litho_colors,
  #       id_shapes = li$id_shapes,
  #       ref_uid = centroid_rv$lda_ref,
  #       hover_uid = centroid_rv$lda_hover,
  #       base_size = max(10, lda_cfg$centroid_size * 2.8)
  #     )
  #   }
  #   pl <- event_register(pl, "plotly_click")
  #   pl <- event_register(pl, "plotly_hover")
  #
  #   pl
  # })

  # output$download_pca_tbl <- downloadHandler(
  #   filename = function() "pca_eigen_table.csv",
  #   content = function(file) {
  #     write_csv(
  #       pca_result()$eigen %>%
  #         rename(`explained variance %` = explained_var_pct, `broken-stick %` = broken_stick_pct),
  #       file
  #     )
  #   }
  # )

  # output$download_lda_tbl <- downloadHandler(
  #   filename = function() "lda_scores.csv",
  #   content = function(file) {
  #     write_csv(lda_projected()$scores, file)
  #   }
  # )

  # pca_all_current <- reactive({
  #   req(pca_projected())
  #   cfg <- get_ord_cfg(input, "pca")
  #   mi <- n_distinct(pca_projected()$scores$dataset_id) > 1
  #   vv <- get_plotly_vectors(pca_projected(), cfg, mi, include_id_suffix = isTRUE(cfg$id_in_vector_labels))
  #   merge_vectors_with_axes(vv, pca_projected()$xlab_plain, pca_projected()$ylab_plain)
  # })
  #
  # pca_centroids_current <- reactive({
  #   req(pca_projected())
  #   flips <- get_flip_flags_for_pair(input$pca_axes_pair %||% "1_2", input$flip_pc1, input$flip_pc2)
  #   get_plotly_centroids(pca_projected(), flip_x = flips$flip_x, flip_y = flips$flip_y)
  # })

  # lda_all_current <- reactive({
  #   req(lda_projected())
  #   cfg <- get_ord_cfg(input, "lda")
  #   mi <- n_distinct(lda_projected()$scores$dataset_id) > 1
  #   vv <- get_plotly_vectors(lda_projected(), cfg, mi, include_id_suffix = isTRUE(cfg$id_in_vector_labels))
  #   merge_vectors_with_axes(vv, lda_projected()$xlab_plain, lda_projected()$ylab_plain)
  # })
  #
  # lda_centroids_current <- reactive({
  #   req(lda_projected())
  #   flips <- get_flip_flags_for_pair(input$lda_axes_pair %||% "1_2", input$flip_ld1, input$flip_ld2)
  #   get_plotly_centroids(lda_projected(), flip_x = flips$flip_x, flip_y = flips$flip_y)
  # })

  # observeEvent(input$pca_axes_pair, {
  #   angle_rv$pca_ref <- NULL
  #   angle_rv$pca_hover <- NULL
  #   centroid_rv$pca_ref <- NULL
  #   centroid_rv$pca_hover <- NULL
  # }, ignoreInit = TRUE)

  # observeEvent(input$lda_axes_pair, {
  #   angle_rv$lda_ref <- NULL
  #   angle_rv$lda_hover <- NULL
  #   centroid_rv$lda_ref <- NULL
  #   centroid_rv$lda_hover <- NULL
  # }, ignoreInit = TRUE)

  # observeEvent(input$pca_clear_ref, {
  #   angle_rv$pca_ref <- NULL
  #   angle_rv$pca_hover <- NULL
  # }, ignoreInit = TRUE)
  #
  # observeEvent(input$lda_clear_ref, {
  #   angle_rv$lda_ref <- NULL
  #   angle_rv$lda_hover <- NULL
  # }, ignoreInit = TRUE)

  # observeEvent(input$pca_clear_cent_ref, {
  #   centroid_rv$pca_ref <- NULL
  #   centroid_rv$pca_hover <- NULL
  # }, ignoreInit = TRUE)

  # observeEvent(input$lda_clear_cent_ref, {
  #   centroid_rv$lda_ref <- NULL
  #   centroid_rv$lda_hover <- NULL
  # }, ignoreInit = TRUE)

  # observeEvent(event_data("plotly_click", source = "pca_ord_plotly"), {
  #   if (!isTRUE(input$pca_interactive)) return()
  #   ed <- event_data("plotly_click", source = "pca_ord_plotly")
  #   if (is.null(ed)) return()
  #   k <- ed$key %||% ed$customdata
  #   if (is.null(k) || !nzchar(as.character(k))) return()
  #   k <- as.character(k)[1]
  #   vv <- pca_all_current()
  #   if (nrow(vv) > 0 && k %in% vv$uid) {
  #     if (is.null(angle_rv$pca_ref)) {
  #       angle_rv$pca_ref <- k
  #     } else {
  #       angle_rv$pca_hover <- k
  #     }
  #   }
  #   ce <- pca_centroids_current()
  #   if (nrow(ce) > 0 && k %in% ce$uid) {
  #     if (is.null(centroid_rv$pca_ref)) {
  #       centroid_rv$pca_ref <- k
  #       centroid_rv$pca_hover <- NULL
  #     } else if (identical(k, centroid_rv$pca_ref)) {
  #       centroid_rv$pca_ref <- NULL
  #       centroid_rv$pca_hover <- NULL
  #     } else {
  #       centroid_rv$pca_hover <- k
  #     }
  #   }
  # }, ignoreInit = TRUE)
  #
  # observeEvent(event_data("plotly_click", source = "lda_ord_plotly"), {
  #   if (!isTRUE(input$lda_interactive)) return()
  #   ed <- event_data("plotly_click", source = "lda_ord_plotly")
  #   if (is.null(ed)) return()
  #   k <- ed$key %||% ed$customdata
  #   if (is.null(k) || !nzchar(as.character(k))) return()
  #   k <- as.character(k)[1]
  #   vv <- lda_all_current()
  #   if (nrow(vv) > 0 && k %in% vv$uid) {
  #     if (is.null(angle_rv$lda_ref)) {
  #       angle_rv$lda_ref <- k
  #     } else {
  #       angle_rv$lda_hover <- k
  #     }
  #   }
  #   ce <- lda_centroids_current()
  #   if (nrow(ce) > 0 && k %in% ce$uid) {
  #     if (is.null(centroid_rv$lda_ref)) {
  #       centroid_rv$lda_ref <- k
  #       centroid_rv$lda_hover <- NULL
  #     } else if (identical(k, centroid_rv$lda_ref)) {
  #       centroid_rv$lda_ref <- NULL
  #       centroid_rv$lda_hover <- NULL
  #     } else {
  #       centroid_rv$lda_hover <- k
  #     }
  #   }
  # }, ignoreInit = TRUE)

  # output$pca_angle_info <- renderUI({
  #   if (!isTRUE(input$pca_interactive)) return(HTML("<span style='color:#666;'>Angle comparison available in interactive mode.</span>"))
  #   vv <- pca_all_current()
  #   if (nrow(vv) == 0) return(HTML("<span style='color:#666;'>No vectors/axes available for angle comparison.</span>"))
  #   info <- compute_pair_metrics(vv, angle_rv$pca_ref, angle_rv$pca_hover)
  #   if (is.null(angle_rv$pca_ref)) return(HTML("<span style='color:#666;'>Click a vector or axis selector to set reference.</span>"))
  #   if (is.null(info)) {
  #     ref_nm <- vv %>% filter(uid == angle_rv$pca_ref) %>% slice(1) %>% pull(display_var)
  #     if (length(ref_nm) == 0) ref_nm <- angle_rv$pca_ref
  #     return(HTML(paste0("<b>Reference:</b> ", ref_nm, " &nbsp;|&nbsp; Click another vector/axis selector to compare.")))
  #   }
  #   HTML(paste0(
  #     "<b>Reference:</b> ", info$ref,
  #     " &nbsp;|&nbsp; <b>Compared:</b> ", info$hov,
  #     " &nbsp;|&nbsp; <b>Angle (°):</b> ", sprintf("%.2f", info$ang),
  #     " &nbsp;|&nbsp; <b>cos(θ):</b> ", sprintf("%.4f", info$cs)
  #   ))
  # })
  #
  # output$lda_angle_info <- renderUI({
  #   if (!isTRUE(input$lda_interactive)) return(HTML("<span style='color:#666;'>Angle comparison available in interactive mode.</span>"))
  #   vv <- lda_all_current()
  #   if (nrow(vv) == 0) return(HTML("<span style='color:#666;'>No vectors/axes available for angle comparison.</span>"))
  #   info <- compute_pair_metrics(vv, angle_rv$lda_ref, angle_rv$lda_hover)
  #   if (is.null(angle_rv$lda_ref)) return(HTML("<span style='color:#666;'>Click a vector or axis selector to set reference.</span>"))
  #   if (is.null(info)) {
  #     ref_nm <- vv %>% filter(uid == angle_rv$lda_ref) %>% slice(1) %>% pull(display_var)
  #     if (length(ref_nm) == 0) ref_nm <- angle_rv$lda_ref
  #     return(HTML(paste0("<b>Reference:</b> ", ref_nm, " &nbsp;|&nbsp; Click another vector/axis selector to compare.")))
  #   }
  #   HTML(paste0(
  #     "<b>Reference:</b> ", info$ref,
  #     " &nbsp;|&nbsp; <b>Compared:</b> ", info$hov,
  #     " &nbsp;|&nbsp; <b>Angle (°):</b> ", sprintf("%.2f", info$ang),
  #     " &nbsp;|&nbsp; <b>cos(θ):</b> ", sprintf("%.4f", info$cs)
  #   ))
  # })

  # output$pca_centroid_info <- renderUI({
  #   if (!isTRUE(input$pca_interactive)) return(HTML("<span style='color:#666;'>Centroid comparison available in interactive mode.</span>"))
  #   if (isTRUE(input$pca_facet_by_id) && n_distinct(pca_projected()$scores$dataset_id) > 1) return(HTML("<span style='color:#666;'>Centroid comparison is shown in interactive non-faceted mode.</span>"))
  #   ce <- pca_centroids_current()
  #   if (nrow(ce) == 0) return(HTML("<span style='color:#666;'>No centroids available for comparison.</span>"))
  #   info <- compute_centroid_pair_metrics(ce, centroid_rv$pca_ref, centroid_rv$pca_hover)
  #   if (is.null(centroid_rv$pca_ref)) return(HTML("<span style='color:#666;'>Click a centroid to set reference.</span>"))
  #   if (is.null(info)) {
  #     ref_nm <- ce %>% filter(uid == centroid_rv$pca_ref) %>% slice(1) %>% pull(label)
  #     if (length(ref_nm) == 0) ref_nm <- centroid_rv$pca_ref
  #     return(HTML(paste0("<b>Reference centroid:</b> ", ref_nm, " &nbsp;|&nbsp; Click another centroid to compare.")))
  #   }
  #   HTML(paste0(
  #     "<b>Reference centroid:</b> ", info$ref,
  #     " &nbsp;|&nbsp; <b>Compared centroid:</b> ", info$hov,
  #     " &nbsp;|&nbsp; <b>Euclidean distance:</b> ", sprintf("%.3f", info$dist)
  #   ))
  # })

  # output$lda_centroid_info <- renderUI({
  #   if (!isTRUE(input$lda_interactive)) return(HTML("<span style='color:#666;'>Centroid comparison available in interactive mode.</span>"))
  #   if (isTRUE(input$lda_facet_by_id) && n_distinct(lda_projected()$scores$dataset_id) > 1) return(HTML("<span style='color:#666;'>Centroid comparison is shown in interactive non-faceted mode.</span>"))
  #   ce <- lda_centroids_current()
  #   if (nrow(ce) == 0) return(HTML("<span style='color:#666;'>No centroids available for comparison.</span>"))
  #   info <- compute_centroid_pair_metrics(ce, centroid_rv$lda_ref, centroid_rv$lda_hover)
  #   if (is.null(centroid_rv$lda_ref)) return(HTML("<span style='color:#666;'>Click a centroid to set reference.</span>"))
  #   if (is.null(info)) {
  #     ref_nm <- ce %>% filter(uid == centroid_rv$lda_ref) %>% slice(1) %>% pull(label)
  #     if (length(ref_nm) == 0) ref_nm <- centroid_rv$lda_ref
  #     return(HTML(paste0("<b>Reference centroid:</b> ", ref_nm, " &nbsp;|&nbsp; Click another centroid to compare.")))
  #   }
  #   HTML(paste0(
  #     "<b>Reference centroid:</b> ", info$ref,
  #     " &nbsp;|&nbsp; <b>Compared centroid:</b> ", info$hov,
  #     " &nbsp;|&nbsp; <b>Euclidean distance:</b> ", sprintf("%.3f", info$dist)
  #   ))
  # })
}

shinyApp(ui, server)
