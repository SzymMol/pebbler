pca_server <- function(id, rv) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      roles_ready <- reactive(isTRUE(rv$roles_applied))
      in_split_mode <- reactiveVal()
      pca_result <- reactiveVal()
      group_colname <- reactiveVal()
      id_colname <- reactiveVal()


      output$pca_ord_top <- renderUI({
        if (roles_ready()) {
          if (rv$roles_update_pca) {
            # clear previous results
            pca_result(NULL)
            rv$roles_update_pca <- FALSE
          }
          ord_top_controls(id, "pca", "PCA")
        }
        else {
          h3("Please select data")
        }
      })

      observeEvent(input$compute_ord, {
        req(roles_ready())
        if (is.null(rv$data)) {
          dat <- rv$filtered_data
          showNotification("PCA running on unprocessed data",
                           type = "warning", duration = 4)
        }
        else {
          dat <- rv$data
        }
        nr <- if (!is.null(rv$id_col)) length(unique(rv$filtered_data[[rv$id_col]])) else 1
        rows <- paste0("R", 1:nr)
        cols <- paste0("C", 1:length(rv$roles$active))
        flip_axs_vals <- outer(rows, cols, Vectorize(function(r, c) {
          isTRUE(input[[paste(r, c, sep = "_")]])
          }))
        if (!is.null(rv$id_col)) {
          rownames(flip_axs_vals) <- unique(rv$filtered_data[[rv$id_col]])
        }
        else {
          rownames(flip_axs_vals) <- "all"
        }
        colnames(flip_axs_vals) <- paste0("PC", 1:length(rv$roles$active))
        # print(flip_axs_vals)
        in_split_mode(!is.null(rv$cmp_mode) && rv$cmp_mode == 'split')
        pca_result(make_ord_result(
          dat = dat, roles = rv$roles,
          has_id = in_split_mode(),
          id_col = rv$id_col, compare_mode = rv$cmp_mode,
          method = "pca",
          flip_axs_vals = flip_axs_vals,
          align_mode = input$pca_align_mode, ref_id = input$pca_ref_id
        ))
        group_colname(rv$roles$group)
        id_colname(rv$id_col)
        showNotification("PCA results computed", type = "message", duration = 4)
      })

      output$flip_axs_grid <- renderUI({
        req(roles_ready())
        if (!is.null(rv$id_col)) {
          dataset_names <- unique(rv$filtered_data[[rv$id_col]])
        }
        else {
          dataset_names <- c("all")
        }
        nr <- length(dataset_names)
        # nr <- if (!is.null(rv$id_col)) length(unique(rv$filtered_data[[rv$id_col]])) else 1
        # rows <- paste0("R", 1:nr)
        cols <- paste0("C", 1:length(rv$roles$active))
        # tags$head(
        #   tags$style(HTML("
        #   .checkbox-matrix .shiny-input-container {
        #     width: auto;
        #     margin-bottom: 0;
        #   }
        #
        #   .checkbox-matrix td {
        #     padding: 2px;
        #   }
        # "))
        # )
        tags$table(
          class = "table table-bordered table-sm",
          # class = "checkbox-matrix",
          tags$thead(
            tags$tr(
              tags$th("Flip PCA eigenvectors"),
              lapply(paste0("PC", 1:length(cols)), tags$th)
            )
          ),
          tags$tbody(
            lapply(1:nr, function(r) {
              tags$tr(
                tags$th(paste("dataset", dataset_names[r])),
                lapply(cols, function(c) {
                  id <- paste(paste0("R", r), c, sep = "_")
                  tags$td(
                    # checkboxInput(inputId = ns(id), label = NULL, value = FALSE)
                    tags$input(
                      id = ns(id),
                      type = "checkbox"
                    )
                    )
                })
              )
            })
          )
        )
      })

      # output$download_pca_tbl <- downloadHandler(
      #   filename = function() "pca_eigen_table.csv",
      #   content = function(file) {
      #     req(pca_result())
      #     write_csv(
      #       pca_result()$eigen %>%
      #         rename(`explained variance %` = explained_var_pct, `broken-stick %` = broken_stick_pct),
      #       file
      #     )
      #   }
      # )

      angle_rv <- reactiveValues(pca_ref = NULL, pca_hover = NULL, lda_ref = NULL, lda_hover = NULL)
      centroid_rv <- reactiveValues(pca_ref = NULL, pca_hover = NULL, lda_ref = NULL, lda_hover = NULL)

      observeEvent(input$pca_axes_pair, {
        angle_rv$pca_ref <- NULL
        angle_rv$pca_hover <- NULL
        centroid_rv$pca_ref <- NULL
        centroid_rv$pca_hover <- NULL
      }, ignoreInit = TRUE)

      output$pca_comparison_heading <- renderUI({
        req(pca_result())
        if (!is.null(in_split_mode()) && in_split_mode()) {
          tags$h4("Comparison mode ON")
        }
        else {
          tags$h4("Comparison mode OFF (no variability in the dataset identifier column)")
        }
      })

      # output$pca_align_mode_ui <- renderUI({
      #   req(rv$filtered_data)
      #   if (is.null(in_split_mode()) || !in_split_mode()) return(NULL)
      #   selectInput(
      #     ns("pca_align_mode"), "How to align separate section plots",
      #     choices = c("None (raw orientation)" = "none", "Auto-flip axes to match reference section" = "sign"),
      #     selected = "none"
      #   )
      # })
      #
      # output$pca_ref_id_ui <- renderUI({
      #   req(rv$filtered_data)
      #   if (is.null(in_split_mode()) || !in_split_mode()) return(NULL)
      #   ids <- unique(as.character(rv$filtered_data[[rv$id_col]]))
      #   selectInput(ns("pca_ref_id"), "Reference section for alignment", choices = ids, selected = ids[1])
      # })

      output$pca_facet_ui <- renderUI({
        # req(rv$filtered_data)
        # if (is.null(in_split_mode()) || !in_split_mode()) return(NULL)
        if (!is.null(in_split_mode()) && in_split_mode() && !isTRUE(input$pca_interactive)) {
          checkboxInput(ns("pca_facet_by_id"), "Show each dataset in a separate panel (static mode only)", FALSE)
        }
        else {
          # updateCheckboxInput(session, "pca_facet_by_id", value = FALSE)
          # print(input$pca_facet_by_id)
          return(NULL)
        }
      })

      observeEvent(input$pca_interactive, {
        if(isTRUE(input$pca_interactive)) {
          updateCheckboxInput(session, "pca_facet_by_id", value = FALSE)
        }
      }, ignoreInit = TRUE)

      output$axes_controls <- renderUI({
        req(pca_result())
        # n <- length(colnames(pca_result()$loadings))
        # axis_choices <- setNames(1:n, paste0("PC", 1:n))
        if (in_split_mode()) {
          axis_choices <- colnames(pca_result()$eig_vectors)[c(-1, -2)]
        }
        else {
          axis_choices <- colnames(pca_result()$eig_vectors)[-1]
        }
        scale_choices <- c('Raw eigenvectors and PCA scores' = 'raw', 'Loadings and standardized PCA scores' = 'loadings')
        fluidRow(
          column(4, selectInput(ns("plt_axis_x"), "Variable on x axis", choices = axis_choices, selected = axis_choices[1])),
          column(4, selectInput(ns("plt_axis_y"), "Variable on y axis", choices = axis_choices, selected = axis_choices[2])),
          column(4, selectInput(ns("plt_scaling"), "Vectors and points scaling", choices = scale_choices, selected = 'raw')),
          column(4, checkboxInput(ns("pca_interactive"), "Interactive plot mode (measure & compare)", FALSE)),
          column(4, uiOutput(ns("pca_facet_ui")))
        )
      })

      output$pca_plot_container <- renderUI({
        req(pca_result())
        if (isTRUE(input$pca_interactive)) {
          tagList(
            plotlyOutput(ns("pca_ord_plotly"), height = 700),
            fluidRow(
              column(8, div(style = "padding-top: 2px;", uiOutput(ns("pca_angle_controls")), uiOutput(ns("pca_centroid_controls")))),
              column(4, htmlOutput(ns("pca_vector_note")))
            )
          )
        }
        else {
          tagList(
            plotOutput(ns("pca_ord_plot"), height = 700),
            fluidRow(
              column(4, selectInput(ns("download_biplot_ext"), "Save as:",
                         choices = c("png", "jpeg", "tiff", "png", "pdf", "svg"),
                         selected = 1)),
              column(4,
                     div(
                       class="form-group shiny-input-container",
                       shiny::tags$label("\u00a0", class="control-label"),
                       div(downloadButton(ns("download_biplot"), "Export PCA biplot")))
                     ),
              column(4, htmlOutput(ns("pca_vector_note")))
            )
          )
        }
      })

      output$pca_ord_plotly <- renderPlotly({
        req(isTRUE(input$pca_interactive))

        # if (!isTRUE(input$pca_interactive)) {
        #   pl <- ggplotly(tooltip = c("x", "y"), source = "pca_ord_plotly")
        #   pl <- event_register(pl, "plotly_click")
        #   return(pl)
        # }

        AXIS_FONT_SIZE <- 16

        bld <- pca_plot_build()
        # mi <- n_distinct(pca_projected()$scores$dataset_id) > 1
        mi <- in_split_mode()
        flips <- get_flip_flags_for_pair(input$pca_axes_pair %||% "1_2", input$flip_pc1, input$flip_pc2)
        faceted <- isTRUE(input$pca_facet_by_id) && isTRUE(mi) && !input$pca_interactive

        p <- bld$plot
        if (!is.na(bld$repel_layer_index) && !is.null(bld$all_lab)) {
          p <- replace_repel_with_text(p, bld$repel_layer_index, bld$all_lab, bld$label_size)
        }

        p <- finish_ord_plot(p, bld$xlab, bld$ylab, mi, faceted, TRUE, flip_x = flips$flip_x, flip_y = flips$flip_y)
        pl <- ggplotly(p, tooltip = c("x", "y"), source = "pca_ord_plotly")
        li <- extract_legend_info(p, pca_projected()$scores, group_colname(), mi)
        # print(li)

        if (!faceted) {
          pl <- clean_plotly_legend(
            pl, group_colname = group_colname(), id_colname = id_colname(),
            litho_levels = li$litho_levels, litho_colors = li$litho_colors,
            id_levels = li$id_levels, id_shapes = li$id_shapes,
            is_faceted = FALSE
          )
        }

        pl <- layout(
          pl,
          xaxis = list(
            title = list(text = bld$xlab, font = list(size = AXIS_FONT_SIZE)),
            tickfont = list(size = AXIS_FONT_SIZE)
          ),
          yaxis = list(
            title = list(text = bld$ylab, font = list(size = AXIS_FONT_SIZE)),
            tickfont = list(size = AXIS_FONT_SIZE)
          )
        )

        pca_cfg <- get_ord_cfg(input, "pca")
        pca_vec_df <- get_plotly_vectors(
          projected = pca_projected(),
          cfg = pca_cfg,
          multi_id = mi,
          include_id_suffix = isTRUE(pca_cfg$id_in_vector_labels)
        )

        pca_all_current(merge_vectors_with_axes(pca_vec_df, pca_projected()$xlab_plain, pca_projected()$ylab_plain))

        if (isTRUE(flips$flip_x) && "xend" %in% names(pca_vec_df)) {
          pca_vec_df$xend <- -pca_vec_df$xend
        }
        if (isTRUE(flips$flip_y) && "yend" %in% names(pca_vec_df)) {
          pca_vec_df$yend <- -pca_vec_df$yend
        }

        if (!isTRUE(faceted)) {
          pl <- add_plotly_vector_traces(
            pl,
            vectors_df = pca_vec_df,
            source_id = "pca_ord_plotly",
            ref_uid = angle_rv$pca_ref,
            hover_uid = angle_rv$pca_hover,
            xlab_plain = bld$xlab_plain,
            ylab_plain = bld$ylab_plain
          )
          pca_centroids_current(get_plotly_centroids(pca_projected(),
                                                     group_colname = group_colname(),
                                                     show_centroids = pca_cfg$show_centroids,
                                                     flip_x = flips$flip_x,
                                                     flip_y = flips$flip_y))
          pl <- add_plotly_centroid_traces(
            pl,
            centroids_df = pca_centroids_current(),
            source_id = "pca_ord_plotly",
            group_colors = li$litho_colors,
            id_shapes = li$id_shapes,
            ref_uid = centroid_rv$pca_ref,
            hover_uid = centroid_rv$pca_hover,
            base_size = max(10, pca_cfg$centroid_size * 2.8)
          )
        }
        pl <- event_register(pl, "plotly_click")
        pl <- event_register(pl, "plotly_hover")

        pl
      })

      pca_projected <- reactiveVal()
      # ({
      #   req(pca_result())
      #   project_for_axes_pair(
      #     pca_result(),
      #     method = "pca",
      #     pair = input$pca_axes_pair %||% "1_2",
      #     facet_by_id = isTRUE(input$pca_facet_by_id)
      #   )
      # })

      pca_ord_plot_base <- reactiveVal()

      output$pca_ord_plot <- renderPlot({
        req(pca_result())
        req(!isTRUE(input$pca_interactive))
        bld <- pca_plot_build()
        # mi <- n_distinct(pca_result()$scores$dataset_id) > 1 # chyba mozna uproscic
        mi <- in_split_mode()
        flips <- get_flip_flags_for_pair(input$pca_axes_pair %||% "1_2", input$flip_pc1, input$flip_pc2)
        pca_ord_plot_base(finish_ord_plot(bld$plot, bld$xlab, bld$ylab, mi, input$pca_facet_by_id, FALSE,
                                          flip_x = flips$flip_x, flip_y = flips$flip_y,
                                          group_name = group_colname(), id_name = id_colname()))
        pca_ord_plot_base()
      })

      pca_plot_build <- reactive({
        req(pca_result())
        # print(pca_result())
        if (input$plt_scaling == 'raw') {
          sc_key = 'scores'
          vec_key = 'eig_vectors'
        }
        else {
          sc_key = 'scores_std'
          vec_key = 'loadings'
        }
        pca_projected(project_for_axes_pair(
          pca_result(),
          sc_key,
          vec_key,
          method = "pca",
          # pair = input$pca_axes_pair %||% "1_2",
          # pair_x = as.integer(input$plt_axis_x), # mozna prosciej to robic przez same nazwy kolumn
          # pair_y = as.integer(input$plt_axis_y),
          pair_x = input$plt_axis_x,
          pair_y = input$plt_axis_y,
          facet_by_id = isTRUE(input$pca_facet_by_id)
        ))
        # if (ncol(pca_result()$scores) >= ncol(pca_result()$loadings)) {
        #   group_colname <- tail(colnames(pca_result()$scores), 1)
        # }
        # else {
        #   group_colname <- NULL
        # }
        make_plot_build("pca", pca_projected(), group_colname(),
                        interactive = isTRUE(input$pca_interactive))
      })

      make_plot_build <- function(prefix, projected, group_colname, interactive = FALSE) {
        cfg <- get_ord_cfg(input, prefix)
        sc <- projected$scores
        multi_id <- n_distinct(sc$dataset_id) > 1
        # group_present <- group_colname %in% names(sc)
        group_present <- !is.null(group_colname)

        hu <- if (group_present) compute_hulls(sc, group_colname, "Axis1", "Axis2", if (multi_id) "dataset_id" else NULL) else sc[0, , drop = FALSE]
        ce <- if (group_present) compute_centroids(sc, group_colname, "Axis1", "Axis2", if (multi_id) "dataset_id" else NULL) else data.frame()
        av <- vector_scale_df(projected$vectors, "x", "y", cfg$vec_scale_active)
        # print(sc)
        # print(hu)
        # print(ce)
        # print(av)
        sv_scale <- if (cfg$lock_vec_scale) cfg$vec_scale_active else cfg$vec_scale_supp
        sv <- projected$supp_vectors
        sv_present <- has_supp_rows(sv)
        if (sv_present) sv <- vector_scale_df(sv, "x", "y", sv_scale)
        # print(sv)
        b <- build_ordination_plot(
          sc = sc, hulls = hu, cents = ce,
          av = av, sv = sv,
          group_colname = group_colname,
          multi_id = multi_id, cfg = cfg,
          interactive = interactive
        )
        if (cfg$equal_aspect) {
          b$plot <- b$plot + coord_fixed(ratio = 1)
        }
        b$xlab <- projected$xlab
        b$ylab <- projected$ylab
        b$xlab_plain <- projected$xlab_plain
        b$ylab_plain <- projected$ylab_plain
        b
      }

      output$pca_angle_controls <- renderUI({
        tagList(
          fluidRow(
            column(4, actionButton(ns("pca_clear_ref"), "Clear vector reference")),
            column(8, htmlOutput(ns("pca_angle_info")))
          )
        )
      })

      observeEvent(input$pca_clear_ref, {
        angle_rv$pca_ref <- NULL
        angle_rv$pca_hover <- NULL
      }, ignoreInit = TRUE)

      output$pca_angle_info <- renderUI({
        req(pca_all_current())
        if (!isTRUE(input$pca_interactive)) return(HTML("<span style='color:#666;'>Angle comparison available in interactive mode.</span>"))
        vv <- pca_all_current()
        if (nrow(vv) == 0) return(HTML("<span style='color:#666;'>No vectors/axes available for angle comparison.</span>"))
        info <- compute_pair_metrics(vv, angle_rv$pca_ref, angle_rv$pca_hover)
        if (is.null(angle_rv$pca_ref)) return(HTML("<span style='color:#666;'>Click a vector or axis selector to set reference.</span>"))
        if (is.null(info)) {
          ref_nm <- vv %>% filter(uid == angle_rv$pca_ref) %>% slice(1) %>% pull(display_var)
          if (length(ref_nm) == 0) ref_nm <- angle_rv$pca_ref
          return(HTML(paste0("<b>Reference:</b> ", ref_nm, " &nbsp;|&nbsp; Click another vector/axis selector to compare.")))
        }
        HTML(paste0(
          "<b>Reference vector:</b> ", info$ref,
          " &nbsp;|&nbsp; <b>Compared vector:</b> ", info$hov,
          " &nbsp;|&nbsp; <b>θ [°] (angle on ", input$plt_axis_x, "-", input$plt_axis_y,
          " plane):</b> ", sprintf("%.2f", info$ang),
          " &nbsp;|&nbsp; <b>cos(θ):</b> ", sprintf("%.4f", info$cs)
        ))
      })

      pca_all_current <- reactiveVal()
      # ({
      #   req(pca_projected())
      #   cfg <- get_ord_cfg(input, "pca")
      #   mi <- n_distinct(pca_projected()$scores$dataset_id) > 1
      #   vv <- get_plotly_vectors(pca_projected(), cfg, mi, include_id_suffix = isTRUE(cfg$id_in_vector_labels))
      #   merge_vectors_with_axes(vv, pca_projected()$xlab_plain, pca_projected()$ylab_plain)
      # })

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

      output$pca_centroid_controls <- renderUI({
        tagList(
          fluidRow(
            column(4, actionButton(ns("pca_clear_cent_ref"), "Clear centroid reference")),
            column(8, htmlOutput(ns("pca_centroid_info")))
          )
        )
      })

      observeEvent(input$pca_clear_cent_ref, {
        centroid_rv$pca_ref <- NULL
        centroid_rv$pca_hover <- NULL
      }, ignoreInit = TRUE)

      output$pca_centroid_info <- renderUI({
        req(pca_centroids_current())
        if (!isTRUE(input$pca_interactive)) return(HTML("<span style='color:#666;'>Centroid comparison available in interactive mode.</span>"))
        if (isTRUE(input$pca_facet_by_id) && n_distinct(pca_projected()$scores$dataset_id) > 1) return(HTML("<span style='color:#666;'>Centroid comparison is shown in interactive non-faceted mode.</span>"))
        ce <- pca_centroids_current()
        if (nrow(ce) == 0) return(HTML("<span style='color:#666;'>No centroids available for comparison.</span>"))
        info <- compute_centroid_pair_metrics(ce, centroid_rv$pca_ref, centroid_rv$pca_hover)
        if (is.null(centroid_rv$pca_ref)) return(HTML("<span style='color:#666;'>Click a centroid to set reference.</span>"))
        if (is.null(info)) {
          ref_nm <- ce %>% filter(uid == centroid_rv$pca_ref) %>% slice(1) %>% pull(label)
          if (length(ref_nm) == 0) ref_nm <- centroid_rv$pca_ref
          return(HTML(paste0("<b>Reference centroid:</b> ", ref_nm, " &nbsp;|&nbsp; Click another centroid to compare.")))
        }
        HTML(paste0(
          "<b>Reference centroid:</b> ", info$ref,
          " &nbsp;|&nbsp; <b>Compared centroid:</b> ", info$hov,
          " &nbsp;|&nbsp; <b>Euclidean distance (on ", input$plt_axis_x, "-", input$plt_axis_y,
          " plane):</b> ", sprintf("%.3f", info$dist)
        ))
      })

      pca_centroids_current <- reactiveVal()
      # ({
      #   req(pca_projected())
      #   flips <- get_flip_flags_for_pair(input$pca_axes_pair %||% "1_2", input$flip_pc1, input$flip_pc2)
      #   get_plotly_centroids(pca_projected(), flip_x = flips$flip_x, flip_y = flips$flip_y)
      # })


      #
      # merge_vectors_with_axes <- function(vec_df, xlab_plain, ylab_plain) {
      #   bind_rows(vec_df, get_virtual_axis_df(xlab_plain, ylab_plain))
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

      output$download_biplot <- downloadHandler(
        filename = function() paste("pca_biplot", input$download_biplot_ext, sep = "."),
        content = function(file) {
          width_id <- paste0("output_", ns("pca_ord_plot"), "_width")
          height_id <- paste0("output_", ns("pca_ord_plot"), "_height")
          ggsave(file, plot = pca_ord_plot_base(),
                 height = session$clientData[[height_id]],
                 width = session$clientData[[width_id]],
                 units = 'mm',
                 scale = 0.35,
                 device = NULL)
        }
      )

      output$pca_vector_note <- renderUI({
        req(pca_result())
        tags$p(
          class = "vector-note",
          HTML("<span style='color:black; font-weight:600;'>Black vectors</span>: active variables <br> <span style='color:red; font-weight:600;'>Red vectors</span>: supplementary variables")
        )
      })

      register_vec_lock_sync(input, session, rv, "pca", "pca_lock_sync")


      output$scree_plot <- renderUI({
        req(pca_result())
        tagList(plotOutput(ns("scree_plot_render"), height = 320),
          fluidRow(
            column(4, selectInput(ns("download_scree_plot_ext"), "Save as:",
                                  choices = c("png", "jpeg", "tiff", "png", "pdf", "svg"),
                                  selected = 1)),
            column(4,
                   div(
                     class="form-group shiny-input-container",
                     shiny::tags$label("\u00a0", class="control-label"),
                     div(downloadButton(ns("download_scree_plot"), "Export scree plot")))
            ),
            column(4, checkboxInput(ns("pca_bkn_stck"), "Show broken stick", TRUE))
          )
        )
      })

      pca_scree_plot_base <- reactiveVal()

      output$scree_plot_render <- renderPlot({
        req(pca_result())
        pca_scree_plot_base(plot_pca_scree(pca_result(), isTRUE(input$pca_bkn_stck), id_colname()))
        pca_scree_plot_base()
      })

      output$download_scree_plot <- downloadHandler(
        filename = function() paste("pca_scree_plot", input$download_scree_plot_ext, sep = "."),
        content = function(file) {
          width_id <- paste0("output_", ns("scree_plot_render"), "_width")
          height_id <- paste0("output_", ns("scree_plot_render"), "_height")
          ggsave(file, plot = pca_scree_plot_base(),
                 height = session$clientData[[height_id]],
                 width = session$clientData[[width_id]],
                 units = 'mm',
                 scale = 0.35,
                 device = NULL)
        }
      )

      pca_num_result_export_server("pca_num_result_export", in_split_mode, pca_result)

      observeEvent(event_data("plotly_click", source = "pca_ord_plotly"), {
        if (!isTRUE(input$pca_interactive)) return()
        ed <- event_data("plotly_click", source = "pca_ord_plotly")
        if (is.null(ed)) return()
        k <- ed$key %||% ed$customdata
        if (is.null(k) || !nzchar(as.character(k))) return()
        k <- as.character(k)[1]
        vv <- pca_all_current()
        if (nrow(vv) > 0 && k %in% vv$uid) {
          if (is.null(angle_rv$pca_ref)) {
            angle_rv$pca_ref <- k
          } else {
            angle_rv$pca_hover <- k
          }
        }
        ce <- pca_centroids_current()
        if (nrow(ce) > 0 && k %in% ce$uid) {
          if (is.null(centroid_rv$pca_ref)) {
            centroid_rv$pca_ref <- k
            centroid_rv$pca_hover <- NULL
          } else if (identical(k, centroid_rv$pca_ref)) {
            centroid_rv$pca_ref <- NULL
            centroid_rv$pca_hover <- NULL
          } else {
            centroid_rv$pca_hover <- k
          }
        }
      }, ignoreInit = TRUE)


    }
  )
}
