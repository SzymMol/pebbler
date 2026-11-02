# main tab: LDA server
lda_server <- function(id, rv) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      roles_ready <- reactive(isTRUE(rv$roles_applied))
      in_split_mode <- reactiveVal()
      lda_result <- reactiveVal()
      group_colname <- reactiveVal()
      id_colname <- reactiveVal()
      lds_num <- reactiveVal()

      output$lda_ord_top <- renderUI({
        if (roles_ready()) {
          if (rv$roles_update_lda) {
            # clear previous results
            lda_result(NULL)
            rv$roles_update_lda <- FALSE
          }
          if (is.null(rv$roles$group)) {
            return(h3("Please select a grouping variable"))
          }
          if (is.null(rv$data)) {
            dat <- rv$filtered_data
          }
          else {
            dat <- rv$data
          }
          lds_num(min(nrow(unique(dat[rv$roles$group]))-1, length(rv$roles$active)))
          if (lds_num() < 2) {
            # minimum number of active variables is handled during data input
            return(h3("Please select a grouping variable with more classes"))
          }
          return(ord_top_controls(id, "lda", "LDA"))
        }
        else {
          return(h3("Please select data"))
        }
      })

      observeEvent(input$compute_ord, {
        # compute LDA
        req(roles_ready())
        if (is.null(rv$data)) {
          dat <- rv$filtered_data
          showNotification("LDA running on unprocessed data",
                           type = "warning", duration = 4)
        }
        else {
          dat <- rv$data
        }
        # prepare data format
        nr <- if (!is.null(rv$id_col)) length(unique(rv$filtered_data[[rv$id_col]])) else 1
        rows <- paste0("R", 1:nr)
        cols <- paste0("C", 1:lds_num())
        flip_axs_vals <- outer(rows, cols, Vectorize(function(r, c) {
          isTRUE(input[[paste(r, c, sep = "_")]])
          }))
        if (!is.null(rv$id_col)) {
          rownames(flip_axs_vals) <- unique(rv$filtered_data[[rv$id_col]])
        }
        else {
          rownames(flip_axs_vals) <- "all"
        }
        colnames(flip_axs_vals) <- paste0("LD", 1:lds_num())
        in_split_mode(!is.null(rv$cmp_mode) && rv$cmp_mode == 'split')
        # calculate LDA
        lda_result(make_ord_result(
          dat = dat, roles = rv$roles,
          has_id = in_split_mode(),
          id_col = rv$id_col, compare_mode = rv$cmp_mode,
          method = "lda",
          flip_axs_vals = flip_axs_vals
        ))
        group_colname(rv$roles$group)
        id_colname(rv$id_col)
        showNotification("LDA results computed", type = "message", duration = 4)
      })

      output$flip_axs_grid <- renderUI({
        # flipping axes control
        req(roles_ready())
        if (!is.null(rv$id_col)) {
          dataset_names <- unique(rv$filtered_data[[rv$id_col]])
        }
        else {
          dataset_names <- c("all")
        }
        nr <- length(dataset_names)
        cols <- paste0("C", 1:lds_num())
        tags$table(
          class = "table table-bordered table-sm",
          # class = "checkbox-matrix",
          tags$thead(
            tags$tr(
              tags$th("Flip LDA eigenvectors"),
              lapply(paste0("LD", 1:length(cols)), tags$th)
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

      angle_rv <- reactiveValues(pca_ref = NULL, pca_hover = NULL, lda_ref = NULL, lda_hover = NULL)
      centroid_rv <- reactiveValues(pca_ref = NULL, pca_hover = NULL, lda_ref = NULL, lda_hover = NULL)

      observeEvent(input$lda_axes_pair, {
        angle_rv$lda_ref <- NULL
        angle_rv$lda_hover <- NULL
        centroid_rv$lda_ref <- NULL
        centroid_rv$lda_hover <- NULL
      }, ignoreInit = TRUE)

      output$lda_comparison_heading <- renderUI({
        req(lda_result())
        if (!is.null(in_split_mode()) && in_split_mode()) {
          tags$h4("Comparison mode ON")
        }
        else {
          tags$h4("Comparison mode OFF (no variability in the dataset identifier column)")
        }
      })

      output$lda_facet_ui <- renderUI({
        if (!is.null(in_split_mode()) && in_split_mode() && !isTRUE(input$lda_interactive)) {
          checkboxInput(ns("lda_facet_by_id"), "Show each dataset in a separate panel (static mode only)", FALSE)
        }
        else {
          return(NULL)
        }
      })

      observeEvent(input$lda_interactive, {
        if(isTRUE(input$lda_interactive)) {
          updateCheckboxInput(session, "lda_facet_by_id", value = FALSE)
        }
      }, ignoreInit = TRUE)

      output$axes_controls <- renderUI({
        req(lda_result())
        if (in_split_mode()) {
          axis_choices <- colnames(lda_result()$scaling)[c(-1, -2)]
        }
        else {
          axis_choices <- colnames(lda_result()$scaling)[-1]
        }
        scale_choices <- c('Raw eigenvectors and PCA scores' = 'raw', 'Loadings and standardized PCA scores' = 'loadings')
        fluidRow(
          column(4, selectInput(ns("lda_axis_x"), "Variable on x axis", choices = axis_choices, selected = axis_choices[1])),
          column(4, selectInput(ns("lda_axis_y"), "Variable on y axis", choices = axis_choices, selected = axis_choices[2])),
          column(4, checkboxInput(ns("lda_interactive"), "Interactive plot mode (measure & compare)", FALSE)),
          column(4, uiOutput(ns("lda_facet_ui")))
        )
      })

      output$lda_plot_container <- renderUI({
        req(lda_result())
        if (isTRUE(input$lda_interactive)) {
          # interactive LDA biplot controls
          tagList(
            plotlyOutput(ns("lda_ord_plotly"), height = 700),
            fluidRow(
              column(8, div(style = "padding-top: 2px;", uiOutput(ns("lda_angle_controls")), uiOutput(ns("lda_centroid_controls")))),
              column(4, htmlOutput(ns("lda_vector_note")))
            )
          )
        }
        else {
          tagList(
            plotOutput(ns("lda_ord_plot"), height = 700),
            fluidRow(
              column(4, selectInput(ns("download_biplot_ext"), "Save as:",
                         choices = c("png", "jpeg", "tiff", "png", "pdf", "svg"),
                         selected = 1)),
              # download controls
              column(4,
                     div(
                       class="form-group shiny-input-container",
                       shiny::tags$label("\u00a0", class="control-label"),
                       div(downloadButton(ns("download_biplot"), "Export LDA biplot")))
                     ),
              column(4, htmlOutput(ns("lda_vector_note")))
            )
          )
        }
      })

      lda_all_current <- reactiveVal()
      lda_centroids_current <- reactiveVal()

      output$lda_ord_plotly <- renderPlotly({
        # interactive plot
        req(isTRUE(input$lda_interactive))

        AXIS_FONT_SIZE <- 16

        bld <- lda_plot_build()
        mi <- in_split_mode()
        faceted <- isTRUE(input$lda_facet_by_id) && isTRUE(mi) && !input$lda_interactive

        p <- bld$plot
        if (!is.na(bld$repel_layer_index) && !is.null(bld$all_lab)) {
          p <- replace_repel_with_text(p, bld$repel_layer_index, bld$all_lab, bld$label_size)
        }

        p <- finish_ord_plot(p, bld$xlab, bld$ylab, mi, faceted, TRUE,
                             group_name = group_colname(), id_name = id_colname())
        pl <- ggplotly(p, tooltip = c("x", "y"), source = "lda_ord_plotly")
        li <- extract_legend_info(p, lda_projected()$scores, group_colname(), mi)

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

        lda_cfg <- get_ord_cfg(input, "lda")
        lda_vec_df <- get_plotly_vectors(
          projected = lda_projected(),
          cfg = lda_cfg,
          multi_id = mi,
          include_id_suffix = isTRUE(lda_cfg$id_in_vector_labels)
        )

        lda_all_current(merge_vectors_with_axes(lda_vec_df, lda_projected()$xlab_plain, lda_projected()$ylab_plain))

        if (!isTRUE(faceted)) {
          pl <- add_plotly_vector_traces(
            pl,
            vectors_df = lda_vec_df,
            source_id = "lda_ord_plotly",
            ref_uid = angle_rv$lda_ref,
            hover_uid = angle_rv$lda_hover,
            xlab_plain = bld$xlab_plain,
            ylab_plain = bld$ylab_plain
          )
          lda_centroids_current(get_plotly_centroids(lda_projected(),
                                                     group_colname = group_colname(),
                                                     show_centroids = lda_cfg$show_centroids,
                                                     ))
          pl <- add_plotly_centroid_traces(
            pl,
            centroids_df = lda_centroids_current(),
            source_id = "lda_ord_plotly",
            group_colors = li$litho_colors,
            id_shapes = li$id_shapes,
            ref_uid = centroid_rv$lda_ref,
            hover_uid = centroid_rv$lda_hover,
            base_size = max(10, lda_cfg$centroid_size * 2.8)
          )
        }
        pl <- event_register(pl, "plotly_click")
        pl <- event_register(pl, "plotly_hover")

        pl
      })

      output$lda_angle_controls <- renderUI({
        # reference vector selection in interactive mode
        tagList(
          fluidRow(
            column(4, actionButton(ns("lda_clear_ref"), "Clear vector reference")),
            column(8, htmlOutput(ns("lda_angle_info")))
          )
        )
      })

      observeEvent(input$lda_clear_ref, {
        angle_rv$lda_ref <- NULL
        angle_rv$lda_hover <- NULL
      }, ignoreInit = TRUE)

      output$lda_angle_info <- renderUI({
        # interactive mode measurements results
        req(lda_all_current())
        if (!isTRUE(input$lda_interactive)) return(HTML("<span style='color:#666;'>Angle comparison available in interactive mode.</span>"))
        vv <- lda_all_current()
        if (nrow(vv) == 0) return(HTML("<span style='color:#666;'>No vectors/axes available for angle comparison.</span>"))
        info <- compute_pair_metrics(vv, angle_rv$lda_ref, angle_rv$lda_hover)
        if (is.null(angle_rv$lda_ref)) return(HTML("<span style='color:#666;'>Click a vector or axis selector to set reference.</span>"))
        if (is.null(info)) {
          ref_nm <- vv %>% filter(uid == angle_rv$lda_ref) %>% slice(1) %>% pull(display_var)
          if (length(ref_nm) == 0) ref_nm <- angle_rv$lda_ref
          return(HTML(paste0("<b>Reference:</b> ", ref_nm, " &nbsp;|&nbsp; Click another vector/axis selector to compare.")))
        }
        HTML(paste0(
          "<b>Reference vector:</b> ", info$ref,
          " &nbsp;|&nbsp; <b>Compared vector:</b> ", info$hov,
          " &nbsp;|&nbsp; <b>θ [°] (angle on ", input$lda_axis_x, "-", input$lda_axis_y,
          " plane):</b> ", sprintf("%.2f", info$ang),
          " &nbsp;|&nbsp; <b>cos(θ):</b> ", sprintf("%.4f", info$cs)
        ))
      })

      output$lda_centroid_controls <- renderUI({
        tagList(
          fluidRow(
            column(4, actionButton(ns("lda_clear_cent_ref"), "Clear centroid reference")),
            column(8, htmlOutput(ns("lda_centroid_info")))
          )
        )
      })

      observeEvent(input$lda_clear_cent_ref, {
        centroid_rv$lda_ref <- NULL
        centroid_rv$lda_hover <- NULL
      }, ignoreInit = TRUE)

      output$lda_centroid_info <- renderUI({
        req(lda_centroids_current())
        if (!isTRUE(input$lda_interactive)) return(HTML("<span style='color:#666;'>Centroid comparison available in interactive mode.</span>"))
        if (isTRUE(input$lda_facet_by_id) && n_distinct(lda_projected()$scores$dataset_id) > 1) return(HTML("<span style='color:#666;'>Centroid comparison is shown in interactive non-faceted mode.</span>"))
        ce <- lda_centroids_current()
        if (nrow(ce) == 0) return(HTML("<span style='color:#666;'>No centroids available for comparison.</span>"))
        info <- compute_centroid_pair_metrics(ce, centroid_rv$lda_ref, centroid_rv$lda_hover)
        if (is.null(centroid_rv$lda_ref)) return(HTML("<span style='color:#666;'>Click a centroid to set reference.</span>"))
        if (is.null(info)) {
          ref_nm <- ce %>% filter(uid == centroid_rv$lda_ref) %>% slice(1) %>% pull(label)
          if (length(ref_nm) == 0) ref_nm <- centroid_rv$lda_ref
          return(HTML(paste0("<b>Reference centroid:</b> ", ref_nm, " &nbsp;|&nbsp; Click another centroid to compare.")))
        }
        HTML(paste0(
          "<b>Reference centroid:</b> ", info$ref,
          " &nbsp;|&nbsp; <b>Compared centroid:</b> ", info$hov,
          " &nbsp;|&nbsp; <b>Euclidean distance (on ", input$lda_axis_x, "-", input$lda_axis_y,
          " plane):</b> ", sprintf("%.3f", info$dist)
        ))
      })

      register_vec_lock_sync(input, session, rv, "lda", "lda_lock_sync")

      observeEvent(event_data("plotly_click", source = "lda_ord_plotly"), {
        if (!isTRUE(input$lda_interactive)) return()
        ed <- event_data("plotly_click", source = "lda_ord_plotly")
        if (is.null(ed)) return()
        k <- ed$key %||% ed$customdata
        if (is.null(k) || !nzchar(as.character(k))) return()
        k <- as.character(k)[1]
        vv <- lda_all_current()
        if (nrow(vv) > 0 && k %in% vv$uid) {
          if (is.null(angle_rv$lda_ref)) {
            angle_rv$lda_ref <- k
          } else {
            angle_rv$lda_hover <- k
          }
        }
        ce <- lda_centroids_current()
        if (nrow(ce) > 0 && k %in% ce$uid) {
          if (is.null(centroid_rv$lda_ref)) {
            centroid_rv$lda_ref <- k
            centroid_rv$lda_hover <- NULL
          } else if (identical(k, centroid_rv$lda_ref)) {
            centroid_rv$lda_ref <- NULL
            centroid_rv$lda_hover <- NULL
          } else {
            centroid_rv$lda_hover <- k
          }
        }
      }, ignoreInit = TRUE)

      lda_projected <- reactiveVal()
      lda_ord_plot_base <- reactiveVal()

      output$lda_ord_plot <- renderPlot({
        req(lda_result())
        req(!isTRUE(input$lda_interactive))
        bld <- lda_plot_build()
        mi <- in_split_mode()
        lda_ord_plot_base(finish_ord_plot(bld$plot, bld$xlab, bld$ylab, mi, input$lda_facet_by_id, FALSE,
                                          group_name = group_colname(), id_name = id_colname()))
        lda_ord_plot_base()
      })

      lda_plot_build <- reactive({
        # create biplot
        req(lda_result())
        sc_key = 'scores'
        vec_key = 'scaling'
        # select plot data
        lda_projected(project_for_axes_pair(
          lda_result(),
          sc_key,
          vec_key,
          method = "lda",
          pair_x = input$lda_axis_x,
          pair_y = input$lda_axis_y,
          facet_by_id = isTRUE(input$lda_facet_by_id)
        ))
        # make plot
        make_plot_build("lda", lda_projected(), group_colname(),
                        interactive = isTRUE(input$lda_interactive))
      })

      make_plot_build <- function(prefix, projected, group_colname, interactive = FALSE) {
        # make biplot
        cfg <- get_ord_cfg(input, prefix)
        sc <- projected$scores
        multi_id <- n_distinct(sc$dataset_id) > 1
        group_present <- !is.null(group_colname)

        hu <- if (group_present) compute_hulls(sc, group_colname, "Axis1", "Axis2", if (multi_id) "dataset_id" else NULL) else sc[0, , drop = FALSE]
        ce <- if (group_present) compute_centroids(sc, group_colname, "Axis1", "Axis2", if (multi_id) "dataset_id" else NULL) else data.frame()
        av <- vector_scale_df(projected$vectors, "x", "y", cfg$vec_scale_active)
        sv_scale <- if (cfg$lock_vec_scale) cfg$vec_scale_active else cfg$vec_scale_supp
        sv <- projected$supp_vectors
        sv_present <- has_supp_rows(sv)
        if (sv_present) sv <- vector_scale_df(sv, "x", "y", sv_scale)
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

      output$download_biplot <- downloadHandler(
        filename = function() paste("lda_biplot", input$download_biplot_ext, sep = "."),
        content = function(file) {
          width_id <- paste0("output_", ns("lda_ord_plot"), "_width")
          height_id <- paste0("output_", ns("lda_ord_plot"), "_height")
          ggsave(file, plot = lda_ord_plot_base(),
                 height = session$clientData[[height_id]],
                 width = session$clientData[[width_id]],
                 units = 'mm',
                 scale = 0.35,
                 device = NULL)
        }
      )

      output$lda_vector_note <- renderUI({
        req(lda_result())
        tags$p(
          class = "vector-note",
          HTML("<span style='color:black; font-weight:600;'>Black vectors</span>: active variables <br> <span style='color:red; font-weight:600;'>Red vectors</span>: supplementary variables")
        )
      })


      output$scree_plot <- renderUI({
        req(lda_result())
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
            column(4, checkboxInput(ns("lda_bkn_stck"), "Show broken stick", TRUE))
          )
        )
      })

      lda_scree_plot_base <- reactiveVal()

      output$scree_plot_render <- renderPlot({
        # make scree plot
        lda_scree_plot_base(plot_pca_scree(lda_result(), isTRUE(input$lda_bkn_stck), id_colname()))
        lda_scree_plot_base()
      })

      output$download_scree_plot <- downloadHandler(
        filename = function() paste("lda_scree_plot", input$download_scree_plot_ext, sep = "."),
        content = function(file) {
          width_id <- paste0("output_", ns("scree_plot_render"), "_width")
          height_id <- paste0("output_", ns("scree_plot_render"), "_height")
          ggsave(file, plot = lda_scree_plot_base(),
                 height = session$clientData[[height_id]],
                 width = session$clientData[[width_id]],
                 units = 'mm',
                 scale = 0.35,
                 device = NULL)
        }
      )

      lda_num_result_export_server("lda_num_result_export", in_split_mode, lda_result)
    }
  )
}
