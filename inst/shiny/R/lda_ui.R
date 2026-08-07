lda_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "LDA", br(),
    uiOutput(ns("lda_ord_top")),
    hr(), uiOutput(ns("lda_comparison_heading")),
    # fluidRow(column(4, uiOutput(ns("pca_align_mode_ui"))),
    #          column(4, uiOutput(ns("pca_ref_id_ui"))),
    #          column(4, uiOutput(ns("pca_facet_ui")))),
    hr(), h4("LDA ordination"),
    uiOutput(ns("axes_controls")),
    uiOutput(ns("lda_plot_container")),
        # fluidRow(
      # column(6, htmlOutput(ns("pca_vector_note"))),
    #   column(6, div(style = "padding-top: 2px;", uiOutput(ns("pca_angle_controls")), uiOutput(ns("pca_centroid_controls"))))
    # ),
    hr(),
    fluidRow(
      column(3, checkboxInput(ns("lda_show_points"), "Show points (LDA scores)", FALSE)),
      column(3, checkboxInput(ns("lda_show_hull_outlines"), "Show convex hull outlines (for groups)", FALSE)),
      column(3, checkboxInput(ns("lda_fill_hulls"), "Fill convex hulls (for groups)", FALSE)),
      column(3, checkboxInput(ns("lda_show_centroids"), "Show centroids (for groups)", TRUE))
    ),
    fluidRow(
      column(3, sliderInput(ns("lda_pt_size"), "Point size", 0.5, 8, 2.3, 0.1)),
      column(3, sliderInput(ns("lda_pt_alpha"), "Point opacity", 0, 1, 0.5, 0.05)),
      column(3, sliderInput(ns("lda_hull_fill_alpha"), "Hull fill opacity", 0, 1, 0.5, 0.05)),
      column(3, sliderInput(ns("lda_centroid_size"), "Centroid size", 1, 12, 4.5, 0.5))
    ),
    fluidRow(
      column(3, sliderInput(ns("lda_label_size"), "Label size", 3, 24, 10, 0.5)),
      column(3, checkboxInput(ns("lda_show_vector_labels"), "Show vector labels", TRUE)),
      column(3, checkboxInput(ns("lda_id_in_vector_labels"), "ID in vector labels", TRUE)),
      column(3, checkboxInput(ns("lda_show_centroid_labels"), "Show centroid labels", TRUE)),
      column(3, checkboxInput(ns("lda_show_zero_axes"), "Bold zero axes", TRUE)),
      column(3, checkboxInput(ns("lda_equal_aspect"), "Equal aspect ratio", FALSE))
    ),
    fluidRow(
      column(3, sliderInput(ns("lda_vec_scale_active"), "Active variable vectors scale", 0.1, 10, 1, 0.1)),
      column(3, sliderInput(ns("lda_vec_scale_supp"), "Supplementary variable vectors scale", 0.1, 100, 1, 0.1)),
      column(3, checkboxInput(ns("lda_lock_vec_scale"), "Lock vector scaling", FALSE))
    ),
    # hr(), verbatimTextOutput("lda_info"), DTOutput("lda_tbl"),

    # hr(), h4("Scree plot"), uiOutput(ns("scree_plot")),
    lda_num_result_export_ui(ns("lda_num_result_export"))
    )
}
