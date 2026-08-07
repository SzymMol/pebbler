ord_top_controls <- function(id, prefix, method_label) {
  ns <- NS(id)
  flip1_id <- if (prefix == "pca") "flip_pc1" else "flip_ld1"
  flip2_id <- if (prefix == "pca") "flip_pc2" else "flip_ld2"
  flip1_label <- if (prefix == "pca") "Flip PC1 axis" else "Flip LD1 axis"
  flip2_label <- if (prefix == "pca") "Flip PC2 axis" else "Flip LD2 axis"
  axis_pair_id <- paste0(prefix, "_axes_pair")
  axis_choices <- if (prefix == "pca") {
    c("PC1 vs PC2" = "1_2", "PC2 vs PC3" = "2_3")
  } else {
    c("LD1 vs LD2" = "1_2", "LD2 vs LD3" = "2_3")
  }


  tagList(
    fluidRow(
      # column(4, downloadButton(ns(paste0("download_", prefix, "_tbl")), paste("Export", method_label, "table"))),
      # column(4, checkboxInput(ns(paste0(prefix, "_interactive")), "Interactive plot (measure & compare)", FALSE)),
      # column(4, selectInput(ns(axis_pair_id), "Axes to display", choices = axis_choices, selected = "1_2"))
    ),
    tags$details(
      tags$summary(
        style = "cursor:pointer; font-weight:bold;",
        paste0("Advanced ", method_label, " options (click to see)")
      ),
      br(),
      # fluidRow(
        # column(6, checkboxInput(ns(flip1_id), flip1_label, FALSE)),
        # column(6, checkboxInput(ns(flip2_id), flip2_label, FALSE))
      # ),
      # br(),
      uiOutput(ns("flip_axs_grid"))
    ),
    fluidRow(
      column(4, div(style = "padding-top: 10px;",
                    actionButton(ns("compute_ord"), if (prefix == "pca") "Run PCA" else "Run LDA")))
    )
  )
}
