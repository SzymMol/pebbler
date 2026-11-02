# PCA and LDA top panel
ord_top_controls <- function(id, prefix, method_label) {
  ns <- NS(id)
  tagList(
    tags$details(
      tags$summary(
        style = "cursor:pointer; font-weight:bold;",
        paste0("Advanced ", method_label, " options (click to see)")
      ),
      br(),
      uiOutput(ns("flip_axs_grid"))
    ),
    fluidRow(
      column(4, div(style = "padding-top: 10px;",
                    actionButton(ns("compute_ord"), if (prefix == "pca") "Run PCA" else "Run LDA")))
    )
  )
}
