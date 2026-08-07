input_data_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Data", br(),
    fluidRow(
      column(4, fileInput(ns("file"), "Upload CSV", accept = ".csv")),
      column(4, div(style = "padding-top: 25px;", actionButton(ns("apply_roles"), "Apply variables")))
    ),
    tags$div(class = "top-aligned-row", column(4, uiOutput(ns("active_ui"))),
             column(4, uiOutput(ns("group_ui"))),
             column(4, uiOutput(ns("supp_ui"))),
             column(4, uiOutput(ns("source_ui")))),
    hr(), uiOutput(ns("comparison_heading")),
    fluidRow(column(6, uiOutput(ns("compare_ids_ui"))),
             column(6, uiOutput(ns("compare_mode_ui")))),
    hr(), DTOutput(ns("preview"))
  )
}
