preprocessing_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Preprocessing", br(),
    fluidRow(
      column(6, uiOutput(ns("prep_cols_ui_act")),
             checkboxInput(ns("do_clr_act"), "clr transform selected columns (for compositional data)", FALSE),
             checkboxInput(ns("do_log_act"), "Log10 transform selected columns", FALSE),
             checkboxInput(ns("do_scale_act"), "Z-score standardize selected columns", FALSE)),
      column(6, uiOutput(ns("prep_cols_ui_supp")),
             checkboxInput(ns("do_clr_supp"), "clr transform selected columns (for compositional data)", FALSE),
             checkboxInput(ns("do_log_supp"), "Log10 transform selected columns", FALSE),
             checkboxInput(ns("do_scale_supp"), "Z-score standardize selected columns", FALSE))
    ),
    fluidRow(
      column(4, actionButton(ns("apply_prep"), "Apply preprocessing")),
      column(4, actionButton(ns("undo_prep"), "Undo preprocessing"))
    ),
    br(), h4("Active variable plots"),
    plotOutput(ns("inspect_line_plot"), height = 420),
    plotOutput(ns("inspect_hist_plot"), height = 660),
    hr(), verbatimTextOutput(ns("prep_log"))
  )
}
