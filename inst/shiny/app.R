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

FILLABLE_SHAPES <- c(21, 22, 23, 24, 25)

AXIS_FONT_SIZE <- 16

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .top-aligned-row { display: flex; align-items: flex-end; flex-wrap: wrap; }
    .top-aligned-row > div { float: none; }
    .checkbox { margin-top: 0px; margin-bottom: 0px; }
    .vector-note { margin-top: 6px; margin-bottom: 0px; font-size: 14px; color: #444; }
    "))),
  titlePanel("pebbler: Multivariate Statistics in Geoscience Made Easy"),
  tabsetPanel(
    id = "main_tabs",
    input_data_ui("Data"),
    preprocessing_ui("Preprocessing"),
    pca_ui("PCA"),
    lda_ui("LDA")
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    raw_data = NULL, # stores raw data loaded from a file
    data = NULL, # stores data for further stages
    id_col = NULL, # dataset column
    roles = list(active = character(), group = NULL, supp = character()), # roles
    roles_applied = FALSE, # flag controlling roles selection
    prep_log = list(), # preprocesing actions logs
    pca_lock_sync = FALSE, # PCA lock vector scale
    lda_lock_sync = FALSE, # LDA lock vector scale
    cmp_mode = NULL # comparison mode flag
  )

  input_data_server("Data", rv)
  preprocessing_server("Preprocessing", rv)
  pca_server("PCA", rv)
  lda_server("LDA", rv)
}

shinyApp(ui, server)
