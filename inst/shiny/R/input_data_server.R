input_data_server <- function(id, rv) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      observeEvent(input$file, {
        # read input file
        req(input$file)
        dat <- read_csv(input$file$datapath, show_col_types = FALSE)
        rv$raw_data <- dat
        # rv$data <- dat
        rv$roles_applied <- FALSE
        rv$roles_update_pca <- FALSE
        rv$roles_update_lda <- FALSE
        # rv$prep_log <- list()
      })
      output$active_ui <- renderUI({
        # select active variables
        req(rv$raw_data)
        selectizeInput(ns("active_vars"), "Active variables (e.g., pebble measurements)",
                       choices = colnames(rv$raw_data[, sapply(rv$raw_data, is.numeric)]),
                       multiple = TRUE)
      })
      output$group_ui <- renderUI({
        # select grouping variable
        req(rv$raw_data)
        selectInput(
          ns("group_var"), "Grouping variable/label (e.g., lithotype)",
          choices = c(" " = "", names(rv$raw_data)),
          selected = character(0)
        )
      })
      output$supp_ui <- renderUI({
        # select supplementary (passive) variables
        req(rv$raw_data)
        selectizeInput(ns("supp_vars"), "Supplementary/passive variables (e.g., stratigraphic height/depth)",
                       choices = colnames(rv$raw_data[, sapply(rv$raw_data, is.numeric)]),
                       multiple = TRUE)
      })
      output$source_ui <- renderUI({
        # select dataset identifier
        req(rv$raw_data)
        selectInput(ns("source_id"), "Top-level dataset identifier (e.g., for comparison between datasets)",
                    choices = c(" " = "", names(rv$raw_data)),
                    selected = character(0)
        )
      })
      check_cmp_mode <- function() {
        # determine whether program operates in comparison mode
        if (is.null(input$source_id) || !nzchar(input$source_id)) {
          return(FALSE)
        }
        return(length(t(unique(rv$raw_data[input$source_id]))) > 1)
      }
      in_cmp_mode <- reactive(check_cmp_mode())
      output$comparison_heading <- renderUI({
        # print comparison mode information
        req(rv$raw_data)
        if (in_cmp_mode()) {
          if (anyNA(rv$raw_data[[input$source_id]])) {
            tags$h4(paste0("Comparison mode ON - Missing data in column '", input$source_id, "'"))
          }
          else {
            tags$h4("Comparison mode ON")
          }
        }
        else {
          tags$h4("Comparison mode OFF (no variability in the dataset identifier column)")
        }
      })

      output$compare_ids_ui <- renderUI({
        # select values for comparison
        req(rv$raw_data)
        if (!in_cmp_mode() || anyNA(rv$raw_data[[input$source_id]])) return(NULL)
        ids <- unique(as.character(rv$raw_data[[input$source_id]]))
        selectizeInput(ns("compare_ids"), "Dataset IDs to compare", choices = ids, selected = ids, multiple = TRUE)
      })

      output$compare_mode_ui <- renderUI({
        # select comparison mode options
        req(rv$raw_data)
        if (!in_cmp_mode() || anyNA(rv$raw_data[[input$source_id]])) return(NULL)
        selectInput(
          ns("compare_mode"), "Ordination mode",
          choices = c("Common for selected IDs" = "pooled", "Separate for each ID (overlaid)" = "split"),
          selected = "split"
        )
      })

      filtered_data <- reactive({
        # return data filtered using comparison values
        req(rv$raw_data)
        dat <- rv$raw_data
        if (in_cmp_mode() && !anyNA(dat[[input$source_id]])) {
          keep_ids <- input$compare_ids %||% unique(as.character(dat[[input$source_id]]))
          dat <- dat %>% filter(as.character(.data[[input$source_id]]) %in% keep_ids)
          dat[[input$source_id]] <- as.factor(dat[[input$source_id]])
          if (length(unique(dat[[input$source_id]])) == 1) {
            # only one class chosen
            rv$cmp_mode <- NULL
          }
          else {
            rv$cmp_mode <- input$compare_mode
          }
        }
        else {
          rv$cmp_mode <- NULL
        }
        rv$filtered_data <- dat
        rv$data <- NULL # clear preprocessing data
        dat
      })

      output$preview <- renderDT({
        # render data preview
        req(filtered_data())
        datatable(filtered_data(), options = list(scrollX = TRUE))
      })

      observeEvent(input$apply_roles, {
        # pass column names to next stages
        req(input$active_vars)
        act_col <- reactive(input$active_vars %||% character(0))
        supp_col <- reactive(input$supp_vars %||% character(0))
        group_col <- reactive(if (!is.null(input$group_var) && nzchar(input$group_var)) input$group_var else NULL)
        id_col <- reactive(if (in_cmp_mode() && !is.null(input$source_id) && nzchar(input$source_id)) input$source_id else NULL)

        # data validity checks
        if (nrow(rv$raw_data[act_col()]) < 2) {
          showNotification(
            paste0("Roles not applied. There must be at least 2 rows of data."),
            type = "warning", duration = 4
          )
          return()
        }

        if (length(act_col()) < 2) {
          showNotification(
            paste0("Roles not applied. There must be at least 2 active columns."),
            type = "warning", duration = 4
          )
          return()
        }

        has_conflict_cols <- check_conflict_columns(act_col(), supp_col())
        if (has_conflict_cols) {
          showNotification(
            paste0("Roles not applied. The same column can't be both active and passive."),
            type = "warning", duration = 4
          )
          return()
        }

        has_missing_data <- check_missing_data(act_col(), supp_col(), group_col(), id_col())

        if (any(as.logical(has_missing_data))) {
          showNotification(
            paste0(
              "Roles not applied - missing data: ",
              ifelse(has_missing_data$act_col, "active column(s), ", ""),
              ifelse(has_missing_data$group_col, "group column, ", ""),
              ifelse(has_missing_data$supp_col, "supplementary column(s), ", ""),
              ifelse(has_missing_data$id_col, "ID column", "")
            ),
            type = "warning", duration = 4
          )
          return()
        }

        # save column names to external variables

        rv$roles <- list(active = act_col(), group = group_col(), supp = supp_col())
        rv$id_col <- id_col()
        rv$roles_update_pca <- rv$roles_applied
        rv$roles_update_lda <- rv$roles_applied
        rv$roles_applied <- TRUE
        rv$data <- NULL # clear preprocessing data

        showNotification(
          paste0(
            "Roles applied: active=", length(rv$roles$active),
            ", group=", ifelse(is.null(rv$roles$group), "none", rv$roles$group),
            ", supplementary=", length(rv$roles$supp)
          ),
          type = "message", duration = 4
        )
      })
      check_missing_data <- function(act_col, supp_col, group_col, id_col) {
        # check input dataset for missing data
        return(list(act_col = anyNA(filtered_data()[act_col]),
                    supp_col = anyNA(filtered_data()[supp_col]),
                    group_col = anyNA(filtered_data()[group_col]),
                    id_col = anyNA(filtered_data()[id_col])))
      }
      check_conflict_columns <- function (act_col, supp_col) {
        return(length(intersect(act_col, supp_col)) > 0)
      }
    }
  )
}
