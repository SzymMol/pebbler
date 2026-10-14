preprocessing_server <- function(id, rv) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      roles_ready <- reactive(isTRUE(rv$roles_applied))

      output$prep_cols_ui_act <- renderUI({
        # select active columns for preprocessing
        req(roles_ready())
        # req(filtered_data(), roles_ready())
        # ch <- setdiff(rv$roles$active %||% character(0), rv$roles$supp %||% character(0))
        # ch <- ch[ch %in% names(filtered_data())[sapply(filtered_data(), is.numeric)]]
        # validate(need(length(ch) > 0, "No eligible active numeric variables."))
        selectizeInput(ns("prep_cols_act"), "Active columns to preprocess",
                       choices = rv$roles$active,
                       selected = rv$roles$active,
                       multiple = TRUE)
      })

      output$prep_cols_ui_supp <- renderUI({
        # select passive columns for preprocessing
        req(roles_ready())
        # req(filtered_data(), roles_ready())
        # ch <- setdiff(rv$roles$active %||% character(0), rv$roles$supp %||% character(0))
        # ch <- ch[ch %in% names(filtered_data())[sapply(filtered_data(), is.numeric)]]
        # validate(need(length(ch) > 0, "No eligible active numeric variables."))
        selectizeInput(ns("prep_cols_supp"), "Passive/supplementary columns to preprocess",
                       choices = rv$roles$supp,
                       multiple = TRUE)
      })

      observeEvent(input$apply_prep, {
        # apply preprocessing functions for selected columns
        req(rv$filtered_data, roles_ready())

        # active_numeric <- setdiff(rv$roles$active %||% character(0), rv$roles$supp %||% character(0))
        # active_numeric <- active_numeric[active_numeric %in% names(rv$data)[sapply(rv$data, is.numeric)]]

        if (length(input$prep_cols_act) == 0 && length(input$prep_cols_supp) == 0) {
          showNotification("No preprocessing applied. No columns selected.", type = "message")
          rv$data <- rv$filtered_data
          return()
        }
        if (length(input$prep_cols_act) > 0 && !isTRUE(input$do_log_act) && !isTRUE(input$do_scale_act) && !isTRUE(input$do_clr_act)) {
          showNotification("Select at least one preprocessing option for active columns.", type = "warning")
          return()
        }
        if (length(input$prep_cols_supp) > 0 && !isTRUE(input$do_log_supp) && !isTRUE(input$do_scale_supp) && !isTRUE(input$do_clr_supp)) {
          showNotification("Select at least one preprocessing option for passive/supplementary columns.", type = "warning")
          return()
        }

        dat <- rv$filtered_data # preprocessing starts with output data from previous tab

        # active columns preprocessing
        if (isTRUE(input$do_clr_act)) {
          dat <- apply_clr_preprocessing(dat, input$prep_cols_act)
        }
        if (isTRUE(input$do_log_act)) {
          dat <- apply_log_preprocessing(dat, input$prep_cols_act)
        }
        if (isTRUE(input$do_scale_act)) {
          dat <- apply_scale_preprocessing(dat, input$prep_cols_act, rv)
        }

        # passive columns preprocessing
        if (isTRUE(input$do_clr_supp)) {
          dat <- apply_clr_preprocessing(dat, input$prep_cols_supp)
        }
        if (isTRUE(input$do_log_supp)) {
          dat <- apply_log_preprocessing(dat, input$prep_cols_supp)
        }
        if (isTRUE(input$do_scale_supp)) {
          dat <- apply_scale_preprocessing(dat, input$prep_cols_supp, rv)
        }

        rv$data <- dat
        # prep log changed to only last action (preprocessing is not sequential) ?
        rv$prep_log <- list(list(
          time = as.character(Sys.time()),
          cols_act = input$prep_cols_act,
          clr_act = isTRUE(input$do_clr_act),
          log10_act = isTRUE(input$do_log_act),
          zscore_act = isTRUE(input$do_scale_act),
          cols_supp = input$prep_cols_supp,
          clr_supp = isTRUE(input$do_clr_supp),
          log10_supp = isTRUE(input$do_log_supp),
          zscore_supp = isTRUE(input$do_scale_supp)
        ))
        showNotification("Preprocessing applied.", type = "message")
      })

      apply_clr_preprocessing <- function(dat, cols) {
        # apply centred logratio (clr) to columns in dat
        if (any(dat[cols] <= 0)) {
          showNotification(paste("Skipping clr for selected columns (<=0 values)."), type = "warning")
        }
        else {
          result <- apply(dat[cols], 1, function(row) {
            gmean <- exp(mean(log(row)))
            return(log(row/gmean))
          })
          dat[cols] <- t(result)
        }
        return(dat)
      }

      apply_log_preprocessing <- function(dat, cols) {
        # apply log10 to columns in dat
        for (cl in cols) {
          if (any(dat[[cl]] <= 0)) {
            showNotification(paste("Skipping log10 for", cl, "(<=0 values)."), type = "warning")
          } else {
            dat[[cl]] <- log10(dat[[cl]])
          }
        }
        return(dat)
      }

      apply_scale_preprocessing <- function(dat, cols, rv) {
        # apply z-scaling to columns in dat
        #
        # if in comparison (split) mode scales are applied based on unique
        # values in dataset identifier column
        if (!is.null(rv$cmp_mode) && rv$cmp_mode == 'split') {
          dat <- dat %>%
            group_by(.data[[rv$id_col]]) %>%
            mutate(across(all_of(cols), ~as.numeric(scale(.x)))) %>%
            ungroup()
        }
        else {
          dat <- dat %>% mutate(across(all_of(cols), ~as.numeric(scale(.x))))
        }
        return(dat)
      }

      observeEvent(input$undo_prep, {
        # undo preprocessing
        req(rv$filtered_data)
        rv$data <- NULL
        # rv$data <- rv$filtered_data
        # rv$id_col <- detect_id_col(names(rv$raw_data))
        rv$prep_log <- list()
        updateCheckboxInput(session, "do_clr_act", value = FALSE)
        updateCheckboxInput(session, "do_log_act", value = FALSE)
        updateCheckboxInput(session, "do_scale_act", value = FALSE)
        updateCheckboxInput(session, "do_clr_supp", value = FALSE)
        updateCheckboxInput(session, "do_log_supp", value = FALSE)
        updateCheckboxInput(session, "do_scale_supp", value = FALSE)
        showNotification("Preprocessing undone. Original uploaded data restored.", type = "message")
      })

      output$prep_log <- renderPrint({
        # print preprocessing log
        if (length(rv$prep_log) == 0) {
          cat("No preprocessing applied.")
        } else {
          print(rv$prep_log)
        }
      })

      get_inspect_long <- reactive({
        # get data for preprocessing plots
        req(rv$filtered_data, roles_ready())
        # print(rv$filtered_data)
        if (is.null(rv$data)) {
          dat <- rv$filtered_data
        }
        else {
          dat <- rv$data
        }
        # ic <- setdiff(rv$roles$active %||% character(0), rv$roles$supp %||% character(0))
        # ic <- ic[ic %in% names(dat)[sapply(dat, is.numeric)]]
        # validate(need(length(ic) > 0, "No numeric active variables selected."))
        ic <- append(rv$roles$active, rv$roles$supp)
        # iv <- if (!is.null(rv$id_col)) as.factor(dat[[rv$id_col]]) else as.factor("all")
        # dat %>%
        #   mutate(.row_id = row_number()) %>%
        #   select(.data[[rv$id_col]], .row_id, all_of(ic)) %>%
        #   pivot_longer(cols = all_of(ic), names_to = "variable", values_to = "value")
        dat <- dat %>% mutate(.row_id = row_number())
        if (!is.null(rv$cmp_mode) && rv$cmp_mode == 'split') {
          dat <- dat %>% select(all_of(rv$id_col), .row_id, all_of(ic))
        }
        else {
          dat <- dat %>% select(.row_id, all_of(ic))
        }
        dat %>% pivot_longer(cols = all_of(ic), names_to = "variable", values_to = "value")
      })

      output$inspect_line_plot <- renderPlot({
        # plot line plots with preprocessed data (also in split mode if applicable)
        df <- get_inspect_long()
        nb <- if ((session$clientData$output_inspect_line_plot_width %||% 1200) < 700) 4 else 8
        p <- ggplot(df, aes(.row_id, value)) +
          geom_line(color = "#2C7FB8", linewidth = 0.45, alpha = 0.95) +
          scale_x_continuous(n.breaks = nb) +
          theme_minimal(base_size = 20) +
          theme(axis.title = element_text(size = 16), axis.text = element_text(size = 16), strip.text = element_text(size = 16)) +
          labs(x = "Sample order (row index)", y = "Value")
        if (!is.null(rv$cmp_mode) && rv$cmp_mode == 'split') p + facet_grid(.data[[rv$id_col]] ~ variable, scales = "free_y") else p + facet_wrap(~variable, scales = "free_y", ncol = 4)
      })

      output$inspect_hist_plot <- renderPlot({
        # plot histograms with preprocessed data (also in split mode if applicable)
        df <- get_inspect_long()
        nb <- if ((session$clientData$output_inspect_hist_plot_width %||% 1200) < 700) 4 else 8
        p <- ggplot(df, aes(value)) +
          geom_histogram(bins = 30, fill = "#4C78A8", alpha = 0.85, color = "white") +
          scale_x_continuous(n.breaks = nb) +
          theme_minimal(base_size = 20) +
          theme(axis.title = element_text(size = 16), axis.text = element_text(size = 16), strip.text = element_text(size = 16)) +
          labs(x = NULL, y = "Count")
        if (!is.null(rv$cmp_mode) && rv$cmp_mode == 'split') p + facet_grid(.data[[rv$id_col]] ~ variable, scales = "free") else p + facet_wrap(~variable, scales = "free", ncol = 4)
      })
    }
  )
}
