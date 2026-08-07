pca_num_result_export_server <- function(id, in_split_mode, pca_result) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$eigenvals_tbl <- renderDT({
        req(pca_result())
        datatable(
          pca_result()$eigen %>%
            rename(`explained variance %` = explained_var_pct, `broken-stick %` = broken_stick_pct)
        )
      })

      output$eigenvals_interpret <- renderUI({
        req(pca_result())
        if ("dataset_id" %in% colnames(pca_result()$eigen)) {
          interpret_tag <- tags$h5(paste0("Interpretation examples: ",
              pca_result()$eigen$PC[1], " explains ",
              sprintf("%.2f", pca_result()$eigen$explained_var_pct[1]),
              '% of variance in "', pca_result()$eigen$dataset_id[1], '" dataset, and ',
              pca_result()$eigen$PC[2], " explains ",
              sprintf("%.2f", pca_result()$eigen$explained_var_pct[2]),
              '% of variance in "', pca_result()$eigen$dataset_id[2], '" dataset.'))
        }
        else {
          interpret_tag <- tags$h5(paste0("Interpretation examples: ",
             pca_result()$eigen$PC[1], " explains ",
             sprintf("%.2f", pca_result()$eigen$explained_var_pct[1]),
             "% of variance in the dataset, and ",
             pca_result()$eigen$PC[2], " explains ",
             sprintf("%.2f", pca_result()$eigen$explained_var_pct[2]),
             "% of variance in the dataset."))
        }
        tagList(
          downloadButton(ns("download_eigenvals_tbl"), "Export eigenvalues table"),
          interpret_tag
        )
      })

      output$download_eigenvals_tbl <- downloadHandler(
        filename = function() "pca_eigenvals_table.csv",
        content = function(file) {
          req(pca_result())
          write_csv(
            pca_result()$eigen, # %>%
              # rename(`explained variance %` = explained_var_pct, `broken-stick %` = broken_stick_pct),
            file
          )
        }
      )

      output$eigenvects_tbl <- renderDT({
        req(pca_result())
        datatable(
          pca_result()$eig_vectors %>%
          rename(`original variable` = original_var)

        )
      })

      output$eigenvects_interpret <- renderUI({
        req(pca_result())
        l <- pca_result()$eig_vectors
        if ("dataset_id" %in% colnames(l)) {
          dataset_name = unique(l[["dataset_id"]])[1]
          l <- l %>% filter(dataset_id == dataset_name)
          var_names <- l[["original_var"]]
          interpret_tag <- tags$h5(paste0('Interpretation examples: within "',
              dataset_name, '" dataset, ',
              'variable "', var_names[which.max(abs(l[,  pca_result()$eigen$PC[1]]))],
              '" has the strongest association with ',
              pca_result()$eigen$PC[1], " among all original variables, and ",
              'variable "', var_names[which.max(abs(l[, pca_result()$eigen$PC[2]]))],
              '" has the strongest association dataset with ',
              pca_result()$eigen$PC[2], " among all original variables."
          ))
        }
        else {
          var_names <- l[["original_var"]]
          interpret_tag <- tags$h5(paste0("Interpretation examples: ",
             'variable "', var_names[which.max(abs(l[,  pca_result()$eigen$PC[1]]))],
             '" has the strongest association with ',
             pca_result()$eigen$PC[1], " among all original variables, and ",
             'variable "', var_names[which.max(abs(l[, pca_result()$eigen$PC[2]]))],
             '" has the strongest association with ',
             pca_result()$eigen$PC[2], " among all original variables."
             ))
        }
        tagList(
          downloadButton(ns("download_eigenvects_tbl"), "Export eigenvectors table"),
          interpret_tag
        )
      })

      output$download_eigenvects_tbl <- downloadHandler(
        filename = function() "pca_eigenvects_table.csv",
        content = function(file) {
          req(pca_result())
          write_csv(
            pca_result()$eig_vectors,
            file
          )
        }
      )

      output$supp_vects_tbl <- renderDT({
        req(pca_result())
        datatable(
          pca_result()$supp_vectors %>%
            rename(`original variable` = original_var)
        )
      })

      output$supp_vects_interpret <- renderUI({
        req(pca_result())
        s <- pca_result()$supp_vectors
        if (nrow(s) == 0) return()
        corr_type <- function (r) ifelse((r > 0), "positively", "negatively")
        if ("dataset_id" %in% colnames(pca_result()$supp_vectors)) {
          dataset_name = unique(s[["dataset_id"]])[1]
          s <- s %>% filter(dataset_id == dataset_name)
          var_name <- s$original_var[1]
          interpret_tag <-
            tags$h5(paste0('Interpretation examples: within "', dataset_name,
                         '" dataset, variable "',
                         var_name,
                         '" is ',  corr_type(s[1, pca_result()$eigen$PC[1]]),
                         " correlated with ",
                         pca_result()$eigen$PC[1], ", and ",
                         'variable "',
                         var_name, '" is ',
                         corr_type(s[1, pca_result()$eigen$PC[2]]),
                         " correlated with ",
                         pca_result()$eigen$PC[2], "."))
        }
        else {
          var_name <- s$original_var[1]
          interpret_tag <-
            # tags$details(
              # tags$summary(
              #   style = "cursor:pointer; font-weight:bold;",
              #   "Show interpretation (click)"
              # ),
              tags$h5(paste0("Interpretation examples: ",
                             'variable "',
                             var_name,
                             '" is ',  corr_type(s[1, pca_result()$eigen$PC[1]]),
                             " correlated with ",
                             pca_result()$eigen$PC[1], ", and ",
                             'variable "',
                             var_name, '" is ',
                             corr_type(s[1, pca_result()$eigen$PC[2]]),
                             " correlated with ",
                             pca_result()$eigen$PC[2], "."))
              # )
        }
        tagList(
          downloadButton(ns("download_supp_vects_tbl"), "Export supplementary variables vectors table"),
          # tags$p("\n"),
          interpret_tag
        )
      })

      output$download_supp_vects_tbl <- downloadHandler(
        filename = function() "pca_supp_vects_table.csv",
        content = function(file) {
          req(pca_result())
          write_csv(
            pca_result()$supp_vectors,
            file
          )
        }
      )

      output$scores_tbl <- renderDT({
        req(pca_result())
        datatable(
          pca_result()$scores
        )
      })

      output$scores_interpret <- renderUI({
        req(pca_result())
        interpret_tag <- tags$h5(
          paste0("Interpretation examples: PCA scores represent positions of data
                 points in the principal axes frame of reference.",
                 ifelse("dataset_id" %in% colnames(pca_result()$scores),
                        " Results were obtained separately for each dataset.", "")))
        tagList(
          downloadButton(ns("download_scores_tbl"), "Export scores table"),
          interpret_tag
        )
      })

      output$download_scores_tbl <- downloadHandler(
        filename = function() "pca_scores_table.csv",
        content = function(file) {
          req(pca_result())
          write_csv(
            pca_result()$scores,
            file
          )
        }
      )

      output$loadings_tbl <- renderDT({
        req(pca_result())
        datatable(
          pca_result()$loadings %>%
            rename(`original variable` = original_var)
        )
      })

      output$loadings_interpret <- renderUI({
        req(pca_result())
        # cov(dat$mass, out$scores_std$PC1)

        interpret_tag <- tags$h5(
          paste0("Interpretation examples: PCA loadings estimate the covariance
                  between original variables and standardized PCA scores.
                  The length of a loading vector approximates the standard deviation
                  of the respective variable.
                  The cosine of the angle between two loading vectors
                  (or between a loading vector and an axis) approximates correlation
                  coefficient between the respective quantities.",
                 ifelse("dataset_id" %in% colnames(pca_result()$scores),
                        " Results were obtained separately for each dataset.", "")))
        tagList(
          downloadButton(ns("download_loadings_tbl"), "Export loadings table"),
          interpret_tag
        )
      })

      output$download_loadings_tbl <- downloadHandler(
        filename = function() "pca_loadings_table.csv",
        content = function(file) {
          req(pca_result())
          write_csv(
            pca_result()$loadings,
            file
          )
        }
      )

      output$scores_std_tbl <- renderDT({
        req(pca_result())
        datatable(
          pca_result()$scores_std
        )
      })

      output$scores_std_interpret <- renderUI({
        req(pca_result())
        interpret_tag <- tags$h5(
          paste0("Interpretation examples: PCA standardized scores represent
                 positions of data points in the principal axes frame of reference
                 normalized to unit variance.",
                 ifelse("dataset_id" %in% colnames(pca_result()$scores),
                        " Results were obtained separately for each dataset.", "")))
        tagList(
          downloadButton(ns("download_scores_std_tbl"), "Export standardized scores table"),
          interpret_tag
        )
      })

      output$download_scores_std_tbl <- downloadHandler(
        filename = function() "pca_scores_std_table.csv",
        content = function(file) {
          req(pca_result())
          write_csv(
            pca_result()$scores_std,
            file
          )
        }
      )

    }
  )
}
