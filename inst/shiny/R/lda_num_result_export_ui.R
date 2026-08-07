lda_num_result_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hr(), h4("Singular values"), DTOutput(ns("axis_pct_tbl")), uiOutput(ns("axis_pct_interpret")),
    hr(), h4("Unstandardized discriminant coefficients"), DTOutput(ns("scaling_tbl")), uiOutput(ns("scaling_interpret")),
    hr(), h4("Supplementary variables vectors"), DTOutput(ns("supp_vects_tbl")), uiOutput(ns("supp_vects_interpret")),
    hr(), h4("LDA scores"), DTOutput(ns("scores_tbl")), uiOutput(ns("scores_interpret")),
    # hr(), h4("PCA loadings"), DTOutput(ns("loadings_tbl")), uiOutput(ns("loadings_interpret")),
    # hr(), h4("PCA standardized scores"), DTOutput(ns("scores_std_tbl")), uiOutput(ns("scores_std_interpret"))
  )
}
