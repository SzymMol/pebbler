pca_num_result_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hr(), h4("Eigenvalues"), DTOutput(ns("eigenvals_tbl")), uiOutput(ns("eigenvals_interpret")),
    hr(), h4("Eigenvectors (PCA coefficients)"), DTOutput(ns("eigenvects_tbl")), uiOutput(ns("eigenvects_interpret")),
    hr(), h4("Supplementary variables vectors"), DTOutput(ns("supp_vects_tbl")), uiOutput(ns("supp_vects_interpret")),
    hr(), h4("PCA scores"), DTOutput(ns("scores_tbl")), uiOutput(ns("scores_interpret")),
    hr(), h4("PCA loadings"), DTOutput(ns("loadings_tbl")), uiOutput(ns("loadings_interpret")),
    hr(), h4("PCA standardized scores"), DTOutput(ns("scores_std_tbl")), uiOutput(ns("scores_std_interpret"))
  )
}
