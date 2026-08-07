#' Run PCA
#'
#' compute PCA results
#'
#' @export
#' @examples
#' run_pca(pca_result)
#' @importFrom shiny validate
#' @importFrom shiny need

run_pca <- function(x_active, x_supp, y_group, flip_cols, roles, idv = NULL) {
  # validate(need(ncol(x_active) >= 2, "Need >=2 numeric active variables for PCA."))
  # validate(need(nrow(x_active) >= 2, "Need >=2 complete observations for PCA."))

  # decomposition X = U@S@V.T
  fit <- prcomp(x_active, center = TRUE, scale. = FALSE)
  s <- as.data.frame(fit$x) # scores U@S
  ev <- as.data.frame(fit$rotation) # eigenvectors in columns V

  # flip eigenvectors
  for (j in 1:dim(ev)[2]){
    if (ev[which.max(abs(ev[, j])), j] < 0) {
      ev[, j] <- -ev[, j]
      s[, j] <- -s[, j]
    }
  }
  for (j in 1:dim(ev)[2]){
    if (flip_cols[j]) {
      ev[, j] <- -ev[, j]
      s[, j] <- -s[, j]
    }
  }

  l <- as.data.frame(t(t(ev) * fit$sdev)) # loadings (covariances between variables and standardized components)
  s_std <- as.data.frame(t(t(s) / fit$sdev))

  l <- add_var_names(l)
  ev <- add_var_names(ev)

  # calculate supplementary vectors
  supp <- project_supp_vectors2(s, x_supp)
  supp <- cbind(original_var = rownames(supp), supp)
  rownames(supp) <- NULL

  # calculate eigenvalues table
  eig <- fit$sdev^2
  prop <- eig / sum(eig)
  bs <- broken_stick(length(eig))
  eigen_df <- data.frame(
    PC = paste0("PC", seq_along(eig)),
    eigenvalue = eig,
    explained_var_pct = prop * 100,
    broken_stick_pct = bs * 100
  )

  # add column with original labels
  if (length(y_group) > 0) {
    s <- bind_cols(s, y_group %>% mutate(across(all_of(colnames(y_group)), factor)))
    s_std <- bind_cols(s_std, y_group %>% mutate(across(all_of(colnames(y_group)), factor)))
  }

  # add column dataset identifiers
  if (!is.null(idv)) {
    eigen_df <- cbind(dataset_id = as.factor(idv), eigen_df)
    s <- cbind(dataset_id = as.factor(idv), s)
    ev <- cbind(dataset_id = as.factor(idv), ev)
    l <- cbind(dataset_id = as.factor(idv), l)
    s_std <- cbind(dataset_id = as.factor(idv), s_std)
    if (nrow(supp) > 0) {
      supp <- cbind(dataset_id = as.factor(idv), supp)
    }
  }

  extra <- list(eigen = eigen_df, fit = fit)

  # s$dataset_id <- as.factor(idv)
  # ev$dataset_id <- as.factor(idv)

  return(c(list(scores = s, eig_vects = ev, supp = supp, loadings = l, scores_std = s_std),
           extra))
}

project_supp_vectors2 <- function(scores_xy, supp_df) {
  # calculate supplementary variables projections for PCA
  supp <- scores_xy[FALSE, ]
  if (is.null(supp_df) || ncol(supp_df) == 0 || nrow(supp_df) == 0) return(supp)
  for (i in 1:length(supp_df)){
    for (j in 1:length(scores_xy)) {
      supp[i, j] = cor(scores_xy[, j], supp_df[, i], use = "complete.obs")
    }
  }
  rownames(supp) <- colnames(supp_df)
  return(supp)
}

add_var_names <- function(v) {
  v <- cbind(original_var = rownames(v), v)
  rownames(v) <- NULL
  return(v)
}
