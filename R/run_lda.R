#' Run LDA
#'
#' Compute LDA results.
#' The procedure utilizes \code{\link[MASS]{lda}} from \code{MASS} package.
#'
#' @export
#' @param x_active data frame with active variables observations
#' @param x_supp data frame with supplementary variables observations
#' @param y_group data frame with the grouping variable labels
#' @param flip_cols vector of logical values indicating whether the sign of the
#'  respective LD column needs to be flipped
#' @param idv dataset name (if multiple datasets are analyzed)
#' @examples
#' data('witow', package = 'pebbler')
#' dat <- witow
#' roles <- list(active = c('a', 'b', 'c', 'mass'),
#'               group = 'lithotype',
#'               supp = 'hgt_min')
#' run_lda(x_active = scale(dat[roles$active]),
#'         x_supp = dat[roles$supp],
#'         y_group = dat[roles$group],
#'         flip_cols = rep(FALSE, length(roles$active)))
#' @importFrom shiny validate
#' @importFrom shiny need
#' @importFrom MASS lda
#' @importFrom stats predict


run_lda <- function(x_active, x_supp, y_group, flip_cols, idv = NULL) {
  validate(need(ncol(x_active) >= 1, "Need >=1 numeric active variables for LDA."))
  validate(need(!is.null(y_group) && ncol(y_group) > 0, "Choose a grouping variable for LDA."))

  # gf <- droplevels(as.factor(y_group))
  # gf <- droplevels(as.factor(y_group))
  validate(need(nrow(unique(y_group)) >= 2, "Grouping variable needs >=2 classes for LDA."))
  validate(need(nrow(x_active) > ncol(x_active), "Need more observations than variables for LDA."))


  # fit <- MASS::lda(x = x_active, grouping = t(y_group), CV = FALSE)
  fit <- lda(x = x_active, grouping = t(y_group), CV = FALSE)
  px <- predict(fit)

  s <- as.data.frame(px$x) # scores
  sc <- as.data.frame(fit$scaling) # unstandardized discriminant coefficients

  # print(sc)
  # print(s[1, ])
  # print((scale(as.matrix(x_active), center=colMeans(fit$means), scale=FALSE) %*% fit$scaling)[1, ])
  # print((as.matrix(s)%*%t(sc))[1, ])

  # # decomposition X = U@S@V.T
  # fit <- prcomp(x_active, center = TRUE, scale. = FALSE)
  # s <- as.data.frame(fit$x) # scores U@S
  # ev <- as.data.frame(fit$rotation) # eigenvectors in columns V

  # TO DO: check
  # flip eigenvectors
  # for (j in 1:dim(sc)[2]){
  #   if (sc[which.max(abs(sc[, j])), j] < 0) {
  #     sc[, j] <- -sc[, j]
  #     s[, j] <- -s[, j]
  #   }
  # }
  for (j in 1:dim(sc)[2]){
    if (flip_cols[j]) {
      sc[, j] <- -sc[, j]
      s[, j] <- -s[, j]
    }
  }

  # l <- as.data.frame(t(t(ev) * fit$sdev)) # loadings (covariances between variables and standardized components)
  # s_std <- as.data.frame(t(t(s) / fit$sdev))

  # l <- add_var_names(l)
  sc <- add_var_names(sc)

  # calculate supplementary vectors
  supp <- project_supp_vectors2(s, x_supp)
  # print(supp)
  # the above is not the same as (formula from Graffelman, 2003)
  # the problem lies in the definitions used: regression or correlation coefficients
  # f <- as.matrix(s)
  # print(solve(t(f)%*%f)%*%t(f)%*%as.matrix(scale(x_supp)))
  supp <- add_var_names(supp)

  # supp <- cbind(original_var = rownames(supp), supp)
  # rownames(supp) <- NULL

  # calculate eigenvalues table
  lda_vals <- fit$svd^2
  lda_pct <- if (length(lda_vals) > 0) lda_vals / sum(lda_vals) * 100 else numeric(0)
  lda_axis_df <- data.frame(
    # dataset_id = idv,
    LD = paste0("LD", seq_along(lda_pct)),
    svd = fit$svd,
    pct = lda_pct
  )
  # eig <- fit$sdev^2
  # prop <- eig / sum(eig)
  # bs <- broken_stick(length(eig))
  # eigen_df <- data.frame(
  #   PC = paste0("PC", seq_along(eig)),
  #   eigenvalue = eig,
  #   explained_var_pct = prop * 100,
  #   broken_stick_pct = bs * 100
  # )

  # add column with original labels
  if (length(y_group) > 0) {
    s <- bind_cols(s, y_group %>% mutate(across(all_of(colnames(y_group)), factor)))
    # s_std <- bind_cols(s_std, y_group %>% mutate(across(all_of(colnames(y_group)), factor)))
  }

  # add column dataset identifiers
  if (!is.null(idv)) {
    lda_axis_df <- cbind(dataset_id = as.factor(idv), lda_axis_df)
    s <- cbind(dataset_id = as.factor(idv), s)
    sc <- cbind(dataset_id = as.factor(idv), sc)
    # l <- cbind(dataset_id = as.factor(idv), l)
    # s_std <- cbind(dataset_id = as.factor(idv), s_std)
    if (nrow(supp) > 0) {
      supp <- cbind(dataset_id = as.factor(idv), supp)
    }
  }

  extra <- list(fit = fit, axis_pct = lda_axis_df)

  # s$dataset_id <- as.factor(idv)
  # ev$dataset_id <- as.factor(idv)

  return(c(list(scores = s,
                scaling = sc,
                supp = supp
                # loadings = l,
                # scores_std = s_std
  ), extra))
}

add_var_names <- function(v) {
  v <- cbind(original_var = rownames(v), v)
  rownames(v) <- NULL
  return(v)
}
