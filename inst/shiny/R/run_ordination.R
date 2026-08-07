run_ordination <- function(df_sub, idv, method, flip1, flip2, roles) {
  x <- safe_numeric_df(df_sub, roles$active)
  cr <- complete.cases(x)
  x <- x[cr, , drop = FALSE]
  df_sub <- df_sub[cr, , drop = FALSE]

  if (method == "pca") {
    validate(need(ncol(x) >= 2, "Need >=2 numeric active variables for PCA."))
    validate(need(nrow(x) >= 2, "Need >=2 complete observations for PCA."))

    fit <- prcomp(x, center = TRUE, scale. = FALSE)

    na3 <- min(3, ncol(fit$x))
    s <- as.data.frame(fit$x[, 1:na3, drop = FALSE])
    while (ncol(s) < 3) s[[paste0("PC", ncol(s) + 1)]] <- 0
    names(s)[1:3] <- c("Axis1", "Axis2", "Axis3")

    nr3 <- min(3, ncol(fit$rotation))
    l <- as.data.frame(fit$rotation[, 1:nr3, drop = FALSE])
    while (ncol(l) < 3) l[[paste0("PC", ncol(l) + 1)]] <- 0
    l$var <- rownames(l)
    rownames(l) <- NULL
    names(l)[1:3] <- c("x", "y", "z")

    eig <- fit$sdev^2
    prop <- eig / sum(eig)
    bs <- broken_stick(length(eig))
    eigen_df <- data.frame(
      dataset_id = idv,
      PC = paste0("PC", seq_along(eig)),
      eigenvalue = eig,
      explained_var_pct = prop * 100,
      broken_stick_pct = bs * 100
    )
    extra <- list(eigen = eigen_df, fit = fit)

    if (!is.null(roles$group) && nzchar(roles$group)) {
      s$group <- as.factor(df_sub[[roles$group]])
    }
  } else {
    validate(need(ncol(x) >= 1, "Need >=1 numeric active variable for LDA."))
    validate(need(!is.null(roles$group) && nzchar(roles$group), "Choose a grouping variable for LDA."))

    gf <- droplevels(as.factor(df_sub[[roles$group]]))
    validate(need(length(unique(gf)) >= 2, "Grouping variable needs >=2 classes for LDA."))
    validate(need(nrow(x) > ncol(x), "Need more observations than variables for LDA."))

    fit <- MASS::lda(gf ~ ., data = cbind(x, gf = gf), CV = FALSE)
    px <- predict(fit)
    n_ld <- min(3, ncol(px$x))

    lda_vals <- fit$svd^2
    lda_pct <- if (length(lda_vals) > 0) lda_vals / sum(lda_vals) * 100 else numeric(0)
    lda_axis_df <- data.frame(
      dataset_id = idv,
      axis_index = seq_along(lda_pct),
      pct = lda_pct,
      stringsAsFactors = FALSE
    )

    s <- as.data.frame(px$x[, 1:n_ld, drop = FALSE])
    while (ncol(s) < 3) s[[paste0("LD", ncol(s) + 1)]] <- 0
    names(s)[1:3] <- c("Axis1", "Axis2", "Axis3")

    x_mat <- as.matrix(x)
    scores_mat <- as.matrix(s[, c("Axis1", "Axis2", "Axis3"), drop = FALSE])
    sc_coefs <- compute_structure_coefficients(x_mat, scores_mat) # this is probably wrong

    l <- data.frame(
      x = sc_coefs[, 1],
      y = sc_coefs[, 2],
      z = sc_coefs[, 3],
      var = colnames(x),
      stringsAsFactors = FALSE
    )
    extra <- list(fit = fit, axis_pct = lda_axis_df)

    s$group <- as.factor(df_sub[[roles$group]])
  }

  s$dataset_id <- as.factor(idv)
  l$dataset_id <- as.factor(idv)

  supp1 <- project_supp_vectors(as.matrix(s[, c("Axis1", "Axis2")]), safe_numeric_df(df_sub, roles$supp))
  supp2 <- project_supp_vectors(as.matrix(s[, c("Axis1", "Axis3")]), safe_numeric_df(df_sub, roles$supp))
  supp3 <- project_supp_vectors(as.matrix(s[, c("Axis2", "Axis3")]), safe_numeric_df(df_sub, roles$supp))
  supp <- full_join(supp1 %>% rename(x = x, y = y), supp2 %>% rename(x13 = x, y13 = y), by = "var") %>%
    full_join(supp3 %>% rename(x23 = x, y23 = y), by = "var")

  if (nrow(supp) > 0) {
    supp <- supp %>%
      mutate(
        x = ifelse(is.na(x), 0, x),
        y = ifelse(is.na(y), 0, y),
        z = ifelse(is.na(y13), ifelse(is.na(y23), 0, y23), y13)
      ) %>%
      select(var, x, y, z)
    supp$dataset_id <- as.factor(idv)
  } else {
    supp <- data.frame(var = character(), x = numeric(), y = numeric(), z = numeric(), dataset_id = factor())
  }

  c(list(scores = s, loadings = l, supp = supp), extra)
}
