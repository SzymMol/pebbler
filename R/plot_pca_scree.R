#' Plot PCA scree
#'
#' Plot PCA scree plot.
#' Scree plot is a barplot which displays PCs in the x-axis
#' and percent of explained variance in the y-axis.
#'
#' @param pca_result the output of \code{\link{run_pca}}
#' @param show_bkn_stck logical parameter; if \code{TRUE} broken stick line is shown, otherwise it is hidden
#' @param legend_title legend title (if \code{pca_result} contains at least two datasets)
#' @returns ggplot object
#' @examples
#' data('witow', package = 'pebbler')
#' dat <- witow
#' roles <- list(active = c('a', 'b', 'c', 'mass'),
#'               group = 'lithotype',
#'               supp = 'hgt_min')
#' pca_result <- run_pca(x_active = scale(dat[roles$active]),
#'                       x_supp = dat[roles$supp],
#'                       y_group = dat[roles$group],
#'                       flip_cols = rep(FALSE, length(roles$active))
#'                       )
#' plot_pca_scree(pca_result, TRUE)
#' @export
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang .data

plot_pca_scree <- function(pca_result, show_bkn_stck, legend_title = "ID") {
  df <- pca_result$eigen %>% mutate(PCn = as.integer(gsub("PC", "", .data$PC)))
  if ("dataset_id" %in% colnames(df)) {
    p <- ggplot(df, aes(.data$PCn, .data$explained_var_pct, fill = .data$dataset_id)) +
      geom_col(position = position_dodge(0.8), alpha = 0.85)
    if (show_bkn_stck) {
      p <- p + geom_line(aes(y = .data$broken_stick_pct, color = .data$dataset_id, group = .data$dataset_id), linewidth = 1) +
        geom_point(aes(y = .data$broken_stick_pct, color = .data$dataset_id), size = 2) +
        labs(x = "Principal component", y = "Explained variance %", fill = legend_title, color = legend_title)
    }
    else {
      p <- p + labs(x = "Principal component", y = "Explained variance %", fill = legend_title)
    }
    p <- p + scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      scale_x_continuous(breaks = df$PCn, labels = df$PC) +
      theme_light(base_size = 18)
  }
  else {
    p <- ggplot(df, aes(.data$PCn, .data$explained_var_pct)) +
      geom_col(position = position_dodge(0.8), alpha = 0.85, fill = '#F8766D')
    if (show_bkn_stck) {
      p <- p + geom_line(aes(y = .data$broken_stick_pct), color = '#F8766D', linewidth = 1) +
        geom_point(aes(y = .data$broken_stick_pct), color = '#F8766D', size = 2)
    }
    p <- p + scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      scale_x_continuous(breaks = df$PCn, labels = df$PC) +
      labs(x = "Principal component", y = "Explained variance %") +
      theme_light(base_size = 18)
  }
  return(p)
}
