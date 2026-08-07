#' Plot pca scree plot
#'
#' Plot pca scree plot
#'
#' @export
#' @examples
#' plot_pca_scree(pca_result)
#' @import ggplot2
#' @import dplyr

plot_pca_scree <- function(pca_result, show_bkn_stck, legend_title = "ID") {
  df <- pca_result$eigen %>% mutate(PCn = as.integer(gsub("PC", "", PC)))
  if ("dataset_id" %in% colnames(df)) {
    p <- ggplot(df, aes(PCn, explained_var_pct, fill = dataset_id)) +
      geom_col(position = position_dodge(0.8), alpha = 0.85)
    if (show_bkn_stck) {
      p <- p + geom_line(aes(y = broken_stick_pct, color = dataset_id, group = dataset_id), linewidth = 1) +
        geom_point(aes(y = broken_stick_pct, color = dataset_id), size = 2) +
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
    p <- ggplot(df, aes(PCn, explained_var_pct)) +
      geom_col(position = position_dodge(0.8), alpha = 0.85, fill = '#F8766D')
    if (show_bkn_stck) {
      p <- p + geom_line(aes(y = broken_stick_pct), color = '#F8766D', linewidth = 1) +
        geom_point(aes(y = broken_stick_pct), color = '#F8766D', size = 2)
    }
    p <- p + scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      scale_x_continuous(breaks = df$PCn, labels = df$PC) +
      labs(x = "Principal component", y = "Explained variance %") +
      theme_light(base_size = 18)
  }
  return(p)
}
