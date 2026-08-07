pub_theme <- function(base_size = 18, axis_font_size = AXIS_FONT_SIZE) {
  theme_minimal(base_size = base_size) +
    theme(
      axis.title = element_text(size = axis_font_size),
      axis.text = element_text(size = axis_font_size),
      plot.title = element_text(size = base_size + 2),
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size - 1),
      panel.grid.minor = element_line(color = "grey85", linewidth = 0.4),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.6)
    )
}
