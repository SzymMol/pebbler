add_sector_with_label <- function(pl, metrics, source_id = NULL) {
  if (is.null(metrics)) return(pl)

  a0 <- metrics$theta_ref
  a1 <- metrics$theta_ref + metrics$dtheta
  n <- 120
  th <- seq(a0, a1, length.out = n)
  r_sector <- 0.33 * min(metrics$nr, metrics$nh)
  if (!is.finite(r_sector) || r_sector <= 0) return(pl)

  x_poly <- c(0, r_sector * cos(th), 0)
  y_poly <- c(0, r_sector * sin(th), 0)

  pl <- add_trace(
    pl,
    type = "scatter",
    mode = "lines",
    x = x_poly,
    y = y_poly,
    fill = "toself",
    fillcolor = "rgba(0, 170, 0, 0.25)",
    line = list(color = "rgba(0, 150, 0, 0.55)", width = 1.5),
    hoverinfo = "skip",
    showlegend = FALSE,
    inherit = FALSE
    # source = source_id
  )

  amid <- metrics$theta_ref + metrics$dtheta / 2
  rlab <- r_sector * 0.62
  xl <- rlab * cos(amid)
  yl <- rlab * sin(amid)

  pl <- add_trace(
    pl,
    type = "scatter",
    mode = "text",
    x = xl, y = yl,
    text = paste0("<b>cos(θ)=", sprintf("%.2f", metrics$cs), "</b>"),
    textfont = list(color = "darkgreen", size = 24),
    hoverinfo = "skip",
    showlegend = FALSE,
    inherit = FALSE
    # source = source_id
  )

  pl
}
