visible_axes_controls <- function(prefix) {
  axis_choices <- if (prefix == "pca") {
    c("PC1 vs PC2" = "1_2", "PC2 vs PC3" = "2_3")
  } else {
    c("LD1 vs LD2" = "1_2", "LD2 vs LD3" = "2_3")
  }
}
