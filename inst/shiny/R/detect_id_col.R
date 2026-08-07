detect_id_col <- function(nms) {
  idx <- which(tolower(trimws(nms)) == "id")
  if (length(idx) == 0) NULL else nms[idx[1]]
}
