register_vec_lock_sync <- function(input, session, rv, prefix, flag_name) {
  lock_id <- paste0(prefix, "_lock_vec_scale")
  active_id <- paste0(prefix, "_vec_scale_active")
  supp_id <- paste0(prefix, "_vec_scale_supp")

  observeEvent(input[[lock_id]], {
    if (isTRUE(input[[lock_id]])) {
      updateSliderInput(session, supp_id, value = input[[active_id]])
    }
  }, ignoreInit = TRUE)

  observeEvent(input[[active_id]], {
    if (isTRUE(input[[lock_id]]) && !isTRUE(rv[[flag_name]])) {
      rv[[flag_name]] <- TRUE
      updateSliderInput(session, supp_id, value = input[[active_id]])
      rv[[flag_name]] <- FALSE
    }
  }, ignoreInit = TRUE)

  observeEvent(input[[supp_id]], {
    if (isTRUE(input[[lock_id]]) && !isTRUE(rv[[flag_name]])) {
      rv[[flag_name]] <- TRUE
      updateSliderInput(session, active_id, value = input[[supp_id]])
      rv[[flag_name]] <- FALSE
    }
  }, ignoreInit = TRUE)
}
