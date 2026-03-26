# ============================================================
# R/async_utils.R
# linkedCells: Async execution and promise helpers
# ============================================================

#' Generate unique execution token
#'
#' @keywords internal
generate_token <- function() {
  sprintf("%.15f-%d", as.numeric(Sys.time()), sample.int(.Machine$integer.max, 1L))
}

#' Check if token is still valid
#'
#' @keywords internal
is_token_active <- function(token, active_token) {
  identical(token, active_token)
}

#' Safe async execution wrapper
#'
#' @keywords internal
with_async_safety <- function(expr, session, on_error = NULL) {

  # Evaluate link_fn SYNCHRONOUSLY in the caller's env to get the data,
  # then ship only data into the future worker
  parent_env <- parent.frame()
  expr_sub   <- substitute(expr)

  # Build the actual call args in the main process so the future
  # worker only receives plain data (no environments / reactives)
  call_result <- tryCatch(
    {
      # We need the evaluated arguments but run link_fn in the future.
      # Strategy: evaluate everything except the heavy call here,
      # then run the call in the future with plain data.
      #
      # Actually the simplest correct fix: just run future with
      # the expression directly, no substitute/eval tricks.
      p <- future::future(
        eval(expr_sub, envir = parent_env),
        seed = TRUE
      )
      return(p)
    },
    error = function(e) {
      if (!is.null(session) && !isTRUE(session$isClosed())) {
        shiny::showNotification(
          paste("Async error:", e$message),
          type = "error", duration = 5, session = session
        )
      }
      promises::promise_resolve(
        list(data = NULL, status = "error", message = e$message)
      )
    }
  )
}
