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

#' Run an expression asynchronously via future
#'
#' Wraps \code{future::future()} with error handling. On failure,
#' notifies the Shiny session and returns a resolved promise with
#' status = "error" so callers always receive a promise.
#'
#' @param expr Expression to evaluate in the future worker
#' @param session Shiny session (for error notifications)
#'
#' @return A promise that resolves to the expression result
#'
#' @keywords internal
with_async_safety <- function(expr, session) {

  parent_env <- parent.frame()
  expr_sub   <- substitute(expr)

  tryCatch(
    {
      future::future(eval(expr_sub, envir = parent_env), seed = TRUE)
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
