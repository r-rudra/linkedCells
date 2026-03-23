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

  # Capture expression lazily
  expr_sub <- substitute(expr)
  parent_env <- parent.frame()

  tryCatch(
    {

      promise <- future::future(
        {
          eval(expr_sub, envir = parent_env)
        },
        seed = TRUE
      )

      promise <- promises::then(
        promise,
        onFulfilled = function(result) {

          if (!is.list(result) ||
              !("data" %in% names(result)) ||
              !("status" %in% names(result))) {
            stop("link_fn must return list with 'data' and 'status'")
          }

          result
        }
      )

      promise <- promises::catch(
        promise,
        onRejected = function(error) {

          # Safe notification (session may be closed)
          if (!is.null(session) && !isTRUE(session$isClosed())) {
            shiny::showNotification(
              paste("Error:", error$message),
              type = "error",
              duration = 5,
              session = session
            )
          }

          if (!is.null(on_error)) {
            on_error(error)
          }

          stop(error$message, call. = FALSE)
        }
      )

      return(promise)

    },
    error = function(e) {

      # Safe notification
      if (!is.null(session) && !isTRUE(session$isClosed())) {
        shiny::showNotification(
          paste("Async error:", e$message),
          type = "error",
          duration = 5,
          session = session
        )
      }

      # Return a proper promise (not future)
      promises::promise_resolve(
        list(
          data = NULL,
          status = "error",
          message = e$message
        )
      )
    }
  )
}
