# ============================================================
# R/async_utils.R
# linkedCells: Async execution and promise helpers
# ============================================================

#' Generate unique execution token
#'
#' @keywords internal
generate_token <- function() {
  as.numeric(Sys.time()) + runif(1)
}

#' Check if token is still valid
#'
#' @keywords internal
is_token_active <- function(token, active_token) {
  identical(token, active_token)
}

#' Run computation asynchronously
#'
#' @keywords internal
run_async_compute <- function(expr, on_success, on_error) {
  
  future::future({
    expr
  }) %...>%
    on_success %...!%
    on_error
}

#' Safe async execution wrapper
#'
#' @keywords internal
with_async_safety <- function(expr, session, on_error = NULL) {
  
  tryCatch(
    {
      future::future({
        expr
      }, seed = TRUE) %...>%
        (function(result) {
          # Validate result structure
          if (!is.list(result) || !("data" %in% names(result))) {
            stop("link_fn must return list with 'data' element")
          }
          result
        }) %...!%
        (function(error) {
          shiny::showNotification(
            paste("Error:", error$message),
            type = "error",
            duration = 5,
            session = session
          )
          if (!is.null(on_error)) on_error(error)
          stop(error)
        })
    },
    error = function(e) {
      shiny::showNotification(
        paste("Async error:", e$message),
        type = "error",
        duration = 5,
        session = session
      )
      future::future(list(data = NULL, status = "error"))
    }
  )
}
