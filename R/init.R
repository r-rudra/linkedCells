# ============================================================
# R/init.R
# linkedCells: Global initialization and async management
# ============================================================

.linkedcells_env <- new.env(parent = emptyenv())
.linkedcells_env$async_initialized <- FALSE
.linkedcells_env$async_plan <- NULL

#' Initialize Async Computation for linkedCells
#'
#' Sets up async backend. Safe to call multiple times.
#' Internal code also uses this function (single source of truth).
#'
#' @param plan Future plan (default: future::multisession)
#' @param force Force re-initialization (default: FALSE)
#'
#' @export
linked_cells_init <- function(plan = NULL, force = FALSE) {

  if (!requireNamespace("future", quietly = TRUE)) {
    stop("Package 'future' is required for async initialization")
  }

  # Already initialized
  if (isTRUE(.linkedcells_env$async_initialized) && !force) {
    return(invisible(TRUE))
  }

  if (is.null(plan)) {
    plan <- future::multisession
  }

  if (!is.function(plan) && !is.character(plan)) {
    stop("plan must be a function or character")
  }

  tryCatch(
    {
      # Reset and set plan (safer)
      future::plan(future::sequential)
      future::plan(plan)

      .linkedcells_env$async_initialized <- TRUE
      .linkedcells_env$async_plan <- plan

      invisible(TRUE)
    },
    error = function(e) {
      warning(sprintf("Failed to initialize async: %s", e$message), call. = FALSE)
      invisible(FALSE)
    }
  )
}

