# ============================================================
# R/init.R
# linkedCells: Global initialization and async management
# ============================================================

# Internal environment for package state (CRAN-safe)
.linkedcells_env <- new.env(parent = emptyenv())
.linkedcells_env$async_initialized <- FALSE
.linkedcells_env$async_plan <- NULL

#' Initialize Async Computation for linkedCells
#'
#' Sets up the async execution backend for constraint propagation.
#' Safe to call multiple times. If not called, auto-initializes on first use.
#'
#' @param plan Future plan (default: future::multisession).
#'   Options: future::sequential, future::multisession, future::multicore
#'
#' @export
linked_cells_init <- function(plan = NULL) {
  
  if (is.null(plan)) {
    plan <- future::multisession
  }
  
  tryCatch(
    {
      future::plan(plan)
      .linkedcells_env$async_initialized <- TRUE
      .linkedcells_env$async_plan <- plan
      
      message("✓ linkedCells async initialized")
      invisible(TRUE)
    },
    error = function(e) {
      warning("Failed to initialize async: ", e$message)
      invisible(FALSE)
    }
  )
}

#' Ensure Async Initialization
#'
#' @keywords internal
ensure_async_ready <- function() {
  
  if (isTRUE(.linkedcells_env$async_initialized)) {
    return(TRUE)
  }
  
  tryCatch(
    {
      future::plan(future::multisession)
      .linkedcells_env$async_initialized <- TRUE
      .linkedcells_env$async_plan <- future::multisession
      return(FALSE)
    },
    error = function(e) {
      warning("Auto-init async failed: ", e$message)
      return(FALSE)
    }
  )
}

#' Get Async State
#'
#' @keywords internal
get_async_state <- function() {
  list(
    initialized = .linkedcells_env$async_initialized,
    plan = .linkedcells_env$async_plan
  )
}
