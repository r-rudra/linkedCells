# ============================================================
# R/detect_changes.R
# linkedCells: Detect which cells changed (type-safe)
# ============================================================

#' Detect Changes Between DataFrames
#'
#' Identifies cells that changed between old and new dataframes.
#' Column-by-column comparison (type-safe, no matrix coercion).
#'
#' @param data_old Original dataframe
#' @param data_new Updated dataframe
#' @param num_locked_rows Number of locked rows at top (default: 0)
#' @param tolerance Numeric tolerance for floating-point comparison (default: 0.1)
#'
#' @return data.frame with columns: row (integer), col (integer)
#'
#' @keywords internal
detect_changes <- function(data_old, data_new, num_locked_rows = 0, tolerance = 0.1) {
  
  # Validation
  if (!is.data.frame(data_old) || !is.data.frame(data_new)) {
    stop("Both inputs must be dataframes")
  }
  
  if (nrow(data_old) != nrow(data_new) || ncol(data_old) != ncol(data_new)) {
    stop("DataFrames must have same dimensions")
  }
  
  if (num_locked_rows < 0 || num_locked_rows >= nrow(data_new)) {
    stop("num_locked_rows must be 0 <= n < nrow(data_new)")
  }
  
  if (tolerance < 0) {
    stop("tolerance must be >= 0")
  }
  
  # Initialize result
  changes <- data.frame(row = integer(0), col = integer(0))
  
  first_editable_row <- num_locked_rows + 1
  if (first_editable_row > nrow(data_old)) {
    return(changes)
  }
  
  # Column-by-column comparison (TYPE-SAFE)
  for (col_idx in seq_len(ncol(data_old))) {
    
    old_col <- data_old[first_editable_row:nrow(data_old), col_idx]
    new_col <- data_new[first_editable_row:nrow(data_old), col_idx]
    
    # Detect changes based on column type
    if (is.numeric(old_col) && is.numeric(new_col)) {
      # Numeric: use tolerance
      diff <- abs(old_col - new_col)
      changed_indices <- which(diff > tolerance)
      
    } else if (is.character(old_col) && is.character(new_col)) {
      # Character: exact match
      changed_indices <- which(old_col != new_col)
      
    } else {
      # Mixed or other types: direct comparison
      changed_indices <- which(old_col != new_col)
    }
    
    # Add to result
    if (length(changed_indices) > 0) {
      actual_rows <- changed_indices + num_locked_rows
      changes <- rbind(
        changes,
        data.frame(
          row = as.integer(actual_rows),
          col = as.integer(rep(col_idx, length(changed_indices))),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  return(changes)
}

#' Build edits dataframe
#'
#' @keywords internal
make_edits_df <- function(row_idx, col_idx) {
  data.frame(
    row = as.integer(row_idx),
    col = as.integer(col_idx),
    stringsAsFactors = FALSE
  )
}
