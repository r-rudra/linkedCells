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
detect_changes <- function(data_old, data_new, num_locked_rows = 0L, tolerance = 0.1) {

  # -------------------------
  # Validation
  # -------------------------
  if (!is.data.frame(data_old) || !is.data.frame(data_new)) {
    stop("Both inputs must be dataframes")
  }

  if (nrow(data_old) != nrow(data_new) ||
      ncol(data_old) != ncol(data_new)) {
    stop("DataFrames must have same dimensions")
  }

  if (num_locked_rows < 0L || num_locked_rows >= nrow(data_new)) {
    stop("num_locked_rows must be 0 <= n < nrow(data_new)")
  }

  if (tolerance < 0) {
    stop("tolerance must be >= 0")
  }

  # -------------------------
  # Early exit
  # -------------------------
  first_row <- num_locked_rows + 1L
  n <- nrow(data_old)

  if (first_row > n) {
    return(data.frame(row = integer(0), col = integer(0)))
  }

  row_idx <- first_row:n

  # -------------------------
  # Column-wise detection
  # -------------------------
  res_list <- lapply(seq_len(ncol(data_old)), function(col_idx) {

    old_col <- data_old[row_idx, col_idx]
    new_col <- data_new[row_idx, col_idx]

    if (is.numeric(old_col) && is.numeric(new_col)) {

      diff <- abs(old_col - new_col)

      changed <- (diff > tolerance) |
        (is.na(old_col) != is.na(new_col))

    } else {

      changed <- (old_col != new_col) |
        (is.na(old_col) != is.na(new_col))
    }

    idx <- which(changed)

    if (length(idx) == 0L) return(NULL)

    data.frame(
      row = as.integer(idx + num_locked_rows),
      col = as.integer(rep.int(col_idx, length(idx))),
      stringsAsFactors = FALSE
    )
  })

  # -------------------------
  # Combine
  # -------------------------
  res_list <- Filter(Negate(is.null), res_list)

  if (length(res_list) == 0L) {
    return(data.frame(row = integer(0), col = integer(0)))
  }

  out <- do.call(rbind, res_list)
  rownames(out) <- NULL

  out
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
