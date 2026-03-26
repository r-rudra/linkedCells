# ============================================================
# R/io_handlers.R
# linkedCells: Excel download / upload helpers
#
# Requires: openxlsx (write), readxl (read)
# Both are listed in Suggests — functionality is gated by
# io_packages_ready().
# ============================================================

#' Check if IO packages are available
#'
#' Returns TRUE only when both openxlsx and readxl are installed.
#'
#' @keywords internal
io_packages_ready <- function() {
  pkg_is_available("openxlsx") && pkg_is_available("readxl")
}

#' Write data.frame to Excel
#'
#' @param data data.frame to write
#' @param file Output file path
#'
#' @keywords internal
write_linked_cells_xlsx <- function(data, file) {
  openxlsx::write.xlsx(data, file, asTable = TRUE, sheetName = "Data")
}

#' Read Excel file as data.frame
#'
#' @param file Path to .xlsx file
#'
#' @return data.frame
#'
#' @keywords internal
read_linked_cells_xlsx <- function(file) {
  as.data.frame(readxl::read_excel(file))
}

#' Validate uploaded column names against expected names
#'
#' @param uploaded_names Column names from uploaded file
#' @param expected_names Column names from current data
#'
#' @return NULL if columns match, otherwise a descriptive string
#'
#' @keywords internal
validate_upload_columns <- function(uploaded_names, expected_names) {
  if (identical(uploaded_names, expected_names)) return(NULL)

  missing_cols <- setdiff(expected_names, uploaded_names)
  extra_cols   <- setdiff(uploaded_names, expected_names)

  parts <- character(0)
  if (length(missing_cols) > 0L) {
    parts <- c(parts, paste("Missing:", paste(missing_cols, collapse = ", ")))
  }
  if (length(extra_cols) > 0L) {
    parts <- c(parts, paste("Extra:", paste(extra_cols, collapse = ", ")))
  }

  paste(parts, collapse = ". ")
}

#' Coerce uploaded columns to match reference types
#'
#' @param uploaded data.frame from upload
#' @param reference data.frame with target column types
#'
#' @return data.frame with coerced types
#'
#' @keywords internal
coerce_upload_types <- function(uploaded, reference) {
  as.data.frame(
    Map(function(upl_col, ref_col) {
      if (is.numeric(ref_col))        as.numeric(upl_col)
      else if (is.character(ref_col))  as.character(upl_col)
      else if (is.logical(ref_col))   as.logical(upl_col)
      else if (is.integer(ref_col))   as.integer(upl_col)
      else upl_col
    }, uploaded, reference),
    stringsAsFactors = FALSE
  )
}
