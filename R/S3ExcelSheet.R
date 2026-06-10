#' Create a New excel_sheet Object
#'
#' Internal constructor that creates an S3 object of class `excel_sheet`.
#' This object encapsulates the tabular data from an Excel sheet along
#' with relevant metadata for traceability and performance.
#'
#' @param data A `data.frame` or `tibble` containing the sheet content.
#' @param sheet Character scalar. Name of the Excel sheet.
#'
#' @return An S3 object of class `excel_sheet`.
#'
#' @details
#' The `excel_sheet` object stores:
#' - The raw data (`data`)
#' - Sheet name for traceability (`sheet`)
#' - Cached dimensions (`n_rows`, `n_cols`) for quick access
#'
#' This constructor is used internally by `read_excel_allsheets()`.
#'
#' @seealso [read_excel_allsheets()], [new_excel_book()], [print.excel_sheet()], [get_data()]
#'
#' @keywords internal
new_excel_sheet <- function(data, sheet) {
  # Input validation
  stopifnot(
    "data must be a data.frame or tibble" = is.data.frame(data),
    "sheet must be a single character string" = is.character(sheet) && length(sheet) == 1
  )

  # Create S3 object with cached metadata
  structure(
    list(
      data  = data,           # Core tabular data
      sheet = sheet,          # Original sheet name
      n_rows = nrow(data),    # Cached for performance
      n_cols = ncol(data)     # Cached for performance
    ),
    class = "excel_sheet"
  )
}


#' Print Method for excel_sheet
#'
#' Custom print method providing a clean and informative summary of
#' an `excel_sheet` object.
#'
#' @param x An object of class `excel_sheet`.
#' @param ... Further arguments (currently ignored).
#'
#' @export
print.excel_sheet <- function(x, ...) {
  cat("<excel_sheet>\n")
  cat("Sheet name :", x$sheet, "\n")
  cat("Dimensions :", x$n_rows, "rows ×", x$n_cols, "columns\n")

  # Show first few column names (if any)
  if (x$n_cols > 0) {
    col_names <- names(x$data)
    if (length(col_names) > 8) {
      col_names <- c(col_names[1:8], "...")
    }
    cat("Columns    :", paste(col_names, collapse = ", "), "\n")
  }

  invisible(x)
}


#' Coerce excel_sheet to data.frame
#'
#' S3 method to allow easy conversion of `excel_sheet` objects to
#' regular `data.frame` using `as.data.frame()`.
#'
#' @param x An object of class `excel_sheet`.
#' @param ... Further arguments (currently ignored).
#'
#' @return A `data.frame` containing the sheet data.
#'
#' @export
as.data.frame.excel_sheet <- function(x, ...) {
  x$data
}


#' Extract Data from excel_sheet
#'
#' Convenience function to explicitly extract the underlying data from
#' an `excel_sheet` object.
#'
#' @param x An object of class `excel_sheet`.
#'
#' @return The data stored in the sheet (data.frame or tibble).
#'
#' @export
get_data <- function(x) {
  if (!inherits(x, "excel_sheet")) {
    stop("`x` must be an object of class 'excel_sheet'", call. = FALSE)
  }
  x$data
}

