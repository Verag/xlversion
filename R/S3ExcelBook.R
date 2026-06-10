#' Create a New excel_book Object
#'
#' Internal constructor for an `excel_book` S3 object.
#'
#' @param sheets A named list of `excel_sheet` objects. Can be empty.
#'
#' @return An S3 object of class `excel_book`.
#'
#' @keywords internal
new_excel_book <- function(sheets) {
  stopifnot(is.list(sheets))

  if (length(sheets) == 0) {
    structure(
      list(sheets = list()),           # sheets vazia
      class = "excel_book"
    )
  } else {
    if (is.null(names(sheets))) {
      names(sheets) <- paste0("sheet", seq_along(sheets))
    }
    structure(
      list(sheets = sheets),
      class = "excel_book"
    )
  }
}

#' Print Method for excel_book
#'
#' @param x An `excel_book` object.
#' @param ... Additional arguments (not used).
#'
#' @export
print.excel_book <- function(x, ...) {
  cat("<excel_book>\n")
  cat("Total sheets :", length(x$sheets), "\n")

  if (length(x$sheets) > 0) {
    cat("Sheet names  :", paste(names(x$sheets), collapse = ", "), "\n")
  } else {
    cat("Sheet names  : (none)\n")
  }

  if (!is.null(attr(x, "failed_sheets"))) {
    cat("Failed sheets:", length(attr(x, "failed_sheets")), "\n")
  }

  invisible(x)
}


#' Extract a Specific Sheet from an excel_book
#'
#' @param book An `excel_book` object.
#' @param name Character scalar. Name of the sheet.
#'
#' @export
get_sheet <- function(book, name) {
  if (!inherits(book, "excel_book")) {
    stop("`book` must be an object of class 'excel_book'", call. = FALSE)
  }

  if (!is.character(name) || length(name) != 1) {
    stop("`name` must be a single character string.", call. = FALSE)
  }

  sheet <- book$sheets[[name]]

  if (is.null(sheet)) {
    warning("Sheet '", name, "' not found in the excel_book.", call. = FALSE)
  }

  sheet
}
