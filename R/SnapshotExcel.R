#' Create a Snapshot of an Excel Workbook
#'
#' @param filename Path to the Excel file.
#' @param algo Hash algorithm (default: "sha256").
#' @param sheets Vector of sheet names or indices to include. NULL = all sheets.
#' @param ignore_column_order If TRUE, column order is ignored when hashing.
#' @param verbose Logical. Show progress messages.
#' @param ... Further arguments passed to `read_excel_allsheets()`.
#'
#' @return A `tibble` with one row per sheet.
#' @export
snapshot_excel <- function(filename,
                           algo = "sha256",
                           sheets = NULL,
                           ignore_column_order = FALSE,
                           verbose = FALSE,
                           ...) {

  # -------------------------
  # INPUT VALIDATION
  # -------------------------
  if (!is.character(filename) || length(filename) != 1) {
    stop("`filename` must be a single character string.", call. = FALSE)
  }

  if (!file.exists(filename)) {
    stop("File does not exist: ", filename, call. = FALSE)
  }

  # -------------------------
  # READ WORKBOOK
  # -------------------------
  if (verbose) message("Creating snapshot for: ", basename(filename))

  book <- read_excel_allsheets(
    filename = filename,
    sheet_names = sheets,
    tibble = TRUE,
    verbose = verbose,
    ...
  )

  # -------------------------
  # HANDLE EMPTY RESULT
  # -------------------------
  if (length(book$sheets) == 0) {
    if (verbose) message("No valid sheets were read from the file.")
    return(
      tibble::tibble(
        sheet     = character(0),
        n_rows    = integer(0),
        n_cols    = integer(0),
        hash      = character(0),
        timestamp = as.POSIXct(character(0))
      )
    )
  }

  # -------------------------
  # BUILD SNAPSHOT
  # -------------------------
  sheet_names <- names(book$sheets)

  snapshot <- tibble::tibble(
    sheet = sheet_names,
    n_rows = unname(vapply(book$sheets, function(s) s$n_rows, integer(1))),
    n_cols = unname(vapply(book$sheets, function(s) s$n_cols, integer(1))),
    hash = unname(vapply(sheet_names, function(sheet_name) {
      file_fingerprint(
        path = filename,
        algo = algo,
        full_path = FALSE,
        sheet = sheet_name,
        ignore_column_order = ignore_column_order
      )$hash
    }, character(1))),
    timestamp = Sys.time()
  )

  if (verbose) {
    message("Snapshot created for ", nrow(snapshot), " sheet(s).")
  }

  snapshot
}
