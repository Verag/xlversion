#' Create a File Fingerprint for Versioning
#'
#' Computes metadata and content hash of a file or a specific Excel sheet.
#' Designed to work together with `read_excel_allsheets()` and `snapshot_excel()`.
#'
#' @param path Character scalar. Path to the file.
#' @param algo Character scalar. Hash algorithm (default: "sha256").
#' @param full_path Logical. If `TRUE`, returns the full normalized path.
#' @param sheet Optional. Sheet name or index (only applicable for Excel files).
#' @param ignore_column_order Logical. If `TRUE`, ignores column order when
#'   hashing Excel sheets (useful when columns may be reordered).
#'
#' @return A one-row `tibble` with file/sheet metadata and hash.
#'
#' @seealso [snapshot_excel()], [read_excel_allsheets()]
#'
#' @export
file_fingerprint <- function(path,
                             algo = "sha256",
                             full_path = TRUE,
                             sheet = NULL,
                             ignore_column_order = FALSE) {

  if (!is.character(path) || length(path) != 1) {
    stop("`path` must be a single character string.", call. = FALSE)
  }

  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }

  if (file.info(path)$isdir) {
    stop("`path` must be a file, not a directory.", call. = FALSE)
  }

  # Excel sheet fingerprint
  if (!is.null(sheet) && grepl("\\.xlsx?$", basename(path), ignore.case = TRUE)) {
    .fingerprint_excel_sheet(
      path = path,
      sheet = sheet,
      algo = algo,
      ignore_column_order = ignore_column_order,
      full_path = full_path
    )
  } else {
    # Whole file fingerprint
    info <- file.info(path)

    tibble::tibble(
      file_name   = basename(path),
      file_path   = if (full_path) normalizePath(path, winslash = "/") else path,
      size_bytes  = info$size,
      mtime       = info$mtime,
      hash        = digest::digest(file = path, algo = algo),
      timestamp   = Sys.time(),
      sheet       = NA_character_
    )
  }
}


# Internal helper: Fingerprint of a specific Excel sheet
.fingerprint_excel_sheet <- function(path,
                                     sheet,
                                     algo = "sha256",
                                     ignore_column_order = FALSE,
                                     full_path = TRUE) {

  # Use the existing architecture instead of calling readxl directly
  book <- read_excel_allsheets(
    filename = path,
    sheet_names = sheet,
    tibble = TRUE,
    verbose = FALSE
  )

  if (length(book$sheets) == 0) {
    stop("Could not read sheet '", sheet, "' from file.", call. = FALSE)
  }

  sheet_obj <- book$sheets[[1]]

  df <- get_data(sheet_obj)   # Use the helper for consistency

  # Ignore column order if requested
  if (ignore_column_order) {
    df <- df[, sort(names(df)), drop = FALSE]
  }

  # Convert to matrix for stable hashing
  mat <- as.matrix(df)
  mat[is.na(mat)] <- "<NA>"

  hash_value <- digest::digest(mat, algo = algo)

  info <- file.info(path)

  tibble::tibble(
    file_name   = basename(path),
    file_path   = if (full_path) normalizePath(path, winslash = "/") else path,
    size_bytes  = info$size,
    mtime       = info$mtime,
    hash        = hash_value,
    timestamp   = Sys.time(),
    sheet       = as.character(sheet)
  )
}
