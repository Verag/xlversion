#' Read All Sheets from an Excel Workbook
#'
#' Reads all sheets (or a selected subset) from an Excel workbook and returns
#' a structured `excel_book` S3 object.
#'
#' This function provides a robust abstraction layer over `readxl` and
#' `openxlsx`, with explicit validation, isolated sheet-level error handling,
#' and production-grade failure semantics suitable for audit and regulated
#' environments.
#'
#' @param filename Character scalar. Full path to the Excel workbook.
#'
#' @param engine Character scalar. Reading backend.
#'   Must be one of:
#'   - `"readxl"` (default)
#'   - `"openxlsx"`
#'
#' @param sheet_names Optional sheet selection.
#'   Accepts:
#'   - `NULL` = all sheets
#'   - character vector of sheet names
#'   - numeric vector of sheet positions (1-based)
#'
#' @param guess_max Integer scalar. Maximum rows used for type guessing
#'   with the `readxl` engine.
#'
#' @param parallel Logical.
#'   Reserved for future parallel execution support.
#'
#' @param tibble Logical.
#'   If `TRUE`, keeps tibbles returned by `readxl`.
#'   Otherwise converts to base `data.frame`.
#'
#' @param verbose Logical.
#'   If `TRUE`, prints progress information.
#'
#' @param return_failed Logical.
#'   If `TRUE`, attaches failed sheet information as attribute
#'   `"failed_sheets"`.
#'
#' @param ... Additional arguments passed to:
#'   - `readxl::read_excel()`
#'   - `openxlsx::read.xlsx()`
#'
#' @details
#' The function performs:
#'
#' 1. Input validation
#' 2. Workbook structure validation
#' 3. Sheet discovery
#' 4. Sheet selection/filtering
#' 5. Independent sheet reads with isolated failures
#' 6. S3 object construction
#'
#' Workbook-level corruption is treated as a fatal error.
#'
#' Sheet-level failures are isolated and optionally attached
#' to the returned object.
#'
#' @return An object of class `excel_book`.
#'
#' Each element is an `excel_sheet` object.
#'
#' Failed sheets may be attached as:
#'
#' ```r
#' attr(book, "failed_sheets")
#' ```
#'
#' @seealso
#' [snapshot_excel()]
#' [file_fingerprint()]
#'
#' @export
read_excel_allsheets <- function(
    filename,
    engine = "readxl",
    sheet_names = NULL,
    guess_max = 10000,
    parallel = FALSE,
    tibble = FALSE,
    verbose = FALSE,
    return_failed = FALSE,
    ...
) {

  # ---------------------------------------------------------------------------
  # INPUT VALIDATION
  # ---------------------------------------------------------------------------

  if (!is.character(filename) || length(filename) != 1) {
    stop(
      "`filename` must be a single character string.",
      call. = FALSE
    )
  }

  if (!file.exists(filename)) {
    stop(
      "File does not exist: ",
      filename,
      call. = FALSE
    )
  }

  if (!engine %in% c("readxl", "openxlsx")) {
    stop(
      "`engine` must be either 'readxl' or 'openxlsx'.",
      call. = FALSE
    )
  }

  if (!is.logical(parallel) || length(parallel) != 1) {
    stop(
      "`parallel` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(tibble) || length(tibble) != 1) {
    stop(
      "`tibble` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(verbose) || length(verbose) != 1) {
    stop(
      "`verbose` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.logical(return_failed) || length(return_failed) != 1) {
    stop(
      "`return_failed` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (!is.numeric(guess_max) ||
      length(guess_max) != 1 ||
      is.na(guess_max) ||
      guess_max < 1) {

    stop(
      "`guess_max` must be a positive integer.",
      call. = FALSE
    )
  }

  if (parallel) {
    stop(
      "`parallel = TRUE` is not yet implemented.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # DEPENDENCY VALIDATION
  # ---------------------------------------------------------------------------

  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop(
      "Package 'purrr' is required.",
      call. = FALSE
    )
  }

  if (engine == "readxl") {

    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop(
        "Package 'readxl' is required.",
        call. = FALSE
      )
    }

  } else {

    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop(
        "Package 'openxlsx' is required.",
        call. = FALSE
      )
    }
  }

  # ---------------------------------------------------------------------------
  # WORKBOOK DISCOVERY
  # ---------------------------------------------------------------------------

  if (verbose) {
    message(
      "Discovering sheets in: ",
      basename(filename)
    )
  }

  sheets_all <- tryCatch({

    if (engine == "readxl") {

      readxl::excel_sheets(filename)

    } else {

      openxlsx::getSheetNames(filename)
    }

  }, error = function(e) {

    stop(
      paste0(
        "Failed to read workbook structure: ",
        conditionMessage(e)
      ),
      call. = FALSE
    )
  })

  # ---------------------------------------------------------------------------
  # WORKBOOK STRUCTURE VALIDATION
  # ---------------------------------------------------------------------------

  if (length(sheets_all) == 0) {
    stop(
      "Workbook contains no readable sheets.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # SHEET SELECTION
  # ---------------------------------------------------------------------------

  if (!is.null(sheet_names)) {

    if (!(is.character(sheet_names) || is.numeric(sheet_names))) {
      stop(
        "`sheet_names` must be NULL, character, or numeric.",
        call. = FALSE
      )
    }

    if (is.numeric(sheet_names)) {

      if (any(is.na(sheet_names))) {
        stop(
          "`sheet_names` contains NA values.",
          call. = FALSE
        )
      }

      if (any(sheet_names < 1)) {
        stop(
          "Sheet indices must be >= 1.",
          call. = FALSE
        )
      }

      if (any(sheet_names > length(sheets_all))) {
        stop(
          "Sheet index out of bounds.",
          call. = FALSE
        )
      }

      sheet_names <- sheets_all[sheet_names]
    }

    missing_sheets <- setdiff(
      sheet_names,
      sheets_all
    )

    if (length(missing_sheets) > 0) {

      warning(
        paste(
          "Sheets not found:",
          paste(missing_sheets, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    sheets <- intersect(
      sheet_names,
      sheets_all
    )

  } else {

    sheets <- sheets_all
  }

  # ---------------------------------------------------------------------------
  # EMPTY SELECTION
  # ---------------------------------------------------------------------------

  if (length(sheets) == 0) {

    warning(
      "No sheets matched the requested selection.",
      call. = FALSE
    )

    return(
      new_excel_book(list())
    )
  }

  # ---------------------------------------------------------------------------
  # SHEET ERROR CONSTRUCTOR
  # ---------------------------------------------------------------------------

  new_excel_sheet_error <- function(sheet, message) {

    structure(
      list(
        sheet = sheet,
        error = message,
        timestamp = Sys.time()
      ),
      class = "excel_sheet_error"
    )
  }

  # ---------------------------------------------------------------------------
  # SINGLE SHEET READER
  # ---------------------------------------------------------------------------

  read_one <- function(sh) {

    tryCatch({

      df <- if (engine == "readxl") {

        readxl::read_excel(
          path = filename,
          sheet = sh,
          guess_max = guess_max,
          ...
        )

      } else {

        openxlsx::read.xlsx(
          xlsxFile = filename,
          sheet = sh,
          ...
        )
      }

      if (!tibble) {

        df <- as.data.frame(
          df,
          stringsAsFactors = FALSE
        )
      }

      if (verbose) {

        message(
          "   ✓ ",
          sh,
          " [",
          nrow(df),
          " rows, ",
          ncol(df),
          " cols]"
        )
      }

      new_excel_sheet(
        data = df,
        sheet = sh
      )

    }, error = function(e) {

      if (verbose) {

        message(
          "   ✗ ",
          sh,
          " → Failed: ",
          conditionMessage(e)
        )
      }

      new_excel_sheet_error(
        sheet = sh,
        message = conditionMessage(e)
      )
    })
  }

  # ---------------------------------------------------------------------------
  # EXECUTION
  # ---------------------------------------------------------------------------

  if (verbose) {

    message(
      "Reading ",
      length(sheets),
      " sheet(s)..."
    )
  }

  res <- lapply(
    sheets,
    read_one
  )

  names(res) <- sheets

  # ---------------------------------------------------------------------------
  # RESULT SEPARATION
  # ---------------------------------------------------------------------------

  failed <- purrr::keep(
    res,
    ~ inherits(.x, "excel_sheet_error")
  )

  success <- purrr::discard(
    res,
    ~ inherits(.x, "excel_sheet_error")
  )

  # ---------------------------------------------------------------------------
  # FINAL VALIDATION
  # ---------------------------------------------------------------------------

  if (length(success) == 0) {

    stop(
      "No valid sheets could be read from workbook.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # BUILD BOOK
  # ---------------------------------------------------------------------------

  book <- new_excel_book(success)

  # ---------------------------------------------------------------------------
  # ATTACH FAILURE METADATA
  # ---------------------------------------------------------------------------

  if (return_failed && length(failed) > 0) {

    attr(book, "failed_sheets") <- failed
  }

  # ---------------------------------------------------------------------------
  # FINAL REPORTING
  # ---------------------------------------------------------------------------

  if (verbose) {

    message(
      "Completed: ",
      length(success),
      "/",
      length(sheets),
      " sheets read successfully."
    )

    if (length(failed) > 0) {

      warning(
        length(failed),
        " sheet(s) failed to read.",
        call. = FALSE
      )
    }
  }

  # ---------------------------------------------------------------------------
  # RETURN
  # ---------------------------------------------------------------------------

  book
}
