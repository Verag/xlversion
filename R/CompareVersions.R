#' Compare Versions of Excel Files or Sheets
#'
#' Compares two Excel files (or selected sheets) and identifies structural
#' and content-level changes using deterministic sheet hashing.
#'
#' Designed for audit, regulatory, and version-control workflows in
#' production-grade environments.
#'
#' @param old_path Character scalar.
#' Path to the original (baseline) Excel file.
#'
#' @param new_path Character scalar.
#' Path to the new Excel file version.
#'
#' @param old_sheets Optional vector of sheets to include from the old file.
#' `NULL` means all sheets.
#'
#' Accepts:
#'
#' - Character vector of sheet names
#' - Numeric vector of sheet indices
#'
#' @param new_sheets Optional vector of sheets to include from the new file.
#' `NULL` means all sheets.
#'
#' Accepts:
#'
#' - Character vector of sheet names
#' - Numeric vector of sheet indices
#'
#' @param ignore_column_order Logical scalar.
#'
#' If `TRUE`, column order is ignored during hash computation.
#'
#' Recommended only when column ordering is not semantically relevant.
#'
#' Default:
#'
#' `FALSE`
#'
#' @param only_changes Logical scalar.
#'
#' If `TRUE`, returns only sheets with detected changes.
#'
#' Default:
#'
#' `FALSE`
#'
#' @param algo Character scalar.
#'
#' Hash algorithm used for fingerprint generation.
#'
#' Supported values:
#'
#' - `"sha256"` (recommended)
#' - `"md5"`
#' - `"sha1"`
#'
#' @param verbose Logical scalar.
#'
#' If `TRUE`, displays progress and summary information.
#'
#' @param quiet Logical scalar.
#'
#' If `TRUE`, suppresses all console output.
#'
#' Useful for batch pipelines and CI environments.
#'
#' @param ... Additional arguments passed to [snapshot_excel()].
#'
#' @details
#'
#' The comparison process:
#'
#' 1. Generates snapshots for both Excel files
#' 2. Computes deterministic sheet-level hashes
#' 3. Joins snapshots by sheet name
#' 4. Detects:
#'
#' - New sheets
#' - Deleted sheets
#' - Structural changes
#' - Content changes
#'
#' Structural changes are inferred using row/column count deltas.
#'
#' Content changes are inferred using hash differences.
#'
#' @return
#'
#' A [`tibble`][tibble::tibble] with one row per sheet comparison.
#'
#' Main output columns include:
#'
#' - `sheet`
#' - `status`
#' - `change_type`
#' - `changed`
#' - `rows_diff`
#' - `cols_diff`
#' - `hash_old`
#' - `hash_new`
#'
#' @section Status Values:
#'
#' Possible values for `status`:
#'
#' - `"Unchanged"`
#' - `"Content changed"`
#' - `"Structure changed"`
#' - `"New sheet"`
#' - `"Deleted sheet"`
#'
#' @family excel comparison
#'
#' @seealso
#' - [snapshot_excel()]
#' - [read_excel_allsheets()]
#' - [file_fingerprint()]
#'
#' @examples
#' \dontrun{
#'
#' compare_excel_versions(
#'   old_path = "baseline.xlsx",
#'   new_path = "updated.xlsx"
#' )
#'
#' compare_excel_versions(
#'   old_path = "v1.xlsx",
#'   new_path = "v2.xlsx",
#'   only_changes = TRUE
#' )
#'
#' compare_excel_versions(
#'   old_path = "old.xlsx",
#'   new_path = "new.xlsx",
#'   ignore_column_order = TRUE
#' )
#'
#' }
#'
#' @export
compare_excel_versions <- function(
    old_path,
    new_path,
    old_sheets = NULL,
    new_sheets = NULL,
    ignore_column_order = FALSE,
    only_changes = FALSE,
    algo = "sha256",
    verbose = TRUE,
    quiet = FALSE,
    ...
) {

  # -------------------------
  # INPUT VALIDATION
  # -------------------------

  checkmate::assert_string(old_path)

  checkmate::assert_string(new_path)

  checkmate::assert_file_exists(
    old_path,
    extension = c("xlsx", "xls")
  )

  checkmate::assert_file_exists(
    new_path,
    extension = c("xlsx", "xls")
  )

  checkmate::assert_flag(
    ignore_column_order
  )

  checkmate::assert_flag(
    only_changes
  )

  checkmate::assert_flag(
    verbose
  )

  checkmate::assert_flag(
    quiet
  )

  algo <- rlang::arg_match(
    algo,
    c("sha256", "md5", "sha1")
  )

  # -------------------------
  # CONSOLE HEADER
  # -------------------------

  if (verbose && !quiet) {

    cli::cli_h1(
      "Excel Version Comparison"
    )

    cli::cli_alert_info(
      "Old file: {.file {basename(old_path)}}"
    )

    cli::cli_alert_info(
      "New file: {.file {basename(new_path)}}"
    )
  }

  # -------------------------
  # SNAPSHOT GENERATION
  # -------------------------

  old_snap <- tryCatch(

    snapshot_excel(
      filename = old_path,
      algo = algo,
      sheets = old_sheets,
      ignore_column_order = ignore_column_order,
      verbose = verbose && !quiet,
      ...
    ),

    error = function(e) {

      cli::cli_abort(
        "Failed to generate snapshot for old file: {e$message}"
      )
    }
  )

  new_snap <- tryCatch(

    snapshot_excel(
      filename = new_path,
      algo = algo,
      sheets = new_sheets,
      ignore_column_order = ignore_column_order,
      verbose = verbose && !quiet,
      ...
    ),

    error = function(e) {

      cli::cli_abort(
        "Failed to generate snapshot for new file: {e$message}"
      )
    }
  )

  # -------------------------
  # JOIN SNAPSHOTS
  # -------------------------

  comparison <- dplyr::full_join(

    old_snap %>%
      dplyr::rename_with(
        ~ paste0(., "_old"),
        .cols = -sheet
      ),

    new_snap %>%
      dplyr::rename_with(
        ~ paste0(., "_new"),
        .cols = -sheet
      ),

    by = "sheet"
  )

  # -------------------------
  # CHANGE DETECTION
  # -------------------------

  comparison <- comparison %>%

    dplyr::mutate(

      changed =
        is.na(hash_old) != is.na(hash_new) |

        (
          !is.na(hash_old) &
            !is.na(hash_new) &
            hash_old != hash_new
        ),

      rows_diff =
        n_rows_new - n_rows_old,

      cols_diff =
        n_cols_new - n_cols_old,

      status = dplyr::case_when(

        is.na(hash_old) ~
          "New sheet",

        is.na(hash_new) ~
          "Deleted sheet",

        changed &
          (rows_diff != 0 | cols_diff != 0) ~
          "Structure changed",

        changed ~
          "Content changed",

        TRUE ~
          "Unchanged"
      ),

      change_type = dplyr::case_when(

        status == "Structure changed" ~
          "structure",

        status == "Content changed" ~
          "data",

        TRUE ~
          NA_character_
      )
    ) %>%

    dplyr::select(

      sheet,
      status,
      change_type,
      changed,

      rows_diff,
      cols_diff,

      n_rows_old,
      n_rows_new,

      n_cols_old,
      n_cols_new,

      hash_old,
      hash_new,

      dplyr::everything()
    ) %>%

    dplyr::arrange(
      status,
      sheet
    )

  # -------------------------
  # SUMMARY
  # -------------------------

  n_changes <- sum(
    comparison$status != "Unchanged"
  )

  if (verbose && !quiet) {

    if (n_changes == 0) {

      cli::cli_alert_success(
        "No changes detected."
      )

    } else {

      cli::cli_alert_warning(
        "{n_changes} change(s) detected."
      )
    }
  }

  # -------------------------
  # FILTER OUTPUT
  # -------------------------

  if (only_changes) {

    comparison <- dplyr::filter(
      comparison,
      status != "Unchanged"
    )
  }

  comparison
}
