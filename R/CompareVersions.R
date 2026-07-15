#' Compare Versions of Excel Files or Sheets
#'
#' Compares two Excel workbooks (or selected worksheets) using deterministic
#' sheet hashing. The function detects structural and content changes and
#' returns a comparison table summarising the differences.
#'
#' @param old_path Character scalar. Path to the original Excel workbook.
#' @param new_path Character scalar. Path to the updated Excel workbook.
#' @param old_sheets Optional character or numeric vector specifying which
#'   worksheets from the original workbook should be compared. `NULL`
#'   compares all worksheets.
#' @param new_sheets Optional character or numeric vector specifying which
#'   worksheets from the updated workbook should be compared. `NULL`
#'   compares all worksheets.
#' @param ignore_column_order Logical. If `TRUE`, column order is ignored
#'   when computing worksheet hashes.
#' @param only_changes Logical. If `TRUE`, only worksheets with detected
#'   changes are returned.
#' @param algo Character scalar. Hashing algorithm passed to
#'   [file_fingerprint()]. One of `"sha256"`, `"md5"` or `"sha1"`.
#' @param verbose Logical. Display progress information.
#' @param quiet Logical. Suppress CLI output.
#' @param ... Additional arguments passed to [snapshot_excel()].
#'
#' @return
#' A tibble containing one row per worksheet together with structural
#' metadata, hashes and change classification.
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


  # ---------------------------------------------------------------------------
  # VALIDATION
  # ---------------------------------------------------------------------------

  checkmate::assert_string(
    old_path
  )

  checkmate::assert_string(
    new_path
  )


  checkmate::assert_file_exists(
    old_path,
    extension = c(
      "xlsx",
      "xls"
    )
  )


  checkmate::assert_file_exists(
    new_path,
    extension = c(
      "xlsx",
      "xls"
    )
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
    c(
      "sha256",
      "md5",
      "sha1"
    )
  )



  # ---------------------------------------------------------------------------
  # HEADER
  # ---------------------------------------------------------------------------

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



  # ---------------------------------------------------------------------------
  # SNAPSHOTS
  # ---------------------------------------------------------------------------

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

      stop(
        paste0(
          "Failed to generate snapshot for old file: ",
          conditionMessage(e)
        ),
        call. = FALSE
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

      stop(
        paste0(
          "Failed to generate snapshot for new file: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )

    }

  )
  # ---------------------------------------------------------------------------
  # JOIN SNAPSHOTS
  # ---------------------------------------------------------------------------

  names(old_snap)[names(old_snap) != "sheet"] <-
    paste0(
      names(old_snap)[names(old_snap) != "sheet"],
      "_old"
    )

  names(new_snap)[names(new_snap) != "sheet"] <-
    paste0(
      names(new_snap)[names(new_snap) != "sheet"],
      "_new"
    )

  comparison <- dplyr::full_join(
    old_snap,
    new_snap,
    by = "sheet"
  )

  # ---------------------------------------------------------------------------
  # CHANGE DETECTION
  # ---------------------------------------------------------------------------

  comparison <- comparison |>

    dplyr::mutate(

      rows_diff =

        dplyr::coalesce(
          .data$n_rows_new,
          0L
        ) -

        dplyr::coalesce(
          .data$n_rows_old,
          0L
        ),



      cols_diff =

        dplyr::coalesce(
          .data$n_cols_new,
          0L
        ) -

        dplyr::coalesce(
          .data$n_cols_old,
          0L
        ),



      changed =

        dplyr::case_when(

          is.na(.data$hash_old) |
            is.na(.data$hash_new) ~

            TRUE,


          .data$hash_old != .data$hash_new ~

            TRUE,


          TRUE ~

            FALSE

        ),



      status =

        dplyr::case_when(

          is.na(.data$hash_old) ~

            "New sheet",


          is.na(.data$hash_new) ~

            "Deleted sheet",


          .data$changed &
            (
              .data$n_rows_old != .data$n_rows_new |
                .data$n_cols_old != .data$n_cols_new
            ) ~

            "Structure changed",


          .data$changed ~

            "Content changed",


          TRUE ~

            "Unchanged"

        ),



      change_type =

        dplyr::case_when(

          .data$status == "Structure changed" ~

            "structure",


          .data$status == "Content changed" ~

            "data",


          TRUE ~

            NA_character_

        )

    ) |>



    dplyr::select(

      dplyr::all_of(

        c(

          "sheet",

          "status",

          "change_type",

          "changed",

          "rows_diff",

          "cols_diff",

          "n_rows_old",

          "n_rows_new",

          "n_cols_old",

          "n_cols_new",

          "hash_old",

          "hash_new"

        )

      ),

      dplyr::everything()

    )|>



    dplyr::arrange(

      .data$status,

      .data$sheet

    )
  # ---------------------------------------------------------------------------
  # SUMMARY
  # ---------------------------------------------------------------------------

  n_changes <- sum(
    comparison[["status"]] != "Unchanged"
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



  # ---------------------------------------------------------------------------
  # FILTER
  # ---------------------------------------------------------------------------

  if (only_changes) {


    comparison <- comparison |>

      dplyr::filter(

        .data$status != "Unchanged"

      )

  }



  comparison

}
