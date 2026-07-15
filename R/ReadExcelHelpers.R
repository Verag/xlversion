# ============================================================================
# Helper Functions
# ============================================================================
#
# Internal utilities used by the Excel import workflow.
#
# These functions provide:
# - input validation;
# - dependency checks;
# - workbook discovery;
# - sheet selection;
# - controlled error handling.
#
# The public API is exposed through read_excel_allsheets().
#
# ============================================================================



#' Validate Excel Import Inputs
#'
#' Internal validation layer used before importing Excel workbooks.
#'
#' This function validates user inputs and prevents invalid execution states
#' before workbook processing begins.
#'
#' @param filename Character scalar. Path to the Excel workbook.
#' @param engine Character scalar. Excel backend.
#' @param sheet_names Optional sheet names or numeric positions.
#' @param guess_max Numeric scalar. Maximum rows used for type inference.
#' @param parallel Logical scalar. Reserved for future parallel processing.
#' @param tibble Logical scalar. Controls tibble output.
#' @param verbose Logical scalar. Controls diagnostic messages.
#' @param return_failed Logical scalar. Backward compatibility parameter.
#'
#' @return Invisibly returns validated parameters.
#'
#' @keywords internal
validate_read_excel_inputs <- function(
    filename,
    engine,
    sheet_names,
    guess_max,
    parallel,
    tibble,
    verbose,
    return_failed
) {


  # -------------------------------------------------------------------------
  # File validation
  # -------------------------------------------------------------------------

  if(!is.character(filename) ||
     length(filename) != 1){

    stop(
      "`filename` must be a single character string.",
      call.=FALSE
    )

  }


  if(!file.exists(filename)){

    stop(
      "File does not exist: ",
      filename,
      call.=FALSE
    )

  }


  extension <- tolower(
    tools::file_ext(filename)
  )


  if(!extension %in% c("xlsx","xls")){

    stop(
      "Unsupported Excel file extension.",
      call.=FALSE
    )

  }



  # -------------------------------------------------------------------------
  # Engine validation
  # -------------------------------------------------------------------------

  engine <- match.arg(
    engine,
    c(
      "readxl",
      "openxlsx"
    )
  )


  # -------------------------------------------------------------------------
  # Sheet selection validation
  # -------------------------------------------------------------------------
  if (!is.null(sheet_names)) {

    if (!is.character(sheet_names) &&
        !is.numeric(sheet_names)) {

      stop(
        "`sheet_names` must be character, numeric or NULL.",
        call. = FALSE
      )

    }

    if (length(sheet_names) == 0) {

      stop(
        "`sheet_names` cannot be empty.",
        call. = FALSE
      )

    }

    if (is.numeric(sheet_names)) {

      if (any(is.na(sheet_names))) {

        stop(
          "`sheet_names` cannot contain NA values.",
          call. = FALSE
        )

      }

      if (any(!is.finite(sheet_names))) {

        stop(
          "`sheet_names` must contain finite values.",
          call. = FALSE
        )

      }

      if (any(sheet_names != as.integer(sheet_names))) {

        stop(
          "`sheet_names` must contain integer sheet indices.",
          call. = FALSE
        )

      }

      if (any(sheet_names < 1)) {

        stop(
          "`sheet_names` must contain positive integer sheet indices.",
          call. = FALSE
        )

      }

    }

  }




  # -------------------------------------------------------------------------
  # Logical parameters validation
  # -------------------------------------------------------------------------
  check_flag <- function(x, name){

    if(
      !is.logical(x) ||
      length(x) != 1 ||
      is.na(x)
    ){

      stop(
        sprintf("`%s` must be TRUE or FALSE.", name),
        call. = FALSE
      )

    }

  }

  check_flag(parallel, "parallel")
  check_flag(tibble, "tibble")
  check_flag(verbose, "verbose")
  check_flag(return_failed, "return_failed")

  # -------------------------------------------------------------------------
  # Guessing configuration validation
  # -------------------------------------------------------------------------

  if(
    !is.numeric(guess_max) ||
    length(guess_max) != 1 ||
    is.na(guess_max) ||
    !is.finite(guess_max) ||
    guess_max < 1
  ){

    stop(
      "`guess_max` must be a single positive number.",
      call. = FALSE
    )

  }

  guess_max <- as.integer(guess_max)


  # -------------------------------------------------------------------------
  # Experimental / reserved arguments
  # -------------------------------------------------------------------------

  if (parallel) {

    warning(
      "`parallel` is reserved for a future release and is currently ignored.",
      call. = FALSE
    )

  }

  if (return_failed) {

    warning(
      "`return_failed` is deprecated and has no effect. Failed worksheets are always available in the returned `excel_book` object.",
      call. = FALSE
    )

  }

  invisible(
    list(
      filename=filename,
      engine=engine
    )
  )

}





#' Validate Excel Reader Dependencies
#'
#' Checks that the required Excel reading package is installed.
#'
#' @param engine Character scalar. Reader backend.
#'
#' @return Invisibly returns TRUE.
#'
#' @keywords internal
validate_dependencies <- function(engine){


  engine <- match.arg(
    engine,
    c(
      "readxl",
      "openxlsx"
    )
  )



  if(engine=="readxl" &&
     !requireNamespace(
       "readxl",
       quietly=TRUE
     )){

    stop(
      "Package 'readxl' is required.",
      call.=FALSE
    )

  }



  if(engine=="openxlsx" &&
     !requireNamespace(
       "openxlsx",
       quietly=TRUE
     )){

    stop(
      "Package 'openxlsx' is required.",
      call.=FALSE
    )

  }

  invisible(TRUE)

}







#' Discover Workbook Structure
#'
#' Extracts worksheet information from an Excel workbook.
#'
#' This function only inspects workbook structure and does not import data.
#'
#' @param filename Character scalar. Workbook path.
#' @param engine Character scalar. Excel backend.
#' @param verbose Logical scalar. Display progress information.
#'
#' @return An object containing workbook information.
#'
#' @keywords internal
discover_workbook <- function(
    filename,
    engine="readxl",
    verbose=FALSE
){


  engine <- match.arg(
    engine,
    c(
      "readxl",
      "openxlsx"
    )
  )



  if(verbose){

    message(
      "Discovering workbook sheets: ",
      basename(filename)
    )

  }



  sheets <- tryCatch(

    {

      if(engine=="readxl"){

        readxl::excel_sheets(
          filename
        )

      }else{

        openxlsx::getSheetNames(
          filename
        )

      }

    },

    error=function(e){

      stop(
        "Unable to read workbook structure: ",
        conditionMessage(e),
        call.=FALSE
      )

    }

  )



  structure(

    list(

      filename = filename,

      engine = engine,

      sheets = sheets,

      n_sheets = length(sheets),

      discovered = Sys.time()

    ),

    class="excel_workbook"

  )

}








#' Select Workbook Sheets
#'
#' Selects worksheets from an available workbook structure.
#'
#' @param all_sheets Character vector with available sheets.
#' @param sheet_names Optional requested sheets.
#'
#' @return Character vector with selected sheet names.
#'
#' @keywords internal
select_sheets <- function(
    all_sheets,
    sheet_names = NULL
){

  # -------------------------------------------------------------------------
  # All worksheets requested
  # -------------------------------------------------------------------------

  if (is.null(sheet_names)) {

    return(all_sheets)

  }


  # -------------------------------------------------------------------------
  # Convert numeric positions into sheet names
  # -------------------------------------------------------------------------

  if (is.numeric(sheet_names)) {

    if (any(sheet_names > length(all_sheets))) {

      stop(
        "Sheet index out of bounds.",
        call. = FALSE
      )

    }

    sheet_names <- all_sheets[as.integer(sheet_names)]

  }


  # -------------------------------------------------------------------------
  # Verify requested sheet names
  # -------------------------------------------------------------------------

  missing <- setdiff(
    sheet_names,
    all_sheets
  )


  if (length(missing) > 0) {

    stop(
      "Requested sheets not found: ",
      paste(
        missing,
        collapse = ", "
      ),
      call. = FALSE
    )

  }


  unname(sheet_names)

}



#' Read Selected Worksheets
#'
#' Imports selected worksheets and returns an `excel_book` object.
#'
#' Each worksheet is processed independently. A failure in one worksheet
#' does not interrupt successful imports from remaining worksheets.
#'
#' @param filename Character scalar. Workbook path.
#' @param sheets Character vector with worksheet names.
#' @param engine Character scalar. Excel backend.
#' @param guess_max Numeric scalar. Type inference rows.
#' @param tibble Logical scalar.
#' @param verbose Logical scalar.
#' @param ... Additional reader arguments.
#'
#' @return An object of class `excel_book`.
#'
#' @keywords internal
read_selected_sheets <- function(
    filename,
    sheets,
    engine="readxl",
    guess_max=10000,
    tibble=TRUE,
    verbose=FALSE,
    ...
){


  engine <- match.arg(
    engine,
    c(
      "readxl",
      "openxlsx"
    )
  )



  reader <- function(sheet){


    tryCatch(

      {


        if(verbose){

          message(
            "Reading sheet: ",
            sheet
          )

        }



        data <- switch(

          engine,

          readxl =
            readxl::read_excel(
              filename,
              sheet=sheet,
              guess_max=guess_max,
              ...),


          openxlsx =
            openxlsx::read.xlsx(
              filename,
              sheet=sheet,
              ...)

        )



        if(!tibble){

          data <-
            as.data.frame(
              data,
              stringsAsFactors=FALSE
            )

        }



        new_excel_sheet(
          data=data,
          sheet=sheet,
          source=filename
        )



      },

      error=function(e){


        structure(

          list(

            sheet=sheet,

            source=filename,

            message=
              conditionMessage(e),

            timestamp=
              Sys.time(),

            traceback = NULL

          ),

          class="excel_sheet_error"

        )

      }

    )

  }



  results <- lapply(
    sheets,
    reader
  )



  names(results)<-sheets



  success <- results[vapply(results,
                            inherits,
                            logical(1),
                            "excel_sheet")]

  failed <- results[vapply(results,
                           inherits,
                           logical(1),
                           "excel_sheet_error")]

  new_excel_book(
    sheets=success,
    failed_sheets=failed,
    file = new_excel_file(filename)
  )

}
